(ns tale.core)


(defn wrap-broadcasts
  [global-subs func]
  (let [broadcasted (atom {:broadcasts {} :subscriptions []})
        broadcast-fn (fn [event & args]
                       (let [global-subs (->
                                           global-subs
                                           (get event)
                                           (into []))]
                         (doseq [sub global-subs] (apply sub args))
                         (swap! broadcasted update-in [:broadcasts] conj {event {:args args}})))]
    (func broadcast-fn)
    (deref broadcasted)))


(defn on
  [broadcasts event func]
  (let [match (find (:broadcasts broadcasts) event)]
    (update-in broadcasts [:subscriptions] conj {:event event :handler func :matched (some? match)})))


(defn call-first
  [broadcasts]
  (let [sub (->> broadcasts
                 (:subscriptions)
                 (filter #(:matched %))
                 (first))
        args (-> broadcasts
                 (:broadcasts)
                 (find (:event sub))
                 (last)
                 (:args))]
    (when (and sub args)
      (apply (:handler sub) args))))


(def global-listeners
  {:user-created [(fn [user] (println user "user-created"))
                  (fn [user] (println user "user-created-2"))]})


(def result
  (wrap-broadcasts global-listeners
                   (fn [broadcast]
                     (broadcast :user-created {:id 1 :name "Gogo"})
                     (when false
                       (broadcast :user-updated {:id 2 :name "Bob"}))
                     (broadcast :user-deleted {:id 3 :name "Jon"}))))


(-> result
    (on :user-created
        (fn [user] (:id user)))
    (on :user-updated
        (fn [user] (:id user)))
    (on :user-deleted
        (fn [_user] :user-deleted))
    (call-first))


(-> result
    (on :user-updated
        (fn [_user] :user-deleted))
    (call-first))
