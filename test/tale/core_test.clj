(ns tale.core-test
  (:require
    [clojure.test :refer [testing deftest is]]
    [tale.core :refer [wrap-broadcasts on call-first]]))


(deftest wrap-broadcast
  (testing "returns the broadcasted events in a map"
    (let [result (wrap-broadcasts {}
                                  (fn [broadcast]
                                    (broadcast :user-created {:name "Jon"})
                                    (broadcast :email-sent {:user {:id 1}})))]
      (is (= result {:broadcasts
                     {:user-created {:args '({:name "Jon"})}, :email-sent {:args '({:user {:id 1}})}}
                     :subscriptions []})))))


(deftest test-on
  (testing "adds a subscriber to the map"
    (let [broadcasts (wrap-broadcasts {}
                                      (fn [broadcast]
                                        (broadcast :user-created {:name "Jon"})
                                        (broadcast :email-sent {:id 1})))
          handler (fn [user] user)
          subs (-> broadcasts (on :user-created handler) (on :user-deleted handler) (:subscriptions))]
      (is (= subs
             [{:event :user-created, :handler handler, :matched true}
              {:event :user-deleted, :handler handler, :matched false}])))))

(deftest test-call-first
  (testing "calls the first broadcast"
    (let [broadcasts (wrap-broadcasts {}
                                      (fn [broadcast]
                                        (broadcast :user-created {:name "Jon"})
                                        (broadcast :email-sent {:id 1})))
          handler (fn [arg] arg)
          result (-> broadcasts (on :user-created handler) (on :email-sent handler) (call-first))]
      (is (= result {:name "Jon"})))))
