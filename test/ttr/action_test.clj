(ns ttr.action-test
  (:require [clojure.test :refer [deftest is testing]]
            [ttr.graph :as gr]
            [ttr.state :as st]
            [ttr.action :as act]))

(deftest sanity
  (testing "Sanity check"
    (is (= 5 (+ 2 3)))))

(deftest test-deal
  (testing "Deal to the table."
    (let [s0 (st/empty-state 2)
          s1 (act/deal-table s0)]
      (is (= 0 (st/hash-sum (:cards s0))))
      (is (= 5 (st/hash-sum (:cards s1)))))))

(deftest test-take-card
  (testing "Take a card into a hand."
    (let [s0 (st/empty-state 2)
          s1 (act/deal-table s0)
          c  (act/random-card (:cards s1))
          s2 (act/take-card 0 c s1)]
      (is (= 1 (get-in s2 [:player 0 :cards c])))
      (is (= 5 (st/hash-sum (:cards s2)))))))

(deftest test-claim
  (testing "Claim a route on the map for player 1"
    (let [s0 (st/empty-state 2)
          r  (first (act/available-routes s0))
          s1 (act/claim-route r 1 s0)]
      (is (= 200 (count (act/available-routes s1)))))))

(deftest test-pay-route
  (testing "Pay for a route"
    (let [s0 (st/empty-state 2)
          s1 (assoc-in s0 [:player 0 :cards] 
                       {:orange 1 :white 2 :yellow 3 :green 4 :locos 1})
          r-white (first (gr/get-routes (:map s0) {:src "Paris" :dest "Frankfurt" :colour :white}))
          s2 (act/claim-route r-white 0 s1)]
      (is (= 0 (get-in s2 [:player 0 :cards :white]))))))

;; The End