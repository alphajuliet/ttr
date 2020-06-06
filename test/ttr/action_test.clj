(ns ttr.action-test
  (:require [clojure.test :refer [deftest is testing]]
            [ttr.graph :as gr]
            [ttr.num-map :as num]
            [ttr.state :as st]
            [ttr.action :as act]))

(deftest sanity
  (testing "Sanity check"
    (is (= 5 (+ 2 3)))))

(deftest test-deal
  (testing "Deal to the table."
    (let [s0 (st/empty-state 2)
          s1 (act/deal-table s0)]
      (is (= 0 (num/map-sum (:cards s0))))
      (is (= 5 (num/map-sum (:cards s1)))))))

(deftest test-take-card
  (testing "Take a card into a hand."
    (let [s0 (st/empty-state 2)
          s1 (act/deal-table s0)
          c  (act/random-card (:cards s1))
          s2 (act/take-card 0 c s1)]
      (is (= 1 (get-in s2 [:player 0 :cards c])))
      (is (= 5 (num/map-sum (:cards s2)))))))

(deftest test-take-random-card
  (testing "Take a random card from the deck."
    (let [s0 (st/empty-state 2)
          s1 (act/take-random-card 0 s0)]
      (is (= 1 (num/map-sum (get-in s1 [:player 0 :cards])))))))

(deftest test-pay-cards
  (testing "Pay with cards."
    (let [cards {:red 3 :white 2 :loco 2}]
      (is (= {:red 1 :white 2 :loco 2} (act/pay-cards :red 2 0 cards)))
      (is (= {:red 0 :white 2 :loco 2} (act/pay-cards :red 3 0 cards)))
      (is (= {:red 0 :white 2 :loco 1} (act/pay-cards :red 4 0 cards)))
      (is (nil? (act/pay-cards :red 6 0 cards)))
      (is (= {:red 1 :white 2 :loco 1} (act/pay-cards :red 3 1 cards)))
      (is (= {:red 0 :white 2 :loco 1} (act/pay-cards :red 4 1 cards)))
      (is (= {:red 0 :white 2 :loco 0} (act/pay-cards :red 5 0 cards))))))

(deftest test-claim
  (testing "Claim a route on the map for player 1"
    (let [s0 (st/empty-state 2)
          s1 (update-in s0 [:player 0 :cards]
                       (partial num/map-add {:white 2 :blue 3 :loco 1}))
          r  (first (act/available-routes s0))
          r-uncoloured (first (gr/get-routes (:map s1) {:colour :none}))
          s2 (act/claim-route r :none 0 s1)
          s3 (act/claim-route r-uncoloured :blue 0 s1)]
      (is (nil? (act/claim-route r :none 0 s0)))
      (is (= 202 (count (act/available-routes s0))))
      (is (= 200 (count (act/available-routes s2))))
      
      (is (nil? (act/claim-route r-uncoloured :yellow 0 s1)))
      (is (= 200 (count (act/available-routes s3)))))))

;; The End