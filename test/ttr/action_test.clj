(ns ttr.action-test
  (:require [clojure.test :refer :all]
            [ttr.state :refer :all]
            [ttr.action :refer :all]))

(deftest sanity
  (testing "Sanity check"
    (is (= 5 (+ 2 3)))))

(deftest test-deal
  (testing "Deal to the table."
    (let [s0 (empty-state 2)
          s1 (deal-table s0)]
      (is (= 0 (hash-sum (:table s0))))
      (is (= 5 (hash-sum (:table s1)))))))

(deftest test-take-card
  (testing "Take a card into a hand."
    (let [s0 (empty-state 2)
          s1 (deal-table s0)
          c  (random-card (:table s1))
          s2 (take-card 0 c s1)]
      (is (= 1 (get-in s2 [:player 0 :cards c])))
      (is (= 5 (hash-sum (:table s2)))))))

;; The End