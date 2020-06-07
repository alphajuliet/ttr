(ns ttr.game-test
  (:require [clojure.test :refer [deftest is testing]]
            #_[ttr.graph :as gr]
            [ttr.num-map :as num]
            #_[ttr.state :as st]
            #_[ttr.action :as act]
            [ttr.game :as game]))

(deftest test-take-card-options
  (testing "All actions for taking cards."
    (let [s0 (game/init-game 2)]
     (is (= 6 (count (game/take-card-options 0 s0)))))))

(deftest test-claim-route-options
  (testing "All claimable routes."
    (let [s0 (game/init-game 2)
          s1 (update-in s0 [:player 0 :cards]
                        (partial num/map-add {:red 1 :blue 3}))]
      (is (= 46 (count (game/claim-route-options 0 s1)))))))

;; The End