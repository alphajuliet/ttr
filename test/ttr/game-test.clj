(ns ttr.game-test
  (:require [clojure.test :refer [deftest is testing]]
            [ttr.graph :as gr]
            [ttr.num-map :as num]
            [ttr.state :as st]
            [ttr.action :as act]
            [ttr.game :as game]))

(deftest test-take-card-options
  (let [s0 (game/init-game 2)]
    (is (= 6 (count (game/take-card-options 0 s0))))))


;; The End