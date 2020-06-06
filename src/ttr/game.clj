;; game.clj
;; AndrewJ 2020-05-10

(ns ttr.game
  (:gen-class)
  (:require [ttr.state :as st]
            [ttr.graph :as gr]
            [ttr.action :as act]
            [ttr.num-map :as num]))

;;-------------------------------
;; Utilities

;;-------------------------------
;; init-game :: Int -> State
(defn init-game
  "Set up the initial game state"
  [nplayers]
  (->> (st/empty-state nplayers)
       (act/deal-table)))

;; take-card-options :: Player -> State -> [Action]
(defn take-card-options
  "Enumerate all the actions for a player to take cards."
  [player state]
  (concat
   (list `(act/take-random-card ~player))
   (for [c (num/map-enumerate (get state :cards))]
     `(act/take-card ~player ~c))))

;; claim-route-options :: Player -> State -> [Action]
;; available-actions :: Player -> State -> [Action]

;; apply-action :: Action -> State -> State
(defn apply-action
  "Apply an action to a state"
  [action state]
  (eval (concat action (list state))))

(def s0 (init-game 2))

;; The End