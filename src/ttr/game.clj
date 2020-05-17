;; game.clj
;; AndrewJ 2020-05-10

(ns ttr.game
  (:gen-class)
  (:require [ttr.state :as st]
            [ttr.graph :as gr]
            [ttr.action :as act]))

;;-------------------------------
;; Utilities

;;-------------------------------
(defn init-game
  "Set up the initial game state"
  [nplayers]
  (->> (st/empty-state nplayers)
       (act/deal-table)))

;; The End