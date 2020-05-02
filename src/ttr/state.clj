;; state.clj
;; AndrewJ 2020-05-02

(ns ttr.state
  (:gen-class)
  (:require [ttr.graph :refer :all]
            [csv-map.core :as csv]))

;;-------------------------------

;; Route :: Map k v
;; read-map :: String -> List Route
(defn read-map
  "Read the map from a CSV file"
  [map-file]
  (csv/parse-csv (slurp map-file) :key :keyword))

;; Map graph
;; g0 :: Graph
(def g0
  (->> "data/ttr-europe-map.csv"
       (read-map)
       (create-graph)))

;; Ticket :: Map k v
;; read-tickets :: String -> List Ticket
(defn read-tickets
  "Read tickets from a CSV file."
  ([]
   (read-tickets "data/ttr-europe-tickets.csv"))
  ([tickets-file]
   (csv/parse-csv (slurp tickets-file) :key :keyword)))

;; Colour :: Union (List Keyword)
;; all-train-cards :: Map Colour Integer
(def all-train-cards
  {:white 12
   :red 12
   :orange 12
   :blue 12
   :green 12
   :black 12
   :pink 12
   :yellow 12
   :loco 14})

;; State :: Map k v
;; init-state :: State
(defn init-state
  "Define a state:
   - Map
   - Deck
   - Player `n`
     - Train cars
     - Train cards
     - Tickets
     - Score"
  [nplayers]
  {:pre [(>= nplayers 2)]}
  
  {:map g0
   :deck all-train-cards
   :tickets (read-tickets)
   :player (vec (repeat nplayers  {:cars []
                                   :cards []
                                   :tickets []
                                   :score 0}))})


;; The End