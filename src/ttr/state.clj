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
;; initial-map :: Graph
(defn initial-map
  []
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

;; Colours :: Union (List Keyword)
(def colours [:white :red :orange :blue :green :black :pink :yellow :loco])

;; all-train-cards :: Map Colours Integer
(defn zero-train-cards [] (zipmap colours (repeat 9 0)))
(defn all-train-cards [] (zipmap colours [12 12 12 12 12 12 12 12 14]))

;; State :: Map k v
;; init-state :: State
(defn empty-state
  "Define the empty state:
   - Map
   - Deck
   - Table
   - Player `n`
     - Train cars
     - Train cards
     - Tickets
     - Score"
  [nplayers]
  {:pre [(>= nplayers 2)]}
  
  {:map (initial-map)
   :deck (all-train-cards)
   :table (zero-train-cards)
   :tickets (read-tickets)
   :player (vec (repeat nplayers  {:cars 45
                                   :cards (zero-train-cards)
                                   :tickets []
                                   :score 0}))})

(defn pprint-state
  "Pretty print the state."
  [s]
  (println "Deck:" (reduce + 0 (vals (:deck s))))
  (println "Table:" (:table s))
  (println "Tickets:" (count (:tickets s)))
  (println "Players:") 
  (map #(println %) (:player s)))

;; The End