;; state.clj
;; AndrewJ 2020-05-02

(ns ttr.state
  (:gen-class)
  (:require [ttr.graph :refer :all]
            [csv-map.core :as csv]
            [clojure.spec.alpha :as s]))

;;-------------------------------
;; Utilities

(defn hash-sum
  "Sum the values of the numeric hash."
  [h]
  (apply + (vals h)))

(defn hash-enumerate
  "For each pair [k v] in a numeric hash, add v copies of k, and concatenate into a single list."
  [h]
  (reduce-kv
   (fn [m k v]
     (into m (repeat v k))) [] h))

;; Ticket :: Map k v
(s/def ::ticket (s/keys :req [string? string? int?]))
(s/def ::tickets (s/coll-of ::ticket))

;; read-tickets :: String -> List Ticket
(defn read-tickets
  "Read tickets from a CSV file, and convert points to an int."
  ([]
   (read-tickets "data/ttr-europe-tickets.csv"))

  ([tickets-file]
   {:post (s/valid? ::tickets %)}
   (as-> tickets-file <>
     (slurp <>)
     (csv/parse-csv <> :key :keyword)
     (map (fn [e] (update e :points #(Integer. %))) <>))))

(s/def ::colour #{:white :red :orange :blue :green :black :pink :yellow :loco})
(def colours [:white :red :orange :blue :green :black :pink :yellow :loco])

(s/def ::cards (s/map-of ::colour int?))
(def zero-train-cards (zipmap colours (repeat 9 0)))
(def all-train-cards (zipmap colours [12 12 12 12 12 12 12 12 14]))

;; State :: Map k v
;; init-state :: State
(s/def ::graph (s/keys))
(s/def ::cars int?)
(s/def ::score int?)
(s/def ::player (s/keys :req-un [::cars ::cards ::tickets ::score]))
(s/def ::state (s/keys :req-un [::graph ::cards ::cards ::tickets (s/coll-of ::player)]))

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
   :deck all-train-cards
   :table zero-train-cards
   :tickets (read-tickets)
   :player (vec (repeat nplayers {:cars 45
                                  :cards zero-train-cards
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