;; state.clj
;; AndrewJ 2020-05-02

(ns ttr.state
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [csv-map.core :as csv]
            [spec-dict :refer [dict]]
            [ttr.graph :as gr]))

;;-------------------------------
;; Specs

(s/def ::ticket (dict {:city1 string? :city2 string? :points pos-int?}))
(s/def ::tickets (s/coll-of ::ticket))
(s/def ::colour #{:white :red :orange :blue :green :black :pink :yellow :loco})
(s/def ::cards (s/map-of ::colour nat-int?))

(s/def ::route-colour #{:white :red :orange :blue :green :black :pink :yellow :none})

(s/def ::route-attrs (s/and (dict {:length pos-int?
                                   :colour ::route-colour
                                   :locos nat-int?
                                   :tunnel boolean?}) #(< (:locos %) (:length %))))

(s/def ::route (s/cat :src string?
                      :dest string?
                      :attrs ::route-attrs))


(s/def ::player (dict {:cars nat-int?
                       :cards ::cards
                       :tickets ::tickets
                       :score nat-int?}))

(s/def ::state (dict {:map (s/keys)
                      :deck ::cards
                      :cards ::cards
                      :tickets ::tickets
                      :player (s/coll-of ::player)}))

;;-------------------------------
;; Utilities

;; read-tickets :: String -> List Ticket
(defn read-tickets
  "Read tickets from a CSV file, and convert points to an int."
  ([]
   (read-tickets "data/ttr-europe-tickets.csv"))

  ([tickets-file]
   {:post [(s/valid? ::tickets %)]}

   (as-> tickets-file <>
     (slurp <>)
     (csv/parse-csv <> :key :keyword)
     (map (fn [e] (update e :points #(Integer. %))) <>))))

;;-------------------------------
;; Definitions

(def colours [:white :red :orange :blue :green :black :pink :yellow :loco])
(def zero-train-cards (zipmap colours (repeat 9 0)))
(def all-train-cards (zipmap colours [12 12 12 12 12 12 12 12 14]))

;;-------------------------------
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
  {:pre [(<= 2 nplayers 5)]}

  {:map (gr/initial-map)
   :deck all-train-cards
   :cards zero-train-cards
   :tickets (read-tickets)
   :player (vec (repeat nplayers {:cars 45
                                  :cards zero-train-cards
                                  :tickets []
                                  :score 0}))})

(defn pprint-state
  "Pretty print the state."
  [s]
  {:pre [(s/valid? ::state s)]}
  (println "Deck:" (reduce + 0 (vals (:deck s))))
  (println "Table:" (:cards s))
  (println "Tickets:" (count (:tickets s)))
  (println "Players:")
  (map #(println %) (:player s)))

;; The End
