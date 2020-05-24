;; state.clj
;; AndrewJ 2020-05-02

(ns ttr.state
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [csv-map.core :as csv]
            [ttr.graph :as gr]
            [ttr.num-map :as num]))

;;-------------------------------
;; Utilities

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

(s/def ::route-colour #{:white :red :orange :blue :green :black :pink :yellow :none})

(s/def ::cards (s/map-of ::colour int?))
(def zero-train-cards (zipmap colours (repeat 9 0)))
(def all-train-cards (zipmap colours [12 12 12 12 12 12 12 12 14]))

;; State :: Map k v
;; init-state :: State
(s/def ::cars int?)
(s/def ::score int?)
(s/def ::plyr (s/keys :req-un [::cars ::cards ::tickets ::score]))

(s/def ::map (s/keys))
(s/def ::deck ::cards)
(s/def ::player (s/coll-of ::plyr))
(s/def ::state (s/keys :req-un [::map ::deck ::cards ::tickets ::player]))

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
  {:pre (s/valid? ::state s)}
  (println "Deck:" (reduce + 0 (vals (:deck s))))
  (println "Cards:" (:cards s))
  (println "Tickets:" (count (:tickets s)))
  (println "Players:")
  (map #(println %) (:player s)))

;; The End