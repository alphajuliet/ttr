;; graph.clj
;; AndrewJ 2020-05-02

(ns ttr.graph
  (:gen-class)
  (:require [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [csv-map.core :as csv]
            [clojure.spec.alpha :as s]))

;;-------------------------------
;; Utilities
(defn boolean->Integer
  [b]
  {:pre (boolean? b)}
  (if b 1 0))

;;-------------------------------
;; Definitions
(s/def ::edge-attrs (s/keys :req-un [::length ::colour ::locos ::tunnel ::claimed-by]) )
(s/def ::route (s/cat :src string? :dest string? :attrs ::edge-attrs))
(s/def ::graph (s/keys :req-un [::node-map ::allow-parallel? ::undirected? ::attrs ::cached-hash]))

;; create-edge :: Map k v -> Route
(defn- create-edge
  "Transform a route into a graph edge."
  [route]
  [(:name1 route)
   (:name2 route)
   {:length (Integer. (:length route))
    :colour (keyword (:colour route))
    :locos (Integer. (:locos route))
    :tunnel (if (= "TRUE" (:istunnel route)) 1 0)
    :claimed-by nil}])

;; create-graph :: String -> Graph
(defn- create-graph
  "Create a TTR graph from a map of edges."
  [map-data]
  (let [g (uber/multigraph)]
    (uber/add-edges* g (map create-edge map-data))))

;;-------------------------------
;; Route :: Map k v
;; read-map :: String -> List Route
(defn- read-map
  "Read the map from a CSV file"
  [map-file]
  (csv/parse-csv (slurp map-file) :key :keyword))

;; initial-map :: Graph
(defn initial-map
  []
  (->> "data/ttr-europe-map.csv"
       (read-map)
       (create-graph)))

;;-------------------------------
(defn get-all-routes
  [g]
  {:pre (s/valid? ::graph g)}
  (map (partial uber/edge-with-attrs g)
       (uber/edges g)))

(defn get-routes
  [g condition]
  (map (partial uber/edge-with-attrs g)
       (uber/find-edges g condition)))

; claimed? :: Graph -> Edge -> Boolean
(defn claimed?
  [g e]
  (as-> e <>
    (uber/find-edge g <>)
    (uber/attr g <> :claimed-by)))

; take-edge :: Graph -> Edge -> Player -> Graph
(defn claim-edge
  "Indicate an edge is taken by a player. This overwrites the existing value."
  [g route player]
  (let [e (uber/find-edge g route)]
    (uber/add-attr g e :claimed-by player)))

;;-------------------------------
;; best-path :: Graph -> String -> String -> Path
(defn best-path
  "Find the shortest path using a weighted edge score."
  [g start end]
  (alg/shortest-path g {:start-node start
                        :end-node end
                        :cost-fn (fn [e]
                                   (+ (* 1 (uber/attr g e :length))
                                      (* 3 (uber/attr g e :locos))
                                      (* 9 (uber/attr g e :tunnel))
                                      (* 100 (boolean->Integer (nil? (uber/attr g e :taken))))))}))

;; The End