;; graph.clj
;; AndrewJ 2020-05-02

(ns ttr.graph
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [spec-dict :refer [dict]]
            [csv-map.core :as csv]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]))

;;-------------------------------
;; Utilities

(defn boolean->Integer
  [b]
  {:pre [(boolean? b)]}
  (if b 1 0))

;;-------------------------------
;; Definitions

(s/def ::edge-attrs (s/keys :req-un [::length ::colour ::locos ::tunnel ::claimed-by]))
(s/def ::route (s/cat :src string?
                      :dest string?
                      :attrs ::edge-attrs))
(s/def ::routes (s/coll-of ::route))
(s/def ::graph (s/keys :req-un [::node-map ::allow-parallel? ::undirected?
                                ::attrs ::cached-hash]))

;; create-edge :: Map k v -> Route
(defn- create-edge
  "Transform a route into a graph edge."
  [route]
  [(:name1 route)
   (:name2 route)
   {:length (Integer. (:length route))
    :colour (keyword (:colour route))
    :locos (Integer. (:locos route))
    :tunnel (if (= "TRUE" (:istunnel route)) true false)
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
  "Get all routes in the graph"
  [g]
  {:pre [(s/valid? ::graph g)]}
  (map (partial uber/edge-with-attrs g)
       (uber/edges g)))

(defn get-routes
  "Get all routes that match the query"
  [g condition]
  (map (partial uber/edge-with-attrs g)
       (uber/find-edges g condition)))

(defn- route->query
  "Convert an edge to a query for uber/find-edge"
  [e]
  {:src (first e) 
   :dest (second e) 
   :colour (:colour (last e))})

(defn update-route
  "Overwrite the value of an route attribute"
  [g route attr value]
  {:pre [(s/valid? ::route route)]}
  (let [edge (uber/find-edge g (route->query route))]
       (uber/set-attrs g edge {attr value})))

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
