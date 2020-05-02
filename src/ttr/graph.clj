;; graph.clj
;; AndrewJ 2020-05-02

(ns ttr.graph
  (:gen-class)
  (:require [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

;;-------------------------------

;; create-route :: Map k v -> Map k v
(defn- create-route
  "Transform a route into a graph edge."
  [route]
  [(:name1 route)
   (:name2 route)
   {:length (Integer. (:length route))
    :colour (keyword (:colour route))
    :locos (Integer. (:locos route))
    :tunnel (if (= "TRUE" (:istunnel route)) 1 0)
    :taken 0}])

;; create-graph :: String -> Graph
(defn create-graph
  "Create a TTR graph from a map of edges."
  [map-data]
  (let [g (uber/multigraph)]
    (uber/add-edges* g (map create-route map-data))))

;; best-path :: Graph -> String -> String -> Path
(defn best-path
  "Find the shortest path using a weighted edge score."
  [g start end]
  (alg/shortest-path g {:start-node start
                        :end-node end
                        :cost-fn (fn [e] (+ (* 1 (uber/attr g e :length))
                                            (* 3 (uber/attr g e :locos))
                                            (* 9 (uber/attr g e :tunnel))
                                            (* 100 (uber/attr g e :taken))))}))

;; The End