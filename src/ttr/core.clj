(ns ttr.core
  (:gen-class)
  (:require [csv-map.core :as csv]
            [ubergraph.core :as uber]))

(defn read-map
  "Read the map from a CSV file"
  [map-file]
  (csv/parse-csv (slurp map-file) :key :keyword))

(defn- create-route
  "Transform a route into a graph edge."
  [route]
  [(:name1 route) 
   (:name2 route)
   {:length (:length route)
    :colour (:colour route)}])

(defn create-graph
  "Create a TTR graph from a map of the map."
  [map-data]
  (let [g (uber/graph)]
    (uber/add-edges* g (map create-route map-data))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
