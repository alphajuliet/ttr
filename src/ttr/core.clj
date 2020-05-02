;; core.clj
;; AndrewJ 2020-05-02

(ns ttr.core
  (:gen-class)
  (:require [ttr.graph]))

; Set up graph
(def g
  (->> "data/ttr-europe.csv"
       (read-map)
       (create-graph)))

(defn -main
  "Nothing, yet."
  [& args]
  (println "Hello, World!"))

;;  The End