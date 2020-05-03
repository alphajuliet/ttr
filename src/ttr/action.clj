;; action.clj
;; AndrewJ 2020-05-03

(ns ttr.action
  (:gen-class)
  (:require [ttr.state :refer :all]
            [ttr.graph :refer :all]
            [csv-map.core :as csv]
            [random-seed.core :as r]))

;;-------------------------------
;; Utilities

(defn- dec-to-0
  "Decrement down to a floor of zero"
  [n]
  (max 0 (dec n)))

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

(defn random-car
  "Select a random card from the deck."
  [state]
  (->> (:deck state)
       (hash-enumerate)
       (r/rand-nth)))

;;-------------------------------

;; deal-car :: State -> State
(defn- deal-car
  "Deal a single car card from the deck to the table."
  [st]
  (let [c (random-car st)]
    (-> st
        (update-in [:deck c] dec-to-0)
        (update-in [:table c] inc))))

(defn- deal-to-5
  "Deal up to five cards to the table."
  [st _]
  (if (< (hash-sum (:table st)) 5)
    (deal-car st)
    ;else
    (reduced st)))

;; deal-table :: State -> State
(defn deal-table
  "Deal random train car cards from the deck to the table until there are five. If there are more than three locos, discard all the table cards, and re-draw new cards."
  [state]
  (loop []
    (let [s (reduce deal-to-5 state (range 5))]
      (if (>= (get-in s [:table :loco]) 3)
        (recur)
        s))))

;; The End