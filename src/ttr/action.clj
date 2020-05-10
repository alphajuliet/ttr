;; action.clj
;; AndrewJ 2020-05-03

(ns ttr.action
  (:gen-class)
  (:require [ttr.state :as st]
            [ttr.graph :as gr]
            [random-seed.core :as r]
            [clojure.spec.alpha :as s]))

;;-------------------------------
;; Utilities

(defn- dec-to-0
  "Decrement down to a floor of zero"
  [n]
  (max 0 (dec n)))

(defn random-card
  "Select a random card from a pile."
  [pile]
  (->> pile
       (st/hash-enumerate)
       (r/rand-nth)))

;;-------------------------------
;; deal-car :: State -> State
(defn- deal-car
  "Deal a single car card from the deck to the table."
  [st]
  (let [c (random-card (:deck st))]
    (-> st
        (update-in [:deck c] dec-to-0)
        (update-in [:cards c] inc))))

(defn- deal-to-5
  "Deal up to five cards to the table."
  [st _]
  (if (< (st/hash-sum (:cards st)) 5)
    (deal-car st)
    ;else
    (reduced st)))

;; deal-table :: State -> State
(defn deal-table
  "Deal random train car cards from the deck to the table until there are five. If there are more than three locos, discard all the table cards, and re-draw new cards."
  [state]
  (loop []
    (let [s (reduce deal-to-5 state (range 5))]
      (if (>= (get-in s [:cards :loco]) 3)
        (recur)
        s))))

;;-------------------------------
(defn take-card
  "A player takes a card from the table into their hand, and deal another card to the table. There is no limit to the number of hand cards."
  [player card state]
  ; Assert that the card is available
  {:pre (>= (get-in state [:cards card]) 1)}

  (-> state
      (update-in [:cards card] dec)
      (update-in [:player player :cards card] inc)
      (deal-table)))

;;-------------------------------
(defn get-available-routes
  "Returns all routes in _each direction_ as a sequence of vectors."
  [state]
  (gr/get-routes (:map state) {:claimed-by nil}))

(defn- route->query
  "Convert an edge to a query for find-edge"
  [e]
  {:src (first e) :dest (second e) :colour (:colour (last e))})

;;-------------------------------
(defn claim-route
  "Claim a route on the map. A route is specified as a map with a :src, :dest and :colour"
  [route player state]
  (let [g (:map state)]
    (assoc state :map (gr/update-route g route :claimed-by player))))


;; The End)