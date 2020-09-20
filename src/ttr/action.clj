;; action.clj
;; AndrewJ 2020-05-03

(ns ttr.action
  (:gen-class)
  (:require [ttr.state :as st]
            [ttr.graph :as gr]
            [ttr.num-map :as num]
            [random-seed.core :as r]
            [cats.builtin]
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
       (num/map-enumerate)
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
  (if (< (num/map-sum (:cards st)) 5)
    (deal-car st)
    ;else
    (reduced st)))

;; deal-table :: State -> State
(defn deal-table
  "Deal random train car cards from the deck to the table until there
  are five. If there are more than three locos, discard all the table
  cards, and re-draw new cards."
  [state]
  (loop []
    (let [s (reduce deal-to-5 state (range 5))]
      (if (>= (get-in s [:cards :loco]) 3)
        (recur)
        s))))

;;-------------------------------
;; available-routes :: State -> Seq Route
(defn available-routes
  "Returns all routes in _each direction_"
  [state]
  {:pre [(s/valid? ::st/state state)]}
  (gr/get-routes (:map state) {:claimed-by nil}))

#_(defn- route->query
    "Convert an edge to a query for find-edge"
    [e]
    {:src (first e) :dest (second e) :colour (:colour (last e))})

;;-------------------------------
;; pay-cards :: Colour -> Int -> Int -> Seq Card -> Seq Card
(defn pay-cards
  "For a given `colour`, `length`, and number of `locos`, work out what
  player `cards` to pay with."
  [colour length locos cards]
  {:pre [(s/valid? ::st/colour colour)
         (s/valid? pos-int? length)
         (s/valid? int? locos)
         (s/valid? ::st/cards cards)]}
  (let [n (- length locos)
        a (min (:loco cards) locos) ; number of mandatory locos to play
        b (min (colour cards) n) ; number of coloured cars to play
        c (min (- (:loco cards) a) (- n b))] ; number of additional locos to play
    (if (or (< a locos)
            (< c (- length a b)))
      nil
      ;else
      (-> cards
          (num/map-sub {:loco a})
          (num/map-sub {colour b})
          (num/map-sub {:loco c})))))

;; pay-for-route :: Route -> Colour -> Player -> State -> State
(defn pay-for-route
  "Pay for the route in cards and locos. If payment is not possible then return
  nil. If a route has a :colour of :none, then pay with `chosen-colour`."
  ;;@@TODO Implement tunnels
  [route chosen-colour player state]
  #_{:pre [(s/valid? ::st/state state)]}
  (let [{:keys [length colour locos]} (last route)
        curr-hand (get-in state [:player player :cards])
        new-hand (if (= colour :none)
                   (pay-cards chosen-colour length locos curr-hand)
                   (pay-cards colour length locos curr-hand))]
    (if (nil? new-hand)
      ;state
      nil
      ;else
      (assoc-in state [:player player :cards] new-hand))))

;;-------------------------------
;; These are the high-level actions for a player

;; claim-route :: Route -> Colour -> Player -> State -> State
(defn claim-route
  "Claim and pay for a route on the map. A route is specified as a map
  with keys `:src`, `:dest` and `:colour`. If the route has :colour of
  :none, then pay with `chosen-colour`."
  [route chosen-colour player state]
  {:pre [(s/valid? ::st/route route)
         (s/valid? int? player)
         (s/valid? ::st/state state)]}
  (let [g (:map state)
        s (pay-for-route route chosen-colour player state)]
    (if (nil? s)
      nil
      ;else
      (assoc s :map (gr/update-route g route :claimed-by player)))))

;; take-card :: Player -> Card -> State -> State
(defn take-card
  "A player takes a card from the table into their hand, and deal another
  card to the table. There is no limit to the number of hand cards."
  [player card state]
  ; Assert that the card is available
  {:pre [(>= (get-in state [:cards card]) 1)]}

  (-> state
      (update-in [:cards card] dec)
      (update-in [:player player :cards card] inc)
      (deal-table)))

;; take-random-card :: Player -> State -> State
(defn take-random-card
  "Take a card from the deck into their hand."
  [player state]
  (let [c (random-card (:deck state))]
    (-> state
        (update-in [:deck c] dec-to-0)
        (update-in [:player player :cards c] inc))))

#_(defn build-station
    "Build a train station in a city"
    [city player state])

;; take-ticket :: Player -> Ticket -> State -> State
(defn take-ticket
  "Take a new ticket from the pile into a player's hand."
  [player ticket state]
  (as-> state <>
     (update-in <> [:tickets] #(remove #{ticket} %))
     (update-in <> [:player player :tickets] #(cons ticket %))))

;; take-random-ticket :: Player -> State -> State
(defn take-random-ticket
  "Take a random ticket from the pile."
  [player state]
  (take-ticket player (rand-nth (:tickets state)) state))

;; The End
