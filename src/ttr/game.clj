;; game.clj
;; AndrewJ 2020-05-10

(ns ttr.game
  (:gen-class)
  (:require [ttr.state :as st]
            [ttr.graph :as gr]
            [ttr.action :as act]
            [ttr.num-map :as num]))

;;-------------------------------
;; Utilities

;;-------------------------------
;; init-game :: Int -> State
(defn init-game
  "Set up the initial game state"
  [nplayers]
  (->> (st/empty-state nplayers)
       (act/deal-table)))

;;-------------------------------
;; take-card-options :: Player -> State -> [Action]
(defn take-card-options
  "Enumerate all the actions for a player to take cards."
  [player state]
  (concat
   (list `(act/take-random-card ~player))
   (for [c (num/map-enumerate (get state :cards))]
     `(act/take-card ~player ~c))))

;;-------------------------------
#_(defn- route->map
    "Turn a route into a proper map."
    [r]
    (into (last r) {:src (first r), :dest (second r)}))

(defn- get-eligible-routes
  "Get all the eligible routes based on the number of each colour card in the hand."
  [player state]
  (for [[colour n] (get-in state [:player player :cards])
        r (range 1 (inc n))
        :when (not (= colour :loco))]
    (let [r1 (gr/get-routes (:map state) {:colour colour
                                          :length r
                                          :claimed-by nil})
          ; And get all the uncoloured routes too
          r2 (gr/get-routes (:map state) {:colour :none
                                          :length r
                                          :claimed-by nil})]
      (concat r1 r2))))

(defn- routes->actions
  "Turn the routes into claim-route actions. 
  Note that the chosen-colour for all routes is :none. A policy needs to
  be invoked by the consumer on the resulting actions to override this for
  uncoloured routes."
  [player routes]
  (for [r routes]
    `(act/claim-route  ~player :none ~r)))

;; claim-route-options :: Player -> State -> [Action]
(defn claim-route-options
  "Enumerate all the claimable routes based on the hand cards and what's
  not yet claimed."
  [player state]
  (->> state
   (get-eligible-routes player)
   (apply concat) ;flatten one level
   (dedupe)
   (routes->actions player)))

;;-------------------------------
;; available-actions :: Player -> State -> [Action]
(defn available-actions
  "Return all the available actions, given a player and a current state."
  [player state]
  (reduce concat
          '()
          ((juxt take-card-options claim-route-options)
           player state)))

;;-------------------------------
;; apply-action :: Action -> State -> State
(defn apply-action
  "Apply an action to a state"
  [action state]
  (eval (concat action (list state))))

(def s0 (init-game 2))

;; The End
