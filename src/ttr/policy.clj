(ns ttr.policy
  (:require [ttr.state :as st]
            [ttr.graph :as gr]
            [ttr.num-map :as num]
            [ttr.action :as act]
            [ttr.game :as g]
            [random-seed.core :as r]
            [spec-dict :refer [dict]]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

;;-------------------------------
;; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn log
  "Print a string to a log file."
  [s]
  (spit log-file-name s :append true)
  (spit log-file-name "\n" :append true))

;;-------------------------------
;; Utilities

(defn argmax-map
  "Find the key with the maximum value."
  [m]
  {:pre (map? m)}
  (key (apply max-key val m)))

(defn argmax
  "Return the value x in xs that maximises (f x)."
  [f xs]
  (apply max-key f xs))

(defn argmin
  "Return the value x in xs that minimises (f x)."
  [f xs]
  (apply min-key f xs))

;;-------------------------------
;; type Policy = Player -> State -> Action
;; apply-policy :: Policy -> Player -> State -> State
(defn apply-policy
  "Apply a given policy function to generate the next state."
  [policy player st]

  (let [action (policy player st)
        new-state (g/apply-action action st)]
    (log action)
    (log (st/pprint-state player new-state))
    new-state))


;;-------------------------------
;; @@TODO: implement
;; play-turn :: PolicySet -> Player -> State -> State
(defn play-turn
  "Play a turn for a given player, using different policies as appropriate"
  [policy-set player state]
  (let [{:keys [p-choose-action p-take-card p-claim-route p-choose-colour]} policy-set
        action (p-choose-action player state)]
    (cond (= action :take-card) (->> state
                                     (p-take-card player)
                                     (p-take-card player))
          (= action :claim-route) (->> state
                                       (p-claim-route player state))
          :else state)))

;-------------------------------
; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for each player.
   Limit the number of turns per player to `max-turns` with default 100."

  ; Limit to 100 iterations if none specified.
  ([policy-sets initial-state]
   (play-game policy-sets initial-state 100))

  ([policy-sets initial-state max-turns]

   #_(log (str policy-a))
   #_(log (str policy-b))
   (log "---- Initial state ----")
   (log (st/pprint-state initial-state))

  ; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (if (act/end-of-game? state)
        (reduced state)
        ;else
        (do
          (log (format "---- Iteration %d: ----" i))
          #_(->> state
               (apply-policy policy-a :a)
               (apply-policy policy-b :b)))))
    initial-state
    (range max-turns))))

;;===============================
;; Policies

;; Define a set of policies for a player
(s/def ::policy-set (dict {:choose-action symbol?
                           :take-ticket symbol?
                           :take-card symbol?
                           :claim-route symbol?}))

(s/def ::action-type #{:take-card :claim-route})

;; type Policy = Player -> State -> State

;;-------------------------------

;; p-action-random :: Player -> State -> ActionType
(defn p-action-random
  "Pick a random action"
  [player state]
  (rand-nth [:take-card :claim-route]))

;; p-ticket-random :: Player -> State -> State
(defn p-ticket-random
  "Pick a random ticket."
  [player state]
  (act/take-ticket player (rand-nth (:tickets state)) state))

;; p-card-random :: Player -> State -> State
(defn p-card-random
  "Take a random card."
  [player state]
  (act/take-card player (act/random-card (:deck state)) state))

;; p-route-random :: Player -> State -> State
(defn p-route-random
  "Claim a random route."
  [player state]
  (act/claim-route (rand-nth st/colours) player state))

;; A policy set of random choices
;; p-set-random :: PolicySet
(def p-set-random {:take-ticket p-ticket-random
                   :take-card p-card-random
                   :claim-route p-route-random
                   :choose-action p-action-random})

;; The End
