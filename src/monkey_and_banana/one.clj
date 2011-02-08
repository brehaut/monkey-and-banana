;; This is a basic implementation of the Monkey and Banana problem
;; from pg 46 of Ivan Bratko's PROLOG Programming for Artifical Intelligence

(ns monkey-and-banana.one
  (:require [clojure.set :as set]))

(defrecord State [monkey onbox box banana])

(def start (State. :door false :window false))

(def ^{:doc "All the valid room locations"}
  room-locations #{:door :middle :window})

(defn grasp
  "The monkey can grasp the banana only when the box is in the middle of the room
   and the monkey is standing on the box."
  [{:keys [monkey onbox box] :as state} ]
  (if (and (= monkey :middle) (= box :middle) onbox)
      #{(assoc state :banana true)}
      #{}))

(defn climb
  "The monkey can only climb on the box when it is in the same location as the
   box and it is currently on the floor"
  [{:keys [monkey onbox box] :as state}]
  (if (and (= monkey box) (not onbox))
      #{(assoc state :onbox true)}
      #{}))

(defn walk
  "The monkey can walk to any other location in the room as long as it is not
   currently on the box."
  [{:keys [monkey onbox] :as state}]
  (if onbox #{}
    (set (map #(assoc state :monkey %)
              (disj room-locations monkey)))))

(defn push
  "The moneky can push the box when it is not standing on the box, and it is
   currently standing in the same location as the box."
  [{:keys [monkey onbox box] :as state}]
  (if (or onbox (not= monkey box)) #{}
    (set (map #(assoc state :monkey % :box %)
              (disj room-locations monkey)))))

(defn new-moves-from-state
  [state]
  (apply set/union ((juxt grasp climb walk push) state)))

(defn new-moves
  "Returns all the new search space based on applying the monkeys moves to the
   current search space."
  [states history]
  (set/difference (apply set/union (map new-moves-from-state states)) history))

(def ^{:doc "The scenario is finished if the monkey has the banana."}
  complete? (partial some :banana))

(defn can-get?
  "can-get? runs the monkey from the initial state until either it gets the
   banana or runs out of unique states.

   Typical callers will only need the arity 1 variation of the function"
  ([state]
     (can-get? #{state} #{}))
  ([states prior-states]
     (cond (empty? states) false
           (complete? states) true
           :else (recur (new-moves states prior-states)
                        (set/union states prior-states)))))
