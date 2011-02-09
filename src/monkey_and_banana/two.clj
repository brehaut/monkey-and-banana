;; This is a basic implementation of the Monkey and Banana problem
;; from pg 46 of Ivan Bratko's PROLOG Programming for Artifical Intelligence

(ns monkey-and-banana.two
  (require [clojure.set :as set])
  (use clojure.contrib.monads))


(def nondeterministic-m (state-t set-m))

(defn nd-get-state 
    [] 
    (fn [state] #{[state state]}))

(defn nd-put-state 
    [state] 
    (fn [_] #{[nil state]}))

(defmonadfn m-map-lift
  "Applies f to each item in xs, lifting the result into the monadic context, then
   reduces the results with m-plus."
  [f xs]
  (apply m-plus (map (comp m-result f) xs)))

(defmonadfn m-juxt
  "Calls a set of monadic functions against one argument, and combines the
   result with m-plus."
  [& fs]
  (fn [v] (apply m-plus (map (fn [f] (f v)) fs))))

;; Implementation:

(def start {:monkey :door :onbox false :box :window :banana false})

(def ^{:doc "All the valid room locations"}
  room-locations #{:door :middle :window})

(with-monad nondeterministic-m
  (defn grasp
    "The monkey can grasp the banana only when the box is in the middle of the room
     and the monkey is standing on the box."
    [{:keys [monkey onbox box] :as state} ]
    (domonad [:when (and (= monkey :middle) (= box :middle) onbox)]
             (assoc state :banana true)))

  (defn climb
    "The monkey can only climb on the box when it is in the same location as the
     box and it is currently on the floor"
    [{:keys [monkey onbox box] :as state}]
    (domonad [:when (and (= monkey box) (not onbox))]
             (assoc state :onbox true)))

  (defn walk
    "The monkey can walk to any other location in the room as long as it is not
     currently on the box."
    [{:keys [monkey onbox] :as state}]
    (if onbox m-zero
        (m-map-lift #(assoc state :monkey %)
                    (disj room-locations monkey))))

  (defn push
    "The moneky can push the box when it is not standing on the box, and it is
     currently standing in the same location as the box."
    [{:keys [monkey onbox box] :as state}]
    (if (or onbox (not= monkey box)) m-zero
        (m-map-lift #(assoc state :monkey % :box %)
                    (disj room-locations monkey))))


  (defn new-moves
    "Given a game state, returns all the new moves that have not already been tried."
    [state]
    (domonad [history (nd-get-state)
              move   ((m-juxt grasp climb walk push) state)
              :when   (not (contains? history move))
              _       (nd-put-state (conj history move))]
              move))

  (defn can-get?
    [start-state]
    (if (seq ((m-until :banana new-moves start-state) #{}))
      true
      false)))


