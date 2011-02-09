;; This is the third implementation of the Monkey and Banana problem
;; from pg 46 of Ivan Bratko's PROLOG Programming for Artifical
;; Intelligence. As well as using monads to abstract away running the
;; planner, it introduces unification to simplify writing rules

(ns monkey-and-banana.three
  (require [clojure.set :as set])
  (use clojure.contrib.monads
       [me.fogus.unifycle :only [unifier]]))


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

(defmacro defrule
  "creates a new rule that matches its argument against the pattern provided. If
   the match holds, then the body is evaluated, otherwise m-zero is returned."
  [name pattern & body]
  (let [syms (->> pattern
                  flatten
                  (filter #(and (symbol? %)))
                  set
                  vec)]
    `(defn ~name
       [mv#]
       (try (let [{:syms ~syms} (unify '~pattern mv#)]
              (do ~@body))
            (catch java.lang.IllegalArgumentException e# m-zero)))))


;; Implementation:

(def start {:door false :window false})

(def ^{:doc "All the valid room locations"}
  room-locations #{:door :middle :window})

(with-monad nondeterministic-m
  (defrule grasp
    [:middle true :middle ?banana]
    (m-result [:middle true :middle true]))

  (defrule climb
    [?pos false ?pos ?banana]
    (m-result [?pos true ?pos ?banana]))

  (defrule walk
    [?monkey false ?box ?banana]
    (m-map-lift (fn [loc] [loc false ?box ?banana])
                (disj room-locations ?monkey)))

  (defrule push
    [?pos false ?pos ?banana]
    (m-map-lift (fn [loc] [loc false loc ?banana])
                (disj room-locations ?pos)))
  


  (defn new-moves
    "Given a game state, returns all the new moves that have not already been
     tried."
    [state]
    (domonad [history (nd-get-state)
              move   ((m-juxt grasp climb walk push) state)
              :when   (not (contains? history move))
              _       (nd-put-state (conj history move))]
              move))

  (defn complete? [[_ _ _ banana]] banana)
  
  (defn can-get?
    [start-state]
    (if (seq ((m-until complete? new-moves start-state) #{}))
      true
      false)))


