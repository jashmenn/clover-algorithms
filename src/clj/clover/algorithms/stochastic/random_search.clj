;; Random Search in the Clojure Programming Language
;; by Nate Murray <nate@xcombinator.com>
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com

(ns clover.algorithms.stochastic.random-search
  (:require [clojure.contrib.math :as math]))

(defn random-between [min max]
  (+ min (* (- max min) (Math/random))))

(defn random-vector [minmax]
  (map (fn [dimension] 
         (random-between (first dimension) (last dimension))) 
       minmax))

(defn objective-function [vector]
  (reduce (fn [sum x] (+ sum (math/expt x 2.0))) 0 vector))

(defn search 
  ([search-space max-iter] (search search-space max-iter nil 0))
  ([search-space max-iter best iter]
    (if (> iter max-iter)
      best
      (let [new-vec  (random-vector search-space)
            new-cost (objective-function new-vec)
            candidate {:vector new-vec :cost new-cost}]
        (if (or (not best) (< new-cost (best :cost)))
          (recur search-space max-iter candidate (inc iter))
          (recur search-space max-iter best      (inc iter)))))))

(defn -main [& args]
  (let [problem-size 2
        search-space (map (fn [i] [-5 5]) (range problem-size))
        max-iter 1000
        best (search search-space max-iter)]
    best))

(comment

  (-main)

 )
