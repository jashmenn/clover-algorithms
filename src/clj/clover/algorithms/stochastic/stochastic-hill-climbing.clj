;; Stochastic Hill Climbingin the Clojure Programming Language
;; by Nate Murray <nate@xcombinator.com>
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com

(ns clover.algorithms.stochastic.stochastic-hill-climbing
  (:require [clojure.contrib.math :as math]))

(defn random-between [min max]
  (+ min (* (- max min) (Math/random))))

(defn random-bitstring [num-bits]
  (vec (map (fn [_] (if (< (Math/random) 0.5) "1" "0")) 
              (range 0 num-bits))))

(defn onemax [col]
  (reduce (fn [sum v] 
            (+ sum (if (= v "1") 1 0))) 0 col))

(defn do-search [iter max-iter num-bits candidate]
  (if (< (- max-iter 1) iter))
    candidate
    (let [new-vec (random-neighbor (candidate :vector))
          new-cost (onemax new-vec)
          new-candidate (:vector new-vec :cost new-cost)]
      (if (>= new-cost (candidate :cost))
        (if (= num-bits cost)
          new-candidate
          (recur (inc iter) max-iter num-bits new-candidate))
        (recur (inc iter) max-iter num-bits candidate))))

(defn search [max-iter num-bits]
  (let [new-vec (random-bitstring num-bits)
        new-cost (onemax new-vec)]
   (do-search 0 max-iter num-bits
              {:vector new-vec :cost new-cost})))


(defn -main [& args]
  (let [num-bits 64
        max-iter 1000
        best (search max-iter num-bits)] 
    best))
