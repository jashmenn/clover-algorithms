;; Guided Local Search algorithm in the Clojure Programming Language
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com
;; by Nate Murray <nate@xcombinator.com>

(ns clover.algorithms.stochastic.guided-local-search
  (:require [clojure.contrib.math :as math])
  (:use [clover data util])
  (:import [java.lang Math]))

(defn search 
  ([max-iter max-no-improv cities]
     (search max-iter max-no-improv cities identity))
  ([max-iter max-no-improv cities callback]
     (let [new-vec (random-permutation cities)
           new-cost (cost new-vec cities)
           best (local-search {:vector new-vec :cost new-cost}
                              cities max-no-improv)]
       (do-search 0 max-iter max-no-improv best cities callback))))

(defn -main [& args]
  (let [max-iter 150
        max-no-improv 20
        alpha 0.3
        local-search-optima 12000.0
        lambda (* alpha (/ local-search-optima (count berlin52)))
        best (search max-iter berlin52 max-no-improv lambda)]
    (println "Done. Best Solution: c=" (best :cost) "(vs. optimal 7542)"
                                  "v=" (pr-str (best :vector)))))
