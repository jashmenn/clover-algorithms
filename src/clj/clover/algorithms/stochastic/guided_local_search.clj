;; Guided Local Search algorithm in the Clojure Programming Language
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com
;; by Nate Murray <nate@xcombinator.com>

(ns clover.algorithms.stochastic.guided-local-search
  (:require [clojure.contrib.math :as math])
  (:use [clover data util])
  (:import [java.lang Math]))

(defn local-search [current cities penalties max-no-improv lambda])
(defn calculate-feature-utilities [penalties cities candidate-vec])
(defn update-penalties [penalties cities current-vec utilities])

(defn do-search [iter max-iter cities max-no-improv lambda penalties best opts]
  (println " > iteration" iter "best=" (best :cost) "aug=" (best :aug-cost))
  ((:callback-best opts) iter best)
  (if (< (- max-iter 1) iter)
    best
    (let [candidate (local-search current cities penalties max-no-improv lambda)
          utilities (calculate-feature-utilities penalties cities (candidate :vector))
          new-penalties (update-penalties penalties cities (current :vector) utilities)]
      (if (< (candidate :cost) (best :cost))
        (recur (inc iter) max-iter cities max-no-improv lambda new-penalties candidate opts)
        (recur (inc iter) max-iter cities max-no-improv lambda new-penalties best opts)))))
 
(defn make-opts [opts]
  (merge {:callback-best  (fn [_ _]) 
          :callback-local (fn [_ _])} opts))

(defn search 
  ([max-iter cities max-no-improv lambda]
     (search max-iter cities max-no-improv lambda (make-opts {})))
  ([max-iter max-no-improv cities lambda opts]
     (let [new-vec (randomize cities)
           penalties (repeat (count cities) (repeat (count cities 0)))
           best (local-search nil cities penalties max-no-improv lambda)]
       (do-search 0 max-iter cities max-no-improv lambda penalties opts))))

(defn -main [& args]
  (let [max-iter 150
        max-no-improv 20
        alpha 0.3
        local-search-optima 12000.0
        lambda (* alpha (/ local-search-optima (count berlin52)))
        best (search max-iter berlin52 max-no-improv lambda)]
    (println "Done. Best Solution: c=" (best :cost) "(vs. optimal 7542)"
                                  "v=" (pr-str (best :vector)))))
