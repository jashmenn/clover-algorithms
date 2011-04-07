;; Guided Local Search algorithm in the Clojure Programming Language
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com
;; by Nate Murray <nate@xcombinator.com>

(ns clover.algorithms.stochastic.iterated-local-search
  (:require [clojure.contrib.math :as math])
  (:use [clover data util])
  (:import [java.lang Math]))

(defn cost [permutation cities]
  (reduce (fn [sum [i c1]]
            (let [c2 (if (= i (dec (count permutation))) 
                       (first permutation)
                       (nth permutation (inc i)))]
              (+ sum (euc-2d (nth cities c1) (nth cities c2))))) 
          0 (map-indexed (fn [i elem] [i elem]) permutation)))

(defn rand-but-not [max exclude]
  (loop [x (rand-int max)]
    (if (contains? exclude x) (recur (rand-int max)) x)))

(defn reverse-subvec [col start end]
  (let [ecol (vec col)
        p1 (subvec ecol 0 start)
        p2 (subvec ecol start (+ end 1))
        p3 (subvec ecol (+ end 1) (count ecol))]
    (vec (concat p1 (reverse p2) p3))))

(defn stochastic-two-opt [permutation]
  (let [psize (count permutation)
        c1 (rand-int psize)
        ex1 (if (= c1 0) (- psize 1) (- c1 1))
        ex2 (if (= c1 (- psize 1)) 0 (+ c1 1))
        exclude [c1 ex1 ex2]
        c2 (rand-but-not psize (set exclude))
        [min max] (sort [c1 c2])]
    (reverse-subvec permutation min max)))

(defn local-search [best-in cities max-no-improv]
  (loop [count 0 best best-in]
    (if (>= count max-no-improv) 
      best
      (let [new-vec (stochastic-two-opt (best :vector))
           new-cost (cost new-vec cities)]
       (if (< new-cost (best :cost))
         (recur 0 {:vector new-vec :cost new-cost})
         (recur (inc count) best))))))

(defn double-bridge-move [perm]
  (let [psize (count perm)
        pos1 (+      1 (rand-int (/ psize 4)))
        pos2 (+ pos1 1 (rand-int (/ psize 4)))
        pos3 (+ pos2 1 (rand-int (/ psize 4)))
        p1 (concat (subvec perm 0 pos1)
                   (subvec perm pos3 psize))
        p2 (concat (subvec perm pos2 pos3)
                   (subvec perm pos1 pos2))]
    (vec (concat p1 p2))))

(defn perturbation [cities best]
  (let [new-vec (double-bridge-move (best :vector))
        new-cost (cost new-vec cities)]
    {:vector new-vec :cost new-cost}))

(defn do-search [iter max-iter max-no-improv best cities callback]
  (println " > iteration" iter "best=" (best :cost))
  (callback iter best)
  (if (< (- max-iter 1) iter)
    best
    (let [candidate (-> (perturbation cities best)
                        (local-search cities max-no-improv))]
      (if (< (candidate :cost) (best :cost))
        (recur (inc iter) max-iter max-no-improv candidate cities callback)
        (recur (inc iter) max-iter max-no-improv best cities callback)))))

(defn random-permutation [cities]
  (shuffle (range (count cities))))

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
  (let [max-iter 100
        max-no-improv 50
        best (search max-iter max-no-improv berlin52)]
    (println "Done. Best Solution: c=" (best :cost) "(vs. optimal 7542)"
                                  "v=" (pr-str (best :vector)))))
