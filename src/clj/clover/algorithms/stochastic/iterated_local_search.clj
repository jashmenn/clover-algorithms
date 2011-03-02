;; Iterated Local Search algorithm in the Clojure Programming Language
;; by Nate Murray <nate@xcombinator.com>
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com

(ns clover.algorithms.stochastic.iterated-local-search
  (:require [clojure.contrib.math :as math]))

(def berlin52 
   "these are xy coordinates from the berlin52 tsp problem. 
see http://www2.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/tsp/ for more problems"
     (partition 2 [565 575 25 185 345 750 945 685 845 655 880 660 25 230 525 1000 580 1175 650 1130 1605 620 1220 580 1465 200 1530 5 845 680 725 370 145 665 415 635 510 875 560 365 300 465 520 585 480 415 835 625 975 580 1215 245 1320 315 1250 400 660 180 410 250 420 555 575 665 1150 1160 700 580 685 595 685 610 770 610 795 645 720 635 760 650 475 960 95 260 875 920 700 500 555 815 830 485 1170 65 830 610 605 625 595 360 1340 725 1740 245]))

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
    (concat p1 p2)))

(defn perturbation [cities best]
  (let [new-vec (double-bridge-move (best :vector))
        new-cost (cost (new-vec cities))]
    {:vector new-vec :cost new-cost}))

(defn do-search [iter max-iter max-no-improv best cities]
  (println " > iteration" iter "best=" (best :cost))
  (if (< (- max-iter 1) iter)
    best
    (let [candidate (-> (perturbation cities best)
                        (local-search cities max-no-improv))]
      (if (< (candidate :cost) (best :cost))
        (recur (inc iter) max-iter max-no-improv candidate cities)
        (recur (inc iter) max-iter max-no-improv best cities)))))

(defn search [max-iter max-no-improv cities]
  (let [new-vec (random-permutation cities)
        new-cost (cost new-vec cities)
        best (local-search {:vector new-vec :cost new-cost}
                           cities max-no-improv)]))

(defn -main [& args]
  (let [max-iter 100
        max-no-improv 50
        best (search max-iter max-no-improv berlin52)]
    best))
