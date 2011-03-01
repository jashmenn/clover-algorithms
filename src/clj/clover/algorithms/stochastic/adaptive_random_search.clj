;; Adaptive Random Search in the Clojure Programming Language
;; by Nate Murray <nate@xcombinator.com>
;; Adapted from Jason Brownlee's 'Clever Algorithms' Project: http://www.CleverAlgorithms.com

(ns clover.algorithms.stochastic.adaptive-random-search
  (:require [clojure.contrib.math :as math]))

(defn random-between [min max]
  (+ min (* (- max min) (Math/random))))

(defn random-vector [minmax]
  (map (fn [dimension] 
         (random-between (first dimension) (last dimension))) 
       minmax))

(defn objective-function [vector]
  (reduce (fn [sum x] (+ sum (math/expt x 2.0))) 0 vector))

(defn mod-zero? [num div]
  (= (mod num div) 0))

(defn large-step-size [iter step-size o]
  (let [{:keys [s-factor l-factor iter-mult]} o]
    (if (and (> iter 0) (mod-zero? iter iter-mult))
      (* step-size l-factor)
      (* step-size s-factor))))

(defn take-step [minmax current step-size]
  (map (fn [i]
         (let [min# (max (first (nth minmax i)) 
                        (- (nth current i) step-size))
               max# (min (last (nth minmax i))
                        (+ (nth current i) step-size))]
           (random-between min# max#)))
       (range (count current))))

(defn step-with-size [bounds current step-size]
  (let [new-vec (take-step bounds (current :vector) step-size)
        new-cost (objective-function new-vec)]
    {:vector new-vec :cost new-cost}))

(defn take-steps [bounds current step-size big-step-size]
  [(step-with-size bounds current step-size)
   (step-with-size bounds current big-step-size)])

(defn fmt-float [f] (format "%.10f" f))

(defn do-search [iter count bounds step-size current o]
  (println " > iteration " iter " best=" (fmt-float (current :cost)))
  (if (> iter (- (o :max-iter) 1)) 
    current
    (let [big-step-size (large-step-size iter step-size o)
          [step big-step] (take-steps bounds current step-size big-step-size)]
      (if (or (<= (step :cost) (current :cost))
               (<= (big-step :cost) (current :cost)))
        (if (<= (big-step :cost) (step :cost))
          (recur (inc iter) 0 bounds big-step-size big-step o)
          (recur (inc iter) 0 bounds step-size     step     o))
        (if (>= count (o :max-no-impr))
         (recur (inc iter) 0 bounds (/ step-size (o :s-factor)) current o)
         (recur (inc iter) (inc count) bounds step-size current o))))))

(defn search [bounds o]
  (let [step-size (* (- (-> bounds first last) (-> bounds first first)) 
                     (o :init-factor))
        new-vec (random-vector bounds)
        new-cost (objective-function new-vec)]
    (do-search 0 0 bounds step-size {:vector new-vec :cost new-cost} o)))

(defn -main [& args]
  (let [problem-size 2
        bounds (map (fn [i] [-5 5]) (range problem-size))
        opts {:max-iter 1000
              :init-factor 0.05
              :s-factor 1.3
              :l-factor 3.0
              :iter-mult 10
              :max-no-impr 30}
        best (search bounds opts)]
    (println "Done. Best Solution: " 
             "c=" (fmt-float (best :cost)) ", "
             "v=" (map fmt-float (best :vector)))))
