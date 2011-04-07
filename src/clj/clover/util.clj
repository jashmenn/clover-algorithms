
(ns clover.util
  (:require [clojure.contrib.math :as math])
  (:import [java.lang Math]))

(defn euc-2d [p1 p2]
  (Math/round 
   (Math/sqrt 
    (+ (Math/pow (double (- (first p1) (first p2))) 2)
       (Math/pow (double (- (last  p1) (last  p2))) 2)))))

(defn randomize [col]
  (shuffle (range (count col))))

