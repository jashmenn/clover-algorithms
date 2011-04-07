;;
(ns clover.visualization.tsp
  (:use rosado.processing)
  (:import (javax.swing JFrame))
  (:import (processing.core PApplet))
  (:require [clover.algorithms.stochastic.iterated-local-search :as ils])
  (:gen-class))

(def screen-size [600 600])
(def cities-range [0 1800])

(defn city-to-pixel [[in-x in-y]]
  (let [xten (/ (first screen-size) 14)
        yten (/ (last screen-size) 14)
        x (map-to in-x (first cities-range) (last cities-range) xten (- (first screen-size) xten))
        y (map-to in-y (first cities-range) (last cities-range) yten (- (last screen-size) xten))]
    [x y]))

(defn draw-cities 
  ([cities] 
     (draw-cities cities (apply max (map first cities))
                         (apply max (map last  cities))))
  ([cities max-x max-y]
     (no-stroke)
     (fill-float 209 125 0)
     (doall 
      (map 
       (fn [[in-x in-y]]
         (let [[x y] (city-to-pixel [in-x in-y])]
           (ellipse x y 10 10))) 
       cities))))

(def current-solution (atom nil))
(def local-solution   (atom nil))

(defn draw-solution [solution cities color]
  (when (seq solution)
    (doall
     (map 
      (fn [idx]
        (when (> idx 0)
          (let [[x1 y1] (city-to-pixel [(first (nth cities (nth solution (dec idx))))
                                        (last  (nth cities (nth solution (dec idx))))])
                [x2 y2] (city-to-pixel [(first (nth cities (nth solution idx)))
                                        (last  (nth cities (nth solution idx)))])]
            (apply stroke-float color)
            (line x1 y1 x2 y2)))) 
      (range (count solution))))))

(defn fancy-draw
  "An example of a function which does *something*."
  [dst]
  (background-float 30 30 30 200)
  (draw-cities ils/berlin52)
  (draw-solution @local-solution ils/berlin52 [0 59 86])
  (draw-solution @current-solution ils/berlin52 [255 255 255])
  (framerate 10))

(def p5-applet
     (proxy [PApplet] []
       (setup []
         (binding [*applet* this]
           (size (first screen-size) (last screen-size))
           (smooth)
           (no-stroke)
           (fill 226)
           (framerate 10)))
       (draw []
         (binding [*applet* this]
           (fancy-draw this)))))

(defn run-tsp [cities]
  (let [max-iter 1000
        max-no-improv 50
        cb1 (fn [iter best]     (swap! current-solution (fn [_] (best :vector))))
        cb2 (fn [solution cost] (swap! local-solution   (fn [_] solution)))
        best (ils/search max-iter max-no-improv cities 
                         {:callback-best cb1 :callback-local cb2})]
    (println "Done. Best Solution: c=" (best :cost) 
             "v=" (pr-str (best :vector)))))

(defn -main [& args] 
  (.init p5-applet)

  (def swing-frame (JFrame. "Processing with Clojure"))
  (doto swing-frame
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setSize (first screen-size) (last screen-size))
    (.add p5-applet)
    (.pack)
    (.show))

  (.start (Thread. (fn [] (run-tsp ils/berlin52)))))

