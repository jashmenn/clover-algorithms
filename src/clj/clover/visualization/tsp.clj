;;
(ns clover.visualization.tsp
  (:use rosado.processing)
  (:import (javax.swing JFrame))
  (:import (processing.core PApplet))
  (:require [clover.algorithms.stochastic.iterated-local-search :as ils]))

(def screen-size [600 600])

(defn draw-cities 
  ;; todo, center them on the screen size. dont stretch
  ([cities] 
     (draw-cities cities (apply max (map first cities))
                         (apply max (map last  cities))))
  ([cities max-x max-y]
     (fill-float 209 125 0)
     (doall 
      (map 
       (fn [[in-x in-y]]
         (let [xten (/ (first screen-size) 14)
               yten (/ (last screen-size) 14)
               x (map-to in-x 0 1800 xten (- (first screen-size) xten))
               y (map-to in-y 0 1800 yten (- (last screen-size) xten))]
           (ellipse x y 10 10))) 
       cities))))

(defn fancy-draw
  "An example of a function which does *something*."
  [dst]
  (background-float 20 20 20)
  (draw-cities ils/berlin52)
  ;;(background-float (rand-int 256) (rand-int 256) (rand-int 256))
  ;;(fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  ;;(ellipse 100 100 (rand-int 90) (rand-int 90))
  ;;(stroke-float 10)
  ;;(line 10 10 (rand-int 150) (rand-int 150))
  ;;(no-stroke)
  ;;(filter-kind INVERT)
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

(.init p5-applet)

(def swing-frame (JFrame. "Processing with Clojure"))
(doto swing-frame
	(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
	(.setSize (first screen-size) (last screen-size))
	(.add p5-applet)
	(.pack)
	(.show))

