(ns advent-of-code-2025.day09
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]])
  (:import [java.awt Polygon]
           [java.awt.geom Area]))

(set! *warn-on-reflection* true)

(def test-input
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(def real-input
  (slurp (io/resource "day09.txt")))

(defn read-input
  [input]
  (map (fn [line]
         (let [[x y] (string/split line #",")]
           [(parse-long x) (parse-long y)]))
       (string/split-lines input)))

(defn gen-pairs
  [xs]
  (for [a xs
        b xs
        :while (not= a b)]
    [a b]))

(defn calc-area
  [[[ax ay] [bx by]]]
  (* (inc (abs (- by ay)))
     (inc (abs (- bx ax)))))

#_(solve-part1 test-input)
#_(solve-part1 real-input)
(defn solve-part1
  [input]
  (let [points (read-input input)]
    (transduce (map calc-area)
               max
               -1
               (gen-pairs points))))

(defn polygon-from-points
  [points]
  (let [poly (Polygon.)]
    (doseq [[x y] points]
      (.addPoint poly (int x) (int y)))
    poly))

(defn rect-from-opposing-corners
  [[[ax ay] [bx by]]]
  (doto (Polygon.)
    (.addPoint ax ay)
    (.addPoint bx ay)
    (.addPoint bx by)
    (.addPoint ax by)))

(defn polygon-contains
  [polygon corners]
  (let [outer-area (Area. polygon)
        inner-area (Area. (rect-from-opposing-corners corners))]
    (.subtract inner-area outer-area)
    (.isEmpty inner-area)))

#_(solve-part2 test-input)
#_(solve-part2 real-input)
(defn solve-part2
  [input]
  (let [points (read-input input)
        polygon (polygon-from-points points)]
    (reduce (fn [max-area pair]
              (let [area (calc-area pair)]
                (if (and (< max-area area) 
                         (polygon-contains polygon pair))
                  area
                  max-area)))
            -1 
            (gen-pairs points))))

