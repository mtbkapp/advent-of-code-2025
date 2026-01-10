(ns advent-of-code-2025.day12
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input 
  "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

(def real-input
  (slurp (io/resource "day12.txt")))

(defn parse-shape
  [s]
  (let [[id-part & shape-parts] (string/split-lines s)
        id (parse-long (string/replace id-part #":" ""))]
    [id {:shape (mapv vec shape-parts)
         :area (->> shape-parts
                    (map seq)
                    (flatten)
                    (filter #(= \# %))
                    (count))}]))

(defn parse-sections
  [s]
  (map (fn [line]
         (let [[size pack-counts] (string/split line #":\s")]
           {:size (mapv parse-long (string/split size #"x"))
            :pack-counts (into {} 
                               (comp (map parse-long)
                                     (filter pos?)
                                     (map-indexed vector))
                               (string/split pack-counts #"\s+"))}))
       (string/split-lines s)))

(defn read-input
  [input]
  (let [sections (string/split input #"\n\n")]
    {:shapes (into {}
                   (map parse-shape)
                   (butlast sections))
     :areas (parse-sections (last sections))}))

(defn fits
  [shapes {:keys [size pack-counts]}]
  (let [required-area (transduce 
                        (map (fn [[shape-id shape-count]]
                               (* (get-in shapes [shape-id :area])
                                  shape-count)))
                        +
                        pack-counts)
        [width height] size]
    (cond (< (* width height) required-area)
          :fits/allocated-area-too-small
          (and (<= (* 9 (apply + (vals pack-counts))) (* width height)))
          :fits/assume-each-shape-is-just-a-square
          :else
          :fits/unknown)))

#_(frequencies (solve-part1 test-input))
#_(frequencies (solve-part1 real-input))
(defn solve-part1
  [input]
  (let [{:keys [shapes areas]} (read-input input)]
    (map (partial fits shapes) areas)))

; Almost got nerd snipped on a (probably impossible) packing problem! 

