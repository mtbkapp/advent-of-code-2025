(ns advent-of-code-2025.day04
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(def real-input
  (slurp (io/resource "day04.txt")))

(defn parse-input
  [input]
  (->> (string/split-lines input)
       (mapv vec)))

(defn vec+
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn read-rolls
  ([input] (read-rolls input 0 0 #{}))
  ([[c & cs] x y rolls]
   (cond (nil? c) rolls
         (= c \newline) (recur cs 0 (inc y) rolls)
         (= c \.) (recur cs (inc x) y rolls)
         (= c \@) (recur cs (inc x) y (conj rolls [x y])))))

(defn moveable?
  [rolls pos]
  (-> (for [dx [-1 0 1]
            dy [-1 0 1]
            :let [npos (vec+ pos [dx dy])]
            :when (and (not (and (zero? dx) (zero? dy)))
                       (contains? rolls npos))]
        npos)
      (count)
      (< 4)))

(defn find-movable
  [rolls]
  (into #{}
        (filter (partial moveable? rolls))
        rolls))

(defn simulate
  ([init-rolls]
   (simulate init-rolls 0))
  ([rolls removed-count]
   (let [removable (find-movable rolls)]
     (if (empty? removable)
       removed-count
       (recur (sets/difference rolls removable)
              (+ removed-count (count removable)))))))

; Part 1
#_(prn (count (find-movable (read-rolls test-input))))
#_(prn (count (find-movable (read-rolls real-input))))

; Part 2
#_(prn (simulate (read-rolls test-input)))
#_(prn (simulate (read-rolls real-input)))

