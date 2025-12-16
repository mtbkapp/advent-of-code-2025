(ns advent-of-code-2025.day08
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input
  "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(def real-input
  (slurp (io/resource "day08.txt")))

(defn read-input
  [input]
  (map #(mapv parse-long (string/split % #",")) (string/split-lines input)))

(defn dist
  [[ax ay az] [bx by bz]]
  (Math/sqrt (+ (Math/pow (- bx ax) 2)
                (Math/pow (- by ay) 2)
                (Math/pow (- bz az) 2))))

(defn pairs
  [jboxes]
  (for [a jboxes
        b jboxes
        :while (not= a b)]
    [a b]))

(defn sort-by-dist
  [pairs]
  (sort-by identity
           (fn [[a b] [c d]]
             (compare (dist a b)
                      (dist c d)))
           pairs))

(defn find-circuit
  [circuits jbox]
  (some (fn [circuit]
          (when (contains? circuit jbox)
            circuit))
        circuits))

(defn combine
  [circuits [a b]]
  (let [a-circuit (find-circuit circuits a)
        b-circuit (find-circuit circuits b)]
    (-> circuits
        (disj a-circuit)
        (disj b-circuit)
        (conj (sets/union a-circuit b-circuit)))))

(defn init-circuits
  [jboxes]
  (into #{}
        (map (fn [a] #{a})) 
        jboxes))

(defn solve-part1
  [input n]
  (let [jboxes (read-input input)
        circuits (init-circuits jboxes)
        closest-pairs (take n (sort-by-dist (pairs jboxes)))
        combined (reduce combine circuits closest-pairs)
        largest (take 3 (sort-by (comp - count) combined))]
    (apply * (map count largest))))

#_(prn (solve-part1 test-input 10))
#_(prn (solve-part1 real-input 1000))

(defn solve-part2
  [input]
  (let [jboxes (read-input input)
        circuits (init-circuits jboxes)
        closest-pairs (sort-by-dist (pairs jboxes))]
    (reduce (fn [circuits pair]
              (let [next-circuits (combine circuits pair)]
                (if (= 1 (count next-circuits))
                  (reduced (* (ffirst pair) (-> pair second first)))
                  next-circuits)))
            circuits
            closest-pairs)))

#_(prn (solve-part2 test-input))
#_(prn (solve-part2 real-input))

