(ns advent-of-code-2025.day05
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input
  "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(def real-input
  (slurp (io/resource "day05.txt")))

#_(parse-input test-input)
#_(parse-input real-input)
(defn parse-input
  [input]
  (let [[ranges _ ids] (->> (string/split-lines input)
                            (partition-by empty?))]
    {:ranges (map #(mapv parse-long (string/split % #"-")) ranges)

     :ids (map parse-long ids)}))

(defn merge-overlapping-ranges
  "Sorts the ranages and merges overlapping ranges."
  [ranges]
  (let [[r & rs] (sort-by first ranges)]
    (reduce (fn [rs [mn mx :as curr-range]]
              (let [[last-mn last-mx] (last rs)]
                (if (<= last-mn mn last-mx)
                  (assoc rs (dec (count rs)) [last-mn (max last-mx mx)])
                  (conj rs curr-range))))
            [r] 
            rs)))

(defn process-input 
  [input]
  (-> input
      parse-input
      (update :ranges merge-overlapping-ranges)))

(defn find-range
  "Given a vector of ranges that are non overlapping and sorted by the min id
  and and id find the range the id is in if it exists via binary search."
  [ranges id]
  (cond (empty? ranges) nil
        (= 1 (count ranges))
        (let [[mn mx :as r] (first ranges)]
          (when (<= mn id mx) r))
        :else
        (let [middle-idx (quot (count ranges) 2) 
              [mn mx :as middle] (nth ranges middle-idx)]
          (cond (< id mn) (recur (subvec ranges 0 middle-idx) id)
                (<= mn id mx) middle 
                (< mx id) (recur (subvec ranges (inc middle-idx)) id)))))

(defn solve-part1
  [input]
  (let [{:keys [ranges ids]} (process-input input)]
    (->> (filter #(find-range ranges %) ids)
         (count))))

#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))

(defn solve-part2
  [input]
  (let [{:keys [ranges]} (process-input input)]
    ; since the ranges are non overlapping just sum their sizes
    (transduce (map (fn [[mn mx]] (inc (- mx mn))))
               +
               ranges)))

#_(prn (solve-part2 test-input))
#_(prn (solve-part2 real-input))
