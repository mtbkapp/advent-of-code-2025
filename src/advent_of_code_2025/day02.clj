(ns advent-of-code-2025.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(def real-input
  (slurp (io/resource "day02.txt")))

(defn parse-input
  [input]
  (map #(mapv parse-long (string/split (string/trim %) #"\-"))
       (string/split input #",")))
   

(reduce (fn [sum [mn mx]]
          (+ sum (- mx mn)))
        0
        (parse-input test-input))

(defn bad-id1?
  [id]
  (let [sid (str id)
        len (count sid)]
    (and (even? len)
         (= (subs sid 0 (/ len 2)) 
            (subs sid (/ len 2))))))

(defn brute
  [bad? ranges]
  (reduce (fn [sum [mn mx]]
            (->> (filter bad? (range mn (inc mx)))
                 (apply + sum)))
          0
          ranges))

; Solve part 1
#_(brute bad-id1? (parse-input test-input))
#_(brute bad-id1? (parse-input real-input))

(defn bad-id2?
  [id]
  (let [sid (str id)]
    (some (fn [n]
            (apply = (partition-all n sid)))
          (range 1 (inc (quot (count sid) 2))))))

; solve part 2
#_(brute bad-id2? (parse-input test-input))
#_(brute bad-id2? (parse-input real-input))
