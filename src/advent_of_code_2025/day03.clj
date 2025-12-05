(ns advent-of-code-2025.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input 
  "987654321111111
811111111111119
234234234234278
818181911112111")

(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map (partial mapv (comp parse-long str)))))

(defn find-max-element-and-index
  ([[x & xs]] (find-max-element-and-index xs 0 [x 0]))
  ([[x & xs] i [mx mxi]]
   (if (nil? x)
     [mx mxi]
     (recur xs
            (inc i)
            (if (< mx x)
              [x i]
              [mx mxi])))))

(defn choose-cells
  [pack n]
  (cond (< n 1)
        (throw (IllegalArgumentException. "n must be 1 or greater"))
        (= n 1)
        [(apply max pack)]
        :else 
        (let [end-i (- (count pack) (dec n))
              [mx mxi] (find-max-element-and-index (subvec pack 0 end-i))
              rest-digits (choose-cells (subvec pack (inc mxi)) 
                                        (dec n))]
          (cons mx rest-digits))))

(defn calc-joltage 
  [digits]
  (parse-long (apply str digits)))

(defn max-joltage
  [packs n]
  (reduce (fn [sum pack]
            (+ sum (calc-joltage (choose-cells pack n))))
          0
          packs))


#_(println "Part 1" "test input" (max-joltage (parse-input test-input) 2))
#_(println "Part 1" "real input" (max-joltage (parse-input real-input) 2))
#_(println "Part 2" "test input" (max-joltage (parse-input test-input) 12))
#_(println "Part 2" "real input" (max-joltage (parse-input real-input) 12))

