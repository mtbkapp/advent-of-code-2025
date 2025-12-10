(ns advent-of-code-2025.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input
  "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(def real-input
  (slurp (io/resource "day06.txt")))

(def op->fn
  {"*" *
   "+" +})

#_(parse-input test-input)
(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map #(->> (string/split % #"\s+") 
                  (remove empty?)
                  (mapv (fn [s]
                          (if-let [n (parse-long s)]
                            n
                            s)))))))

#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (let [worksheet (parse-input input)]
    (let [nums (butlast worksheet)]
      (->> (map-indexed vector (last worksheet))
           (reduce (fn [total [i op]]
                     (let [col (map #(nth % i) nums)]
                       (+ total (apply (op->fn op) col))))
                   0)))))


#_(prn (solve-part2 test-input))
#_(prn (solve-part2 real-input))
(defn solve-part2 
  [input]
  (let [chrs (map vec (string/split-lines input))
        width (count (first chrs))
        ops (->> (last chrs)
                 (remove #(= \space %)) 
                 (map str))
        nums-chrs (butlast chrs)
        xfer-xs->all (fn [{:keys [all xs]}]
                       {:all (conj all xs) :xs []})
        save-x (fn [{:keys [all xs]} x]
                 {:all all :xs (conj xs x)})
        nums (->> (reduce (fn [acc col-idx]
                            (let [col (map #(nth % col-idx) nums-chrs)
                                  x (-> (apply str col) string/trim parse-long)]
                              (if (nil? x)
                                (xfer-xs->all acc)
                                (save-x acc x))))
                          {:all [] :xs []}
                          (range (dec width) -1 -1))
                  xfer-xs->all
                  :all
                  reverse)]
    (->> (map (fn [op xs]
                (apply (op->fn op) xs))
              ops
              nums)
         (apply +))))
