(ns advent-of-code-2025.day10
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

; Basically the solution [here](https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/)
; with additional insight from [this](https://aoc.winslowjosiah.com/solutions/2025/day/10/)

(def test-input
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(def real-input
  (slurp (io/resource "day10.txt")))

(defn read-longs
  [s]
  (map parse-long (string/split s #",")))

(defn read-input
  [input]
  (->> (string/split-lines input)
       (map (fn [line]
              (let [[section & ss] (string/split line #" ")
                    lights (->> (string/replace section #"\[|\]" "")
                                (mapv #(= % \#)))
                    buttons (map #(read-longs (string/replace % #"\(|\)" ""))
                                 (butlast ss))
                    joltage (-> (last ss)
                                (string/replace #"\{|\}" "")
                                (read-longs)
                                vec)]
                {:goal-lights lights
                 :init-lights (vec (repeat (count lights) false))
                 :goal-joltage joltage
                 :init-joltage (vec (repeat (count joltage) 0))
                 :buttons buttons})))))

(defn press-button
  [lights button]
  (reduce (fn [next-lights i]
            (update next-lights i not))
          lights
          button))

(defn button-combos
  [[b & bs :as buttons]]
  (cond (empty? buttons) []
        (= 1 (count buttons)) [[] [b]]
        :else
        (let [sub (button-combos bs)]
          (into sub (map #(conj % b) sub)))))

(defn solve-part1-for-machine
  [{:keys [init-lights goal-lights buttons]}]
  (transduce (comp (map (juxt identity 
                              (fn [combo]
                                (reduce press-button init-lights combo))))
                   (filter (comp #(= goal-lights %) second))
                   (map (comp count first)))
             min
             Long/MAX_VALUE
             (button-combos buttons)))

#_(solve-part1 test-input)
#_(solve-part1 real-input)
(defn solve-part1
  [input]
  (transduce (map solve-part1-for-machine)
             +
             (read-input input)))

(defn light-pattern
  [init-lights buttons]
  (reduce press-button
          init-lights
          buttons))

(defn button-combos-by-light-pattern 
  [init-lights buttons]
  (reduce (fn [idx combo]
            (update idx
                    (light-pattern init-lights combo)
                    (fnil conj #{})
                    combo))
          {}
          (button-combos buttons)))

(defn joltage-parity
  [joltage]
  (map odd? joltage))

(defn decrease-joltage
  [joltage combo]
  (->> (flatten combo)
       (frequencies)
       (reduce (fn [new-joltage [i mag]]
                 (update new-joltage i - mag))
               joltage)))

(defn halve-joltage
  [joltage]
  (mapv (fn [j]
          (assert (even? j))
          (/ j 2)) 
        joltage))

(declare button-presses-for-joltage)

(defn button-presses-for-joltage*
  [patterns joltage]
  (cond (every? zero? joltage) 0
        (some neg? joltage) (long Integer/MAX_VALUE)
        :else
        (if-let [combos (get patterns (joltage-parity joltage))]
          (let [presses (map (fn [combo]
                               (let [next-joltage (halve-joltage (decrease-joltage joltage combo))
                                     presses (button-presses-for-joltage patterns next-joltage)]
                                 (+ (* 2 presses) (count combo))))
                             combos)]
            (apply min presses))
          (long Integer/MAX_VALUE))))

(def button-presses-for-joltage
  (memoize button-presses-for-joltage*))

#_(solve-part2 test-input)
#_(solve-part2 real-input)
(defn solve-part2
  [input]
  (transduce (map (fn [{:keys [init-lights buttons goal-joltage] :as machine}]
                    (button-presses-for-joltage 
                      (button-combos-by-light-pattern init-lights buttons)
                      goal-joltage)))
             +
             (read-input input)))

