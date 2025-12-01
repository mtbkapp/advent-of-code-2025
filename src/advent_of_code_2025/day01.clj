(ns advent-of-code-2025.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input 
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(def real-input
  (slurp (io/resource "day01.txt")))

#_(parse-input test-input)
(defn parse-input
  [s]
  (map (fn [[dir & mag]]
         [(keyword (str dir)) (parse-long (apply str mag))])
       (string/split-lines s)))

(defn move
  [start [dir mag]]
  (mod ((if (= :L dir) - +) start mag) 100))

#_(solve-part1 test-input)
#_(solve-part1 real-input)
(defn solve-part1
  [input]
  (->> (reductions move 50 (parse-input input))
       (filter zero?)
       (count)))

(defn move2
  [{:keys [location zero-ticks]} [dir mag]]
  (let [q (quot mag 100)
        r (mod mag 100)
        l ((if (= :L dir) - +) location r)
        new-loc (mod l 100)
        crossed-zero? (not= l new-loc)
        started-zero? (= 0 location)
        landed-zero? (= 0 new-loc)]
    {:zero-ticks (+ zero-ticks 
                    q 
                    (if (and (not started-zero?)
                             (or crossed-zero? landed-zero?))  
                      1
                      0))
     :location new-loc}))

#_(solve-part2 test-input)
#_(solve-part2 real-input)
(defn solve-part2
  [input]
  (->> (reduce move2
               {:location 50 :zero-ticks 0}
               (parse-input input))
       :zero-ticks))
