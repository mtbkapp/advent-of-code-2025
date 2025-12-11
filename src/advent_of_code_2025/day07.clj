(ns advent-of-code-2025.day07
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(def real-input
  (slurp (io/resource "day07.txt")))

(defn parse-line 
  [line]
  (into []
        (comp (map {\^ :splitter \S :start})
              (map-indexed vector)
              (remove (comp nil? second)))
        line))

#_(clojure.pprint/pprint (parse-input test-input))
(defn parse-input
  [input]
  (let [lines (string/split-lines input)]
    (transduce (map-indexed vector)
               (completing
                 (fn [init-state [y line]]
                   (reduce (fn [s [x t]]
                             (update s
                                     ({:start :beams :splitter :splitters} t)
                                     conj
                                     [x y]))
                           init-state
                           (parse-line line))))
               {:split-count 0 
                :beams #{} 
                :splitters #{}
                :height (count lines)}
               lines)))

(defn vec+
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn iter-beams
  [{:keys [split-count beams splitters] :as state}]
  (let [{:keys [split-count beams]}
        (transduce (map (fn [beam]
                          (let [next-beam (vec+ beam [0 1])]
                            (if (contains? splitters next-beam)
                              [(vec+ next-beam [-1 0]) 
                               (vec+ next-beam [1 0])]
                              [next-beam]))))
                   (completing
                     (fn [acc beams]
                       (cond-> (update acc :beams into beams)
                         (= 2 (count beams)) (update :split-count inc))))
                   {:split-count split-count :beams #{}}
                   beams)]
    {:split-count split-count
     :beams beams
     :splitters splitters}))

(defn run-beams
  [state n]
  (if (zero? n)
    state
    (recur (iter-beams state) (dec n))))


#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (let [{:keys [height] :as init-state} (parse-input input)]
    (:split-count (run-beams init-state height))))

