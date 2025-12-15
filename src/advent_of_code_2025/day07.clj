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

(defn iter-beams2
  [beam-counts splitters]
  (reduce (fn [new-beam-counts [pos c]]
            (let [next-pos (vec+ pos [0 1])
                  next-positions (if (contains? splitters next-pos)
                                   [(vec+ next-pos [-1 0])
                                    (vec+ next-pos [1 0])]
                                   [next-pos])]
              (reduce #(update %1 %2 (fnil + 0) c) 
                      new-beam-counts
                      next-positions)))
          {}
          beam-counts))

#_(prn (solve-part2 test-input))
#_(prn (solve-part2 real-input))
(defn solve-part2
  [input]
  (let [{:keys [beams splitters height]} (parse-input input)
        beam-counts {(first beams) 1}
        end-beam-counts (->> (iterate #(iter-beams2 % splitters) beam-counts)
                             (drop height)
                             first)]
    (apply + (vals end-beam-counts))))

