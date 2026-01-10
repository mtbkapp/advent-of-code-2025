(ns advent-of-code-2025.day11
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer [deftest testing is]]))

(def test-input
  "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(def test-input2
  "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(def real-input
  (slurp (io/resource "day11.txt")))

(defn read-input
  [input]
  (into {}
        (map (fn [line]
              (let [[node outputs] (string/split line #": ")]
                [node (string/split outputs #"\s+")])))
        (string/split-lines input)))

(def empty-graph
  {})

(defn add-edge
  [g [a b]]
  (-> g
      (update-in [a :out] (fnil conj #{}) b)
      (update-in [b :in] (fnil conj #{}) a)))

(defn remove-edge
  [g [a b]]
  (-> g
      (update-in [a :out] (fnil disj #{}) b)
      (update-in [b :in] (fnil disj #{}) a)))

(defn build-graph
  [input]
  (transduce (mapcat (fn [[node outputs]]
                       (map #(vector node %) outputs)))
             (completing add-edge)
             empty-graph
             (read-input input)))

(defn no-incoming
  [graph]
  (into #{}
        (comp (filter #(empty? (:in (val %))))
              (map key))
        graph))

(defn topo-sort
  "Topological sort via Kahn's algorithm."
  [graph]
  (loop [graph graph
         next-nodes (no-incoming graph) 
         topo []]
    (if (empty? next-nodes)
      (do (assert (every? #(empty? (:in (val %))) graph) "no cycles!")
          topo)
      (let [n (first next-nodes)
            {:keys [out]} (get graph n)
            G' (reduce (fn [g m]
                         (remove-edge g [n m]))
                       graph 
                       out)]
        (recur G'
               (into (disj next-nodes n)
                     (filter #(empty? (:in (get G' %))))
                     out)
               (conj topo n))))))

(defn count-paths 
  [graph start]
  (let [[n & nodes] (drop-while #(not= start %) (topo-sort graph))]
    (reduce (fn [paths n]
              (assoc paths n (->> (get-in graph [n :in])
                                  (map #(paths % 0))
                                  (apply +))))
            {n 1}
            nodes)))

; Algorithm
;
; The number of paths to a node is the sum of the number of paths to it's 
; incoming nodes. Using topological ordering ensures that incoming nodes are
; processed first.

#_(prn (solve-part1 test-input))
#_(prn (solve-part1 real-input))
(defn solve-part1
  [input]
  (let [g (build-graph input)]
    (get (count-paths g "you") "out")))

#_(prn (solve-part2 test-input2))
#_(prn (solve-part2 real-input))
(defn solve-part2
  [input]
  (let [g (build-graph input)]
    (* (get (count-paths g "svr") "fft")
       (get (count-paths g "fft") "dac")
       (get (count-paths g "dac") "out"))))

