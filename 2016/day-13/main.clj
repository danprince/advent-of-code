(ns day-13
  (:require [common.astar :refer [shortest-path]]))

(defn manhattan-dist
  "Returns the Manhattan distance between two points."
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn von-neumann-neighbours
  "Returns the Von Neumann neighbourhood of a point (up, down, left, right)."
  [[x y]]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn flood-fill
  "Returns all nodes reached from `start` a flood fill in `max-steps` steps."
  [start neighbours max-steps]
  (let [frontier (ref [start])
        visited (ref #{})]
    (dosync
     (dotimes [_ max-steps]
       (let [batch @frontier]
         (alter frontier empty)
         (doseq [current batch]
           (alter visited conj current)
           (doseq [next (neighbours current)]
             (when-not (@visited next)
               (alter frontier conj next)
               (alter visited conj next))))))
     @visited)))

(defn count-set-bits [x]
  (loop [count 0, x x]
    (if (pos? x)
      (recur (+ count (bit-and x 1)) (bit-shift-right x 1))
      count)))

(defn is-wall?
  "Returns true if `[x y]` is a wall, given an `input` number."
  [input [x y]]
  (if (or (neg? x) (neg? y))
    true
    (-> input
        (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
        count-set-bits
        odd?)))

(defn debug-area [input w h details]
  (doseq [y (range h)]
    (doseq [x (range w)]
      (if-let [ch (details [x y])]
        (print ch)
        (print (if (is-wall? input [x y]) \# \.))))
    (print \newline)
    (flush)))

(defn part-1 [input start goal]
  (let [input (Integer/parseInt input)
        valid-node? (fn [node] (not (is-wall? input node)))
        neighbours (fn [node] (filter valid-node? (von-neumann-neighbours node)))
        shortest-path (shortest-path start goal manhattan-dist neighbours)]
    (count shortest-path)))

(defn part-2 [input start]
  (let [input (Integer/parseInt input)
        valid-node? (fn [node] (not (is-wall? input node)))
        neighbours (fn [node] (filter valid-node? (von-neumann-neighbours node)))]
    (count (flood-fill start neighbours 50))))

(comment
  (part-1 "10" [1 1] [7 4]))

(let [input (slurp "day-13/input.txt")]
  (time (println "Part 1:" (part-1 input [1 1] [31 39])))
  (time (println "Part 2:" (part-2 input [1 1]))))

