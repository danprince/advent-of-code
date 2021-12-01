(ns day-01
  (:require [clojure.string :as str]))

(def north [0 -1])
(def east [1 0])
(def south [0 1])
(def west [-1 0])
(def turns [north east south west])

(def +vec (partial map +))

(defn parse-instruction [s]
  (let [turn (first s)
        count (Integer/parseInt (subs s 1))]
    [turn count]))

(defn generate-path [[path heading] [turn count]]
  (let [rotate (case turn \L dec \R inc)
        heading (mod (rotate heading) 4)
        moves (map turns (repeat count heading))
        path (concat path moves)]
    [path heading]))

(defn parse [input]
  (->> (str/split input #", ")
       (map parse-instruction)
       (reduce generate-path [[] 0])
       first))

(defn taxicab-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn find-final-block [path]
  (reduce +vec path))

(defn find-first-repeat [path]
  (loop [visited #{}
         pos [0 0]
         path path]
    (let [[move & path] path
          pos (+vec pos move)]
      (cond
        (contains? visited pos) pos
        (not-empty path) (recur (conj visited pos) pos path)))))

(defn part-1 [input]
  (->> (parse input)
       find-final-block
       taxicab-distance))

(defn part-2 [input]
  (->> (parse input)
       find-first-repeat
       taxicab-distance))

(assert (= (part-1 "R2, L3") 5))
(assert (= (part-1 "R2, R2, R2") 2))
(assert (= (part-1 "R5, L5, R5, R3") 12))
(assert (= (part-2 "R8, R4, R4, R8") 4))

(let [input (slurp "day-01/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
