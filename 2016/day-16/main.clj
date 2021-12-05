(ns day-16.main
  (:require [clojure.string :as str]))

(defn flip [n]
  (if (pos? n) 0 1))

(defn generate-more-data [data]
  (let [a data
        b (map flip (reverse a))]
    (concat a [0] b)))

(assert (= (generate-more-data [1]) [1 0 0]))
(assert (= (generate-more-data [0]) [0 0 1]))
(assert (= (generate-more-data [1 1 1 1 1]) [1 1 1 1 1 0 0 0 0 0 0]))
(assert (= (generate-more-data [1 1 1 1 0 0 0 0 1 0 1 0]) [1 1 1 1 0 0 0 0 1 0 1 0 0 1 0 1 0 1 1 1 1 0 0 0 0]))

(defn check-pair [[a b]]
  (if (= a b) 1 0))

(defn checksum [data]
  (loop [data data]
    (let [pairs (partition 2 data)
          sum (map check-pair pairs)]
      (if (even? (count sum))
        (recur sum)
        sum))))

(assert (= (checksum [1 1 0 0 1 0 1 1 0 1 0 0 ]) [1 0 0]))
(assert (= (checksum [1 0 0 0 0 0 1 1 1 1 0 0 1 0 0 0 0 1 1 1]) [0 1 1 0 0]))

(defn fill-disk [data size]
  (loop [data data]
    (if (>= (count data) size)
      (take size data)
      (recur (generate-more-data data)))))

(assert (= (fill-disk [1 0 0 0 0] 20) [1 0 0 0 0 0 1 1 1 1 0 0 1 0 0 0 0 1 1 1]))

(defn invalid-digit [c]
  (throw (Exception. (format "Invalid digit: %d" c))))

(defn parse-input [input]
  (for [c input]
    (case c \0 0, \1 1, (invalid-digit c))))

(defn part-1 [input]
  (-> (parse-input input) (fill-disk 272) checksum str/join))

(defn part-2 [input]
  (-> (parse-input input) (fill-disk 35651584) checksum str/join))

(let [input (slurp "day-16/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
