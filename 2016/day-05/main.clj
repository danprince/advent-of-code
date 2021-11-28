(ns advent-of-code.day-05
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (BigInteger. 1 raw)))

(defn five-leading-zeros? [n]
  (< n 0x00000fffffffffffffffffffffffffff))

(defn int->hex [n]
  (format "%032x" n))

(defn char->int [c]
  (Integer/parseInt (str c) 16))

(defn get-valid-hashes [input]
  (->> (map str (repeat input) (range))
       (map md5)
       (filter five-leading-zeros?)
       (map int->hex)))

(defn hash-into-password [hash password]
  (let [len (count password)
        idx (char->int (nth hash 5))
        val (nth hash 6)
        cur (nth password idx nil)]
    (cond
      (not (< -1 idx len)) password
      (nil? cur) (assoc password idx val)
      :else password)))

(defn part-1 [input]
  (->> (get-valid-hashes input)
       (map #(nth % 5))
       (take 8)
       (apply str)))

(defn part-2 [input]
  (loop [hashes (get-valid-hashes input)
         password (vec (repeat 8 nil))]
    (let [[hash & rest] hashes
          password (hash-into-password hash password)]
      (if (every? identity password)
        (str/join password)
        (recur rest password)))))

(assert (part-1 "abc") "18f47a30")
(assert (part-2 "abc") "05ace8e3")

(let [input (slurp "day-05/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
