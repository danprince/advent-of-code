(ns day-14.main
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn md5
  "Returns a hex formatted lowercase MD5 checksum of the input string"
  [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def pattern-3 #"(\w)\1\1")
(def pattern-5 #"(\w)\1\1\1\1")

(defn run-of-3
  "Returns the first character that is repeated 3 times."
  [hash-fn idx] (->> idx hash-fn (re-find pattern-3) second first))

(defn runs-of-5
  "Returns the set of all characters that repeat at least 5 times."
  [hash-fn idx] (->> idx hash-fn (re-seq pattern-5) (map first) (map first) set))

(defn has-matching-run-of-5?
  "Returns true if one of the next thousand numbers has five or more `ch`
  sequentially."
  [hash-fn start ch]
  (loop [idx start]
    (let [fives (runs-of-5 hash-fn idx)]
      (if (contains? fives ch)
        true
        (if (< idx (+ start 1000))
          (recur (inc idx))
          false)))))

(defn is-key?
  "Returns true if this idx is a "
  [hash-fn idx]
  (when-let [three (run-of-3 hash-fn idx)]
    (has-matching-run-of-5? hash-fn (inc idx) three)))

(defn create-digest
  "Returns a hash function that applies a salted md5 hash `n` times."
  ([salt] (create-digest salt 1))
  ([salt n]
   (memoize
    (fn [x]
      (loop [i 0, hash x]
        (if (< i n)
          (recur (inc i) (md5 (str salt hash)))
          hash))))))

(defn part-1 [salt]
  (let [hash (create-digest salt)
        is-key? (partial is-key? hash)]
    (->> (range) (filter is-key?) (take 64) last)))

(defn part-2 [salt]
  (let [hash (create-digest salt 2017)
        is-key? (partial is-key? hash)]
    (->> (range) (filter is-key?) (take 64) last)))

(let [input (slurp "day-14/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
