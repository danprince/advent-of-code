(ns day-07
  (:require [clojure.string :as str]))

(defn parse-ip [ip]
  (map vector
       (cycle [:supernet :hypernet])
       (str/split ip #"[\[\]]")))

(defn find-abba [s]
  (let [pattern #"([a-z])([a-z])\2\1"
        [abba a b] (re-find pattern s)]
    (when (not= a b) abba)))

(defn find-all-abas [s]
  (let [pattern #"([a-z])(?=([a-z])(\1))"
        matches (re-seq pattern s)]
    (for [[_ a b _] matches :when (not= a b)]
      (str a b a))))
(defn aba->bab [[a b _]]
  (str b a b))

(defn find-abba-in-ip [ip]
  (let [parts (parse-ip ip)
        groups (group-by first parts)
        hypernets (map second (:hypernet groups))
        supernets (map second (:supernet groups))
        hypernet-abbas (filter find-abba hypernets)
        supernet-abbas (filter find-abba supernets)]
    (when (empty? hypernet-abbas)
      (first supernet-abbas))))

(defn supports-tls? [ip]
  (some? (find-abba-in-ip ip)))

(defn supports-ssl? [ip]
  (let [parts (parse-ip ip)
        groups (group-by first parts)
        super-seqs (mapv second (:supernet groups))
        hyper-seqs (mapv second (:hypernet groups))
        super-abas (mapcat find-all-abas super-seqs)
        needs-babs (map aba->bab super-abas)]
    (first (for [bab needs-babs
                 seq hyper-seqs
                 :when (str/includes? seq bab)] bab))))

(assert (supports-tls? "abba[mnop]qrst"))
(assert (not (supports-tls? "abcd[bddb]xyyx")))
(assert (not (supports-tls? "aaaa[qwer]tyui")))
(assert (supports-tls? "ioxxoj[asdfgh]zxcvbn"))
(assert (not (supports-tls? "abba[abba]abba")))

(assert (supports-ssl? "aba[bab]xyz"))
(assert (not (supports-ssl? "xyx[xyx]xyx")))
(assert (supports-ssl? "aaa[kek]eke"))
(assert (supports-ssl? "zazbz[bzb]cdb"))

(defn part-1 [input]
  (->> (str/split-lines input)
       (filter supports-tls?)
       count))

(defn part-2 [input]
  (->> (str/split-lines input)
       (filter supports-ssl?)
       count))

(let [input (slurp "day-07/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
