(ns advent-of-code.day-09)

(def marker-pattern #"(.*?)\((\d+)x(\d+)\)(.*)")

(defn decompress [input recurse]
  (if-let [[_ before n m after] (re-matches marker-pattern input)]
    (let [len (Integer/parseInt n)
          times (Integer/parseInt m)
          data (subs after 0 len)
          rest (subs after len)]
      (+ (count before)
         (* (recurse data) times)
         (decompress rest recurse)))
    (count input)))

(defn recursive-length [input]
  (decompress input recursive-length))

(defn part-1 [input]
  (decompress input count))

(defn part-2 [input]
  (decompress input recursive-length))

(assert (= (part-1 "ADVENT") (count "ADVENT")))
(assert (= (part-1 "A(1x5)BC") (count "ABBBBBC")))
(assert (= (part-1 "(3x3)XYZ") (count "XYZXYZXYZ")))
(assert (= (part-1 "A(2x2)BCD(2x2)EFG") (count "ABCBCDEFEFG")))
(assert (= (part-1 "(6x1)(1x3)A") (count "(1x3)A")))
(assert (= (part-1 "X(8x2)(3x3)ABCY") (count "X(3x3)ABC(3x3)ABCY")))

(assert (= (part-2 "(3x3)XYZ") (count "XYZXYZXYZ")))
(assert (= (part-2 "X(8x2)(3x3)ABCY") (count "XABCABCABCABCABCABCY")))
(assert (= (part-2 "(27x12)(20x12)(13x14)(7x10)(1x12)A") 241920))
(assert (= (part-2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") 445))

(let [input (slurp "day-09/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
