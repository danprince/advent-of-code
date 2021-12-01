(ns day-12
  (:require [clojure.string :as str]))

(def op-table {"cpy" :cpy "inc" :inc "dec" :dec "jnz" :jnz})
(def reg-table {"a" :a "b" :b "c" :c "d" :d})

(defn parse-op [s]
  (assert (op-table s) (format "Invalid opcode: %s" s))
  (op-table s))

(defn parse-reg [s]
  (assert (reg-table s) (format "Invalid register: %s" s))
  (reg-table s))

(defn parse-val [s]
  (Integer/parseInt s))

(defn parse-reg-or-val [s]
  (try (parse-val s)
    (catch Exception _ (parse-reg s))))

(defn parse-ins [s]
  (let [[oc x y] (str/split s #" ")]
    (case (parse-op oc)
      :inc [:inc (parse-reg x)]
      :dec [:dec (parse-reg x)]
      :jnz [:jnz (parse-reg-or-val x) (parse-val y)]
      :cpy [:cpy (parse-reg-or-val x) (parse-reg-or-val y)])))

(defn parse-prog [s]
  (->> s str/split-lines (mapv parse-ins)))

(defn init [s]
  {:ip 0
   :program (parse-prog s)
   :registers {:a 0 :b 0 :c 0 :d 0}
   :cycles 0})

(defn halted? [{:keys [ip program]}]
  (>= ip (count program)))

(defn val-or-reg [{:keys [registers]} s]
  (or (registers s) s))

(defn exec [{:keys [ip program] :as vm}]
  (let [[op x y] (nth program ip)
        vm (update vm :ip inc)
        vm (update vm :cycles inc)]
    (case op
      :cpy (assoc-in vm [:registers y] (val-or-reg vm x))
      :inc (update-in vm [:registers x] inc)
      :dec (update-in vm [:registers x] dec)
      :jnz (if-not (zero? (val-or-reg vm x))
              (assoc vm :ip (+ ip y))
              vm))))

(defn debug [{:keys [registers program ip cycles]}]
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
  (printf "ip: %03d                       cycles: %03d\n\n" ip cycles)
  (printf "a: %05d   b: %05d   c: %05d   d: %05d\n\n" (:a registers) (:b registers) (:c registers) (:d registers))
  (doseq [i (range (count program))]
    (printf "%2d %s  %-13s %s\n" i
            (if (= i ip) ">>>" "   ")
            (nth program i)
            (if (= i ip) "<<<" "   ")))
  (flush))

(defn exec-with-debug [vm mode]
  (loop [vm vm]
    (debug vm)
    (case mode
      :auto (Thread/sleep 200)
      :step (do (printf "\nPress enter to continue...") (.read *in*)))
    (if (not (halted? vm))
      (recur (exec vm))
      vm)))

(defn exec-until-halted [vm]
  (loop [vm vm]
    (if (not (halted? vm))
      (recur (exec vm))
      vm)))

(defn part-1 [input]
  (-> (init input)
      (exec-with-debug :auto)
      (get-in [:registers :a])))

(defn part-2 [input]
  (-> (init input)
      (assoc-in [:registers :c] 1)
      exec-until-halted
      (get-in [:registers :a])))

(let [input (slurp "day-12/input.txt")]
  (time (println "Part 1:" (part-1 input)))
  (time (println "Part 2:" (part-2 input))))
