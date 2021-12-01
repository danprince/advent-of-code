(ns day-08
  (:require [clojure.string :as str]
            [clojure.set :refer [union]]))

(def pattern-rect #"rect (\d+)x(\d+)")
(def pattern-rotate #"rotate (column|row) [xy]=(\d+) by (\d+)")

(defn parse-cmd [s]
  (or
   (when-let [[_ x y] (re-matches pattern-rect s)]
     [:rect (Integer/parseInt x) (Integer/parseInt y)])

   (when-let [[_ dir idx shift] (re-matches pattern-rotate s)]
     [:rotate (keyword dir) (Integer/parseInt idx) (Integer/parseInt shift)])))

(defn parse-cmds [input]
  (->> input str/split-lines (map parse-cmd)))

(defn pixel-display
  ([] (pixel-display 50 6))
  ([w h] {:width w, :height h, :pixels #{}}))

(defn draw-rect [pixels w h]
  (union pixels (set (for [x (range w),
                           y (range h)]
                       [x y]))))

(defn shift-col [pixels col shift height]
  (set (for [[x y] pixels]
         (if (= x col)
           [x (mod (+ y shift) height)]
           [x y]))))

(defn shift-row [pixels row shift width]
  (set (for [[x y] pixels]
         (if (= y row)
           [(mod (+ x shift) width) y]
           [x y]))))

(defmulti exec
  (fn [_display cmd] (first cmd)))

(defmethod exec :rect [display [_ w h]]
  (update display :pixels #(draw-rect % w h)))

(defmethod exec :rotate
  [{pixels :pixels, w :width, h :height, :as display}
   [_type dir idx shift]]
  (assoc
   display
   :pixels
   (case dir
     :column (shift-col pixels idx shift h)
     :row (shift-row pixels idx shift w))))

(defn display->string [{:keys [pixels width height]}]
  (doseq [y (range height)]
    (doseq [x (range width)]
      (if (pixels [x y])
        (print \#)
        (print \space)))
    (print \newline))
  (flush))

(defn part-1 [input]
  (->> input
       parse-cmds
       (reduce exec (pixel-display))
       :pixels
       count))

(defn part-2 [input]
  (->> input
       parse-cmds
       (reduce exec (pixel-display))
       display->string))

(let [input (slurp "day-08/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:")
  (println (part-2 input)))
