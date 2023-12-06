(ql:quickload :cl-ppcre)

(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day5.input")))

(defparameter *example* (string-trim '(#\newline) (uiop:read-file-string "2023/day5.example")))

(defun parse-integers (str)
  (mapcar #'parse-integer (split-sequence:split-sequence #\space str)))

(defun parse-map (str)
  (mapcar #'parse-integers (split-sequence:split-sequence #\newline str)))

(defun parse-map-block (str)
  (parse-map (subseq str (1+ (position #\newline str)))))

(defun lookup (map value)
  (loop for (dst src len) in map
          when (and (>= value src) (< value (+ src len)))
        do (return (+ dst (- value src)))
        finally (return value)))

(assert (equal (lookup (parse-map "50 98 2") 49) 49))
(assert (equal (lookup (parse-map "50 98 2") 98) 50))
(assert (equal (lookup (parse-map "50 98 2") 99) 51))
(assert (equal (lookup (parse-map "50 98 2") 100) 100))

(defun part-1 (input)
  (let* ((blocks (ppcre:split "\\n\\n" input))
         (map-blocks (cdr blocks))
         (maps (mapcar #'parse-map-block map-blocks))
         (seeds-block (first blocks))
         (seeds (parse-integers (subseq seeds-block (length "seeds: ")))))
    ;; Technically these maps are already ordered so that we could just thread
    ;; the seed values through them all in order with reduce, but it makes the
    ;; code a lot less explicit.
    (destructuring-bind (seed-to-soil
                         soil-to-fertilizer
                         fertilizer-to-water
                         water-to-light
                         light-to-temperature
                         temperature-to-humidity
                         humidity-to-location) maps
      (loop for seed in seeds
            for soil = (lookup seed-to-soil seed)
            for fertilizer = (lookup soil-to-fertilizer soil)
            for water = (lookup fertilizer-to-water fertilizer)
            for light = (lookup water-to-light water)
            for temperature = (lookup light-to-temperature light)
            for humidity = (lookup temperature-to-humidity temperature)
            for location = (lookup humidity-to-location humidity)
              minimize location))))

;; Part 2 seems really tricky for day 5!
;;
;; The naive approach of actually generating the ranges of seeds and running
;; them all is likely to be prohibitively expensive (hundreds of millions of
;; seeds).
;;
;; Initially wondered about just passing the min/max of each seed range but
;; presumably middle values could map to a separate range that eventually
;; maps through to a lower location.
;;
;; Next idea would be to actually work backwards from the location map
;; to try and figure out whether any of the initial seeds would map into the
;; lowest value range, then the next lowest and so on. That seems tricky
;; because you also need to know whether there are any unmapped values that
;; could end up being lower.
;;
;; Noticing that in most of the maps in my input, there's a 0 source value
;; and a 0 destination value. Might be able to use these as a shortcut
;; through the mappings.
;;
;; For example, "humidity-to-location" has the following mappings:
;;
;;   dst       src       len
;;   857589555 0         114197007
;;   0         114197007 857589555
;;
;; This would mean the lowest possible location would be 0 and that could
;; only come from a humidity of 114197007. A humidity of 0 would use the
;; other mapping and come out as a location of 857589555.
;;
;; Doesn't seem like an accident that these ranges share the same three numbers
;; but I can't infer whether that actually gives it a useful property. It's
;; worth noting that it's only the final map that has this phenomena.
;;
;; It feels tempting to just work backwards from the mapping that produces
;; the smallest values, but if the mapped ranges overlap then the highest
;; mapped value from that range may be larger than the smallest mapped value
;; from the next range.
;;
;; 🤷

(defun part-2 (input)
  input)

(part-1 *example*)
(part-1 *input*)
