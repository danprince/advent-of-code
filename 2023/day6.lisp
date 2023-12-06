(ql:quickload :cl-ppcre)

(defparameter *input* (uiop:read-file-string "2023/day6.input"))

(defparameter *example* (uiop:read-file-string "2023/day6.example"))

(defun parse-ints (str)
  "Parse all the positive integers out of a string."
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" str)))

(defun parse-sparse-int (str)
  "Parse a sparse integer (allows gaps between digits) out of a string."
  (parse-integer (remove-if-not #'digit-char-p str)))

(defun count-ways-to-win (total-time record-distance)
  "Count the number of ways you can beat the record distance with different
  amounts of time holding the charging button."
  (loop for holding-time from 0 to total-time
        for moving-time = (- total-time holding-time)
        for distance = (* holding-time moving-time)
          count (when (> distance record-distance) 1)))

(assert (equal (count-ways-to-win 7 9) 4))
(assert (equal (count-ways-to-win 15 40) 8))
(assert (equal (count-ways-to-win 30 200) 9))

(defun part-1 (input)
  (let* ((lines (split-sequence:split-sequence #\newline input))
         (times (parse-ints (first lines)))
         (distances (parse-ints (second lines))))
    (reduce #'* (loop for time in times
                      for distance in distances
                      collect (count-ways-to-win time distance)))))

(defun part-2 (input)
  (let* ((lines (split-sequence:split-sequence #\newline input))
         (time (parse-sparse-int (first lines)))
         (distance (parse-sparse-int (second lines))))
    (count-ways-to-win time distance)))

;; There's certainly a much more efficient closed form solution here for part
;; two but it turns out that Common Lisp will crunch the numbers the naive way
;; in under a second. Not going to say no to that!

(part-1 *example*)
(part-1 *input*)

(part-2 *example*)
(part-2 *input*)
