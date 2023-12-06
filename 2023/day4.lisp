(ql:quickload :cl-ppcre)

(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day4.input")))

(defparameter *example* "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defun sum (seq)
  (reduce #'+ seq))

(defun parse-integers (str)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" str)))

(defun count-matches (card-str)
  (ppcre:register-groups-bind
    ((#'parse-integers winning-numbers numbers))
    ("Card +\\d+: (.*) \\| (.*)" card-str)
    (length (intersection numbers winning-numbers))))

(defun score-points (card-str)
  (let ((matches (count-matches card-str)))
    (if (zerop matches) 0 (expt 2 (1- matches)))))

(defun part-1 (input)
  (loop for line in (split-sequence:split-sequence #\newline input)
          sum (score-points line)))

(defun part-2 (input)
  ;; Iterate through the cards in reverse to eliminate the need for a ton of
  ;; forward recursion.
  (loop with scores = '()
        for line in (reverse (split-sequence:split-sequence #\newline input))
        for matches = (count-matches line)
        for score = (1+ (sum (subseq scores 0 matches)))
        do (push score scores)
        finally (return (sum scores))))

(assert (equal (part-1 *example*) 13))
(assert (equal (part-2 *example*) 30))

(format t "Part 1: ~A~%" (part-1 *input*))
(format t "Part 2: ~A~%" (part-2 *input*))
