(ql:quickload "cl-ppcre")

(defun engine-symbol-p (char)
  (and (not (digit-char-p char))
       (not (equal char #\newline))
       (not (equal char #\.))))

(defun gear-symbol-p (char)
  (equal char #\*))

(defun subseq-safe (seq start end)
  "Like subseq but ensures that start and end are not out of bounds."
  (let* ((last-index (- (length seq) 1))
         (start-index (min (max start 0) last-index))
         (end-index (min (max end 0) last-index)))
    (subseq seq start-index end-index)))

(defun count-columns (input)
  "Count the number of columns in the input, assuming it is a consistent width rectangle."
  (1+ (position #\newline input)))

(defun get-surrounding-chars (input start end)
  "Get the list of characters that surround a range within the input."
  (let* ((columns (count-columns input))
         (str-above (subseq-safe input (- start columns 1) (- end columns -1)))
         (str-left (subseq-safe input (- start 1) start))
         (str-right (subseq-safe input end (+ end 1)))
         (str-below (subseq-safe input (+ start columns -1) (+ end columns 1))))
    (concatenate 'list str-above str-left str-right str-below)))

(defun parse-numbers-in-range (input start end)
  "Parse the list of numbers that overlap a range within the input."

  ;; Expand the start/end indexes to entirely contain all overlapping numbers.
  (loop while (and (>= start 0) (< start (length input)) (digit-char-p (elt input start)))
        do (decf start))
  (loop while (and (>= end 0) (< end (length input)) (digit-char-p (elt input end)))
        do (incf end))

  ;; Now we've adjusted the indexes, read the string then split by non-digit characters
  ;; and parse each integer we find.
  (loop for part in (ppcre:all-matches-as-strings "\\d+" (subseq-safe input start end))
          when (> (length part) 0)
        collect (parse-integer part)))

(defun get-adjacent-numbers (input index)
  "Parse all numbers adjacent to the index in question."
  (let* ((columns (count-columns input))
         (top-left-index (- index columns 1))
         (top-right-index (- index columns -1))
         (left-index (- index 1))
         (right-index (+ index 1))
         (bottom-left-index (+ index columns -1))
         (bottom-right-index (+ index columns 1)))
    (concatenate 'list
      (parse-numbers-in-range input top-left-index top-right-index)
      (parse-numbers-in-range input bottom-left-index bottom-right-index)
      (parse-numbers-in-range input left-index left-index)
      (parse-numbers-in-range input right-index right-index))))

(defun part-1 (input)
  "Find the sum of all numbers adjacent to engine symbols."
  (loop for (start end) on (ppcre:all-matches "\\d+" input) by #'cddr
          when (some #'engine-symbol-p (get-surrounding-chars input start end))
          sum (parse-integer input :start start :end end)))

(defun part-2 (input)
  "Find the sum of the product of all gear number pairs."
  (loop for ch across input
        for index from 0
        for numbers = (when (gear-symbol-p ch) (get-adjacent-numbers input index))
          sum (if (= (length numbers) 2)
                  (apply #'* numbers)
                  0)))

(defparameter *example* (string-trim '(#\newline) "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."))

(assert (equal (part-1 *example*) 4361))
(assert (equal (part-2 *example*) 467835))

(defparameter *input* (uiop:read-file-string "2023/day3.txt"))
(format t "Part 1: ~A~%" (part-1 *input*))
(format t "Part 2: ~A~%" (part-2 *input*))
