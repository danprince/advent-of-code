(ql:quickload "cl-ppcre")

(defparameter *digit-map*
              '(("one" . "1")
                ("two" . "2")
                ("three" . "3")
                ("four" . "4")
                ("five" . "5")
                ("six" . "6")
                ("seven" . "7")
                ("eight" . "8")
                ("nine" . "9")))

(defparameter *digit-regex* "one|two|three|four|five|six|seven|eight|nine|0|1|2|3|4|5|6|7|8|9")

(defun parse-number-string (str)
  (or (cdr (assoc str *digit-map* :test #'string=))
      (parse-integer str)))

(defun get-calibration-value (line)
  (parse-integer (format nil "~A~A"
                   (some #'digit-char-p line)
                   (some #'digit-char-p (reverse line)))))

(defun get-artistic-calibration-value (line)
  (parse-integer
    (format nil "~A~A"
      (parse-number-string (ppcre:scan-to-strings *digit-regex* line))
      ;; yuck
      (parse-number-string
        (reverse (ppcre:scan-to-strings (reverse *digit-regex*) (reverse line)))))))

(defun part-1 (input)
  (loop for line in (split-sequence:split-sequence #\newline input)
          sum (get-calibration-value line)))

(defun part-2 (input)
  (loop for line in (split-sequence:split-sequence #\newline input)
          sum (get-artistic-calibration-value line)))

(assert (equal (get-calibration-value "1abc2") 12))
(assert (equal (get-calibration-value "pqr3stu8vwx") 38))
(assert (equal (get-calibration-value "a1b2c3d4e5f") 15))
(assert (equal (get-calibration-value "treb7uchet") 77))

(assert (equal (get-artistic-calibration-value "two1nine") 29))
(assert (equal (get-artistic-calibration-value "eightwothree") 83))
(assert (equal (get-artistic-calibration-value "xtwone3four") 24))
(assert (equal (get-artistic-calibration-value "4nineeightseven2") 42))
(assert (equal (get-artistic-calibration-value "zoneight234") 14))
(assert (equal (get-artistic-calibration-value "7pqrstsixteen") 76))
(assert (equal (get-artistic-calibration-value "eightwo") 82))

(let ((input (string-trim '(#\newline) (uiop:read-file-string "2023/day1.input"))))
  (print (part-1 input))
  (print (part-2 input)))
