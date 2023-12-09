(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day9.input")))
(defparameter *example* (string-trim '(#\newline) (uiop:read-file-string "2023/day9.example")))

(defun parse-histories (input)
  (loop for line in (split-sequence:split-sequence #\newline input)
        collect (mapcar #'parse-integer (split-sequence:split-sequence #\space line))))

(defun find-differences (numbers)
  (loop for ns on numbers
        while (second ns)
        collect (- (second ns) (first ns))))

(defun find-next-term (numbers)
  (let ((differences (find-differences numbers)))
    (if (every #'zerop differences)
        (first numbers)
        (+ (car (last numbers)) (find-next-term differences)))))

(defun find-previous-term (numbers)
  (let ((differences (find-differences numbers)))
    (if (every #'zerop differences)
        (first numbers)
        (- (first numbers) (find-previous-term differences)))))

(defun part-1 (input)
  (loop for history in (parse-histories input)
          sum (find-next-term history)))

(defun part-2 (input)
  (loop for history in (parse-histories input)
          sum (find-previous-term history)))

(assert (equal (part-1 *example*) 114))
(assert (equal (part-2 *example*) 2))
(assert (equal (part-1 *input*) 1987402313))
(assert (equal (part-2 *input*) 900))
