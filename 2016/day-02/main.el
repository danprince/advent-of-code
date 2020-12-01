;;; -*- lexical-binding: t; -*-

(require 'seq)

(defun solve-part-1 (input)
  (let ((code nil)
        (x 1)
        (y 1))
    (seq-doseq (char input)
      (pcase char
        (?U (if (> y 0) (setq y (1- y))))
        (?L (if (> x 0) (setq x (1- x))))
        (?D (if (< y 2) (setq y (1+ y))))
        (?R (if (< x 2) (setq x (1+ x))))
        (?\n (push (read-keypad-1 x y) code))))
    (string-join (nreverse code) "")))

(defun solve-part-2 (input)
  (let* ((code nil)
         (x 0)
         (y 2))
    (seq-doseq (char input)
      (pcase char
        (?U (if (read-keypad-2 x (1- y)) (setq y (1- y))))
        (?D (if (read-keypad-2 x (1+ y)) (setq y (1+ y))))
        (?L (if (read-keypad-2 (1- x) y) (setq x (1- x))))
        (?R (if (read-keypad-2 (1+ x) y) (setq x (1+ x))))
        (?\n (push (read-keypad-2 x y) code))))
    (string-join (nreverse code) "")))

(defconst part-2-keypad
  '(nil nil "1" nil nil
    nil "2" "3" "4" nil
    "5" "6" "7" "8" "9"
    nil "A" "B" "C" nil
    nil nil "D" nil nil))

(defun read-keypad-1 (x y)
  (number-to-string (+ 1 x (* y 3))))

(defun read-keypad-2 (x y)
  (nth (+ x (* y 5)) part-2-keypad))

(ert-deftest examples ()
  (should (string= (solve-part-1 "ULL\nRRDDD\nLURDL\nUUUUD\n") "1985"))
  (should (string= (solve-part-2 "ULL\nRRDDD\nLURDL\nUUUUD\n") "5DB3")))

(defun solve (input)
  (format "Part 1: %s\nPart 2: %s"
          (solve-part-1 input)
          (solve-part-2 input)))

(solve (with-temp-buffer
         (insert-file-contents "input.txt")
         (buffer-string)))
