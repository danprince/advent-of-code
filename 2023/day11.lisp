(defpackage :aoc/2023/day11 (:use :cl))
(in-package :aoc/2023/day11)

(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day11.input")))
(defparameter *example* (string-trim '(#\newline) (uiop:read-file-string "2023/day11.example")))

(defun range (n)
  (loop for i from 0 below n collect i))

(defun get-distance (p1 p2)
  (destructuring-bind ((x1 y1) (x2 y2)) (list p1 p2)
    (+ (abs (- y2 y1))
       (abs (- x2 x1)))))

(defun get-all-pairs (seq)
  (loop for l on seq
        append (loop for b in (rest l)
        collect (list (car l) b))))

(defun find-galaxies (input)
  (loop with cols = (1+ (position #\newline input))
        for ch across input
        for index from 0
        for x = (mod index cols)
        for y = (floor (/ index cols))
          when (equal ch #\#)
        collect (list x y)))

(defun expand-galaxies (input)
  (let* ((cols (position #\newline input))
         (rows (count #\newline input))
         (galaxies (find-galaxies input))
         (empty-xs (set-difference (range cols) (mapcar #'first galaxies)))
         (empty-ys (set-difference (range rows) (mapcar #'second galaxies))))
    (loop for (gx gy) in galaxies
          for dx = (count-if (lambda (x) (< x gx)) empty-xs)
          for dy = (count-if (lambda (y) (< y gy)) empty-ys)
          collect (list (+ gx dx)
                        (+ gy dy)))))

(defun part-1 (input)
  (loop with galaxies = (expand-galaxies input)
        for (g1 g2) in (get-all-pairs galaxies)
        sum (get-distance g1 g2)))

(assert (equal (part-1 *example*) 374))

(part-1 *input*)
