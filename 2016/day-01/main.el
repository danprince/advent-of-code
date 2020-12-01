;;; -*- lexical-binding: t; -*-

(require 'seq)

(defun solve-part-1 (input)
  (let ((steps (parse-input input :north)))
    (manhattan-distance-from-origin
     (seq-reduce 'step-to steps '(0 0)))))

(defun solve-part-2 (input)
  (let ((steps (parse-input input :north))
        (visited nil)
        (position '(0 0)))
    (while steps
      (push position visited)
      (setq position (step-to position (pop steps)))
      (when (member position visited)
        (setq steps nil)))
    (manhattan-distance-from-origin position)))

(defun parse-input (input initial-direction)
  "Parse a string of the form R23, L2 into a list of single directional steps"
  (let ((directions)
        (direction initial-direction))
    (dolist (part (split-string input ", "))
      (let* ((turn (aref part 0))
             (steps (substring part 1 (length part)))
             (steps (string-to-number steps)))
        (setq direction (rotate-90 direction (= turn ?L)))
        (setq directions (append directions (make-list steps direction)))))
    directions))

(defun rotate-90 (direction cc)
  "Rotate a direction 90 degrees clockwise or counter-clockwise"
  (if cc
      (pcase direction
        (:north :west)
        (:west  :south)
        (:south :east)
        (:east  :north))
    (pcase direction
      (:north :east)
      (:east  :south)
      (:south :west)
      (:west  :north))))

(defun step-to (position direction)
  "Get the new position after stepping in a direction"
  (seq-let (x y) position
    (pcase direction
      (:north (list x (1- y)))
      (:east  (list (1+ x) y))
      (:south (list x (1+ y)))
      (:west  (list (1- x) y)))))

(defun manhattan-distance-from-origin (position)
  (seq-let (x y) position
    (+ (abs x) (abs y))))

(ert-deftest examples ()
  (should (equal (solve-part-1 "R2, L3") 5))
  (should (equal (solve-part-1 "R2, R2, R2") 2))
  (should (equal (solve-part-1 "R5, L5, R5, R3") 12))
  (should (equal (solve-part-2 "R8, R4, R4, R8") 4)))

(defun solve (input)
  (format "Part 1: %s\nPart 2: %s"
          (solve-part-1 input)
          (solve-part-2 input)))

(solve (with-temp-buffer
         (insert-file-contents "input.txt")
         (buffer-string)))
