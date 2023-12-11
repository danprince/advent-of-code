(defpackage :aoc/2023/day10 (:use :cl))
(in-package :aoc/2023/day10)

(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day10.input")))
(defparameter *pipe-chars* "|-LJ7F")
(defparameter *directions* '(:north :east :south :west))

(defun elt-safe (seq index)
  "Like elt, but returns nil if index is out of bounds."
  (when (and (>= index 0) (< index (length seq)))
        (elt seq index)))

(defun transition (input pos dir)
  "Given a position and a direction to transition, return the new position."
  (let ((columns (1+ (position #\newline input))))
    (ecase dir
      (:north (- pos columns))
      (:south (+ pos columns))
      (:east (+ pos 1))
      (:west (- pos 1)))))

(defun valid-transition-p (dir source-char target-char)
  "Check whether a given transition is valid (e.g. the source pipe actually
  connects into the target pipe)."
  (ecase dir
    (:north (and (find source-char "|LJ") (find target-char "|F7")))
    (:south (and (find source-char "|F7") (find target-char "|LJ")))
    (:east (and (find source-char "-FL") (find target-char "-J7")))
    (:west (and (find source-char "-J7") (find target-char "-FL")))))

(defun find-next-pos (input current-pos previous-pos)
  "Given the current position and the previous position in the input, figure
  out where the next position is, after following the pipe."
  (loop for dir in *directions*
        for next-pos = (transition input current-pos dir)
        for current-char = (elt-safe input current-pos)
        for next-char = (elt-safe input next-pos)
          unless (equal next-pos previous-pos) ; never go back to the previous tile
          when (valid-transition-p dir current-char next-char)
        do (return next-pos)))

(defun get-loop-path (input start)
  "Given a starting position, try to find a loop. Returns nil if there is no
  loop and returns a list of positions which represent each visited cell if
  there is a loop."
  (loop with pos = start
        with prev = start
        for next = (find-next-pos input pos prev)
        while next ; Some tiles may have no legal transitions.
        do (rotatef prev pos next)
        collect pos
        until (equal pos start)))

(defun find-loop (input)
  "Try different start characters until we find one that creates a loop.
  Returns the positions visited by the loop and the version of the input with
  'S' swapped for the correct character."
  (loop with start = (position #\S input)
        for pipe-char across *pipe-chars*
        for possible-input = (substitute pipe-char #\S input)
        for path = (get-loop-path possible-input start)
          when path
        do (return (values path possible-input))))

(defun part-1 (input)
  (ceiling (/ (length (find-loop input)) 2)))

(defun part-2 (input)
  (multiple-value-bind (path grid) (find-loop input)
    (format t "~a~%~A~%" path grid)
    (loop with inside = nil
          for ch across grid
          for index from 0
          for on-path = (member index path)
          for will-enter = (and on-path (not inside) (find ch "|7J"))
          for will-exit = (and on-path inside (find ch "|FL"))
            when (and inside (not on-path)) count 1
            when (and inside (not on-path)) do (print (list ch index))
            when will-enter do (setq inside t)
            when will-exit do (setq inside nil))))

(part-2 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(defparameter *example-1* (string-trim '(#\newline) "
-L|F7
7S-7|
L|7||
-L-J|
L|-JF"))

(defparameter *example-2* (string-trim '(#\newline) "
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"))

(assert (equal (part-1 *example-1*) 4))
(assert (equal (part-1 *example-2*) 8))
(assert (equal (part-1 *input*) 6717))
