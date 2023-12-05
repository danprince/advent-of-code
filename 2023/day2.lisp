(ql:quickload "cl-ppcre")

(defun parse-game-id (str)
  "Parse the numeric id from a string like 'Game 3'"
  (parse-integer (ppcre:scan-to-strings "\\d+" str)))

(defun parse-draw-result (str)
  "Parses a string like '3 red' into a list like (3 'red')"
  (let ((parts (split-sequence:split-sequence #\space str)))
    (list (parse-integer (first parts)) (second parts))))

(defun parse-draw-results (str)
  (loop for match in (ppcre:all-matches-as-strings "\\d+ (red|blue|green)" str)
        collect (parse-draw-result match)))

(defun game-possible-p (game-string &key red green blue)
  "Check whether a given game would be possible with the given numbers of cubes."
  (every
      (lambda (result)
        (destructuring-bind (count color) result
          (or (and (string= color "red") (<= count red))
              (and (string= color "green") (<= count green))
              (and (string= color "blue") (<= count blue)))))
      (parse-draw-results game-string)))

(defun find-power (game-string)
  "Find the product of the minimum number of cubes required to play this game."
  (let ((red 0) (blue 0) (green 0))
    (loop for (count color) in (parse-draw-results game-string)
          do (cond ((string= color "red") (setf red (max red count)))
                   ((string= color "green") (setf green (max green count)))
                   ((string= color "blue") (setf blue (max blue count)))))
    (* red blue green)))

(defun part-1 (input)
  "Sum the ids of the games that would have been possible"
  (loop for line in (split-sequence:split-sequence #\newline input)
          when (game-possible-p line :red 12 :green 13 :blue 14)
          sum (parse-game-id line)))

(defun part-2 (input)
  "Find the total 'power' of all games, based on the minimum number of cubes required to play."
  (loop for line in (split-sequence:split-sequence #\newline input)
          sum (find-power line)))

(defparameter *example* "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(assert (equal (part-1 *example*) 8))
(assert (equal (part-2 *example*) 2286))

(let ((input (string-trim '(#\newline) (uiop:read-file-string "2023/day2.txt"))))
  (print (part-1 input))
  (print (part-2 input)))
