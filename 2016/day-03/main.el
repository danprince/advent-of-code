(require 'seq)
(require 'dash)

(defun parse-side-lengths (str)
  (mapcar 'string-to-number (split-string str)))

(defun parse-input (input)
  (let* ((sanitized-input (replace-regexp-in-string "\s+" " " input)))
    (->> (split-string sanitized-input "\n")
         (-map 'string-trim)
         (-map 'parse-side-lengths)
         (-filter 'identity)))) ;; TODO: Work out why there are nil values

(defun rotate-triangles (triangles)
  (let ((rotated '()))
    (while triangles
      (let ((a (pop triangles))
            (b (pop triangles))
            (c (pop triangles)))
        (dotimes (i 3)
          (push (list (pop a) (pop b) (pop c)) rotated))))
      rotated))

(defun valid-trianglep (sides)
  (seq-let (a b c) (sort sides '>)
    (< a (+ b c))))

(defun solve-part-1 (input)
  (let* ((triangles (parse-input input))
         (valid-triangles (-filter 'valid-trianglep triangles)))
    (length valid-triangles)))

(defun solve-part-2 (input)
  (let* ((triangles (rotate-triangles (parse-input input)))
         (valid-triangles (-filter 'valid-trianglep triangles)))
    (length valid-triangles)))

(ert-deftest examples ()
  (should (not (valid-trianglep '(5 10 25))))
  (should (valid-trianglep '(1 1 1))))

(defun solve (input)
  (format "Part 1: %s\nPart 2: %s"
          (solve-part-1 input)
          (solve-part-2 input)))

(solve (with-temp-buffer
         (insert-file-contents "input.txt")
         (buffer-string)))
