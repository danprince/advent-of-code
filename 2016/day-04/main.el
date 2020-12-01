(require 'seq)

(defun solve-part-1 (input) 0)
(defun solve-part-2 (input) 0)

(defun parse-input (input)
  (let ((lines (split-string input "\n"))
        (rooms '()))
    (seq-doseq (line lines)
      (let ((checksum (substring line -6 -1))
            (sector (substring line -10 -7))
            (data (substring line 0 -11)))
        (push `(,data ,sector ,checksum) rooms)))
    rooms))

(defun validate-checksum (room)
  (let ((frequencies nil)
        (data (car room))
        (checksum (nth 2 room)))
    (seq-doseq (char data)
      (when (/= char ?-)
        (let* ((count (or 0 (car (assoc char frequencies))))
               (count (1+ count)))
          (message "char: %d count: %d" char count)
          (add-to-list 'frequencies (list count char)))))
    frequencies))

(validate-checksum (car (parse-input "aaaaa-bbb-z-y-x-123[abxyz]")))

(ert-deftest examples ()
  (should (validate-checksum (car (parse-input "aaaaa-bbb-z-y-x-123[abxyz]"))))
  (should (validate-checksum (car (parse-input "a-b-c-d-e-f-g-h-987[abcde]"))))
  (should (not (validate-checksum (car (parse-input "not-a-real-room-404[oarel]")))))
  (should (not (validate-checksum (car (parse-input "totally-real-room-200[decoy]"))))))

(defun solve (input)
  (format "Part 1: %d\nPart 2: %d"
          (solve-part-1 input)
          (solve-part-2 input)))

(solve (with-temp-buffer
         (insert-file-contents "input.txt")
         (buffer-string)))
