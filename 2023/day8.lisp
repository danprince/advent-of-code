(ql:quickload :cl-ppcre)

(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day8.input")))

(defun parse-network (str)
  (loop for line in (split-sequence:split-sequence #\newline str)
        collect (ppcre:register-groups-bind
                  (node left-node right-node) ("(.+) = \\((.+), (.+)\\)" line)
                  (list node left-node right-node))))

(defun step-network (network current-node direction)
  (destructuring-bind (left right)
      (rest (assoc current-node network :test #'equal))
    (ecase direction
      (#\L left)
      (#\R right))))

(defun elt-cycle (seq index)
  (elt seq (mod index (length seq))))

(defun parse-input (input)
  (destructuring-bind (instructions network-str) (ppcre:split "\\n\\n" input)
    (values instructions (parse-network network-str))))

(defun count-steps-if (instructions network start-node end-node-p)
  (loop with node = start-node
        for step from 0
        for direction = (elt-cycle instructions step)
        until (funcall end-node-p node)
        do (setf node (step-network network node direction))
          count 1))

(defun zzz-p (node)
  (equal node "ZZZ"))

(defun part-1 (input)
  (multiple-value-bind (instructions network) (parse-input input)
    (count-steps-if instructions network "AAA" #'zzz-p)))

(defun start-node-p (node)
  (equal (elt node (1- (length node))) #\A))

(defun end-node-p (node)
  (equal (elt node (1- (length node))) #\Z))

(defun part-2 (input)
  (multiple-value-bind (instructions network) (parse-input input)
    (apply #'lcm (loop with nodes = (mapcar #'first network)
                       for node in nodes if (start-node-p node)
                       collect (count-steps-if instructions network node #'end-node-p)))))

;; Briefly tried part two by actually stepping simultaneously just in case I
;; had a lucky input, but that was still running ten seconds later. Feels like
;; a classic "lowest common multiple" problem which returns the correct answer
;; for the example and my input, but I'm not sure that it's generally correct.
;;
;; Presumably you can only use LCM if each start node visits a single end node.
;; The puzzle description doesn't explicitly clarify whether we have this
;; property (but I imagine all inputs do) it just says there are the same number
;; of end nodes as start nodes.
;;
;; Imagine the following scenario:
;;
;; 11A -> 22A -> 33Z -> 44Z
;; 11B -> 22B -> 33B -> 44Z
;;
;; It takes 3 steps before we're at the exits, but with the LCM approach the
;; first path would show it taking 2 steps, and the second path taking 3. The
;; lcm(2, 3) = 6 and whilst it's correct that the ghost would be at all Z
;; nodes on step 6, that wouldn't be the first time it was.
;;
;; So the _correct_ solution (which I am definitely not going to implement)
;; would be to find the distances to all Z nodes during the first cycle through
;; the network, then find lowest common multiple of all the permutations of
;; distance lengths for all paths.

(assert (equal (part-1 *input*) 16409))
(assert (equal (part-2 *input*) 11795205644011))
