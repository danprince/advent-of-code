(defparameter *input* (string-trim '(#\newline) (uiop:read-file-string "2023/day7.input")))
(defparameter *example* (string-trim '(#\newline) (uiop:read-file-string "2023/day7.example")))

(defparameter *jokers* nil)
(defparameter *card-ranks* "23456789TJQKA")
(defparameter *card-ranks-with-jokers* "J23456789TQKA")
(defparameter *hand-ranks* '((1 1 1 1 1) (2 1 1 1) (2 2 1) (3 1 1) (3 2) (4 1) (5)))

(defmacro with-jokers (&body forms)
  `(let ((*jokers* t)) ,@forms))

(defun rank-card (card)
  (if *jokers*
      (position card *card-ranks-with-jokers*)
      (position card *card-ranks*)))

(defun rank-hand (hand)
  (position (type-of-hand hand) *hand-ranks* :test #'equal))

(defun type-of-hand (hand)
  (if *jokers*
      (count-cards-with-jokers hand)
      (count-cards hand)))

(defun count-cards (hand)
  (sort (remove 0 (loop for card across *card-ranks* collect (count card hand))) #'>))

(defun count-cards-with-jokers (hand)
  (let ((counts (count-cards (remove #\J hand)))
        (jokers (count #\J hand)))
    (if (= jokers 5)
        ;; Five jokers is always considered 5 of a kind.
        '(5)
        ;; Otherwise add the jokers to the count of the most common card.
        (cons (+ jokers (first counts)) (rest counts)))))

(defun compare-card-by-card (hand-a hand-b)
  "Given two hands with the same hand rank, check whether the first hand is
  stronger by comparing the individual cards."
  (loop for card-a across hand-a
        for card-b across hand-b
        for rank-a = (rank-card card-a)
        for rank-b = (rank-card card-b)
        while (= rank-a rank-b)
        finally (return (> rank-a rank-b))))

(defun compare-hands (hand-a hand-b)
  "Compare two hands to see if the first hand is stronger."
  (let ((rank-a (rank-hand hand-a))
        (rank-b (rank-hand hand-b)))
    (cond ((> rank-a rank-b) t)
          ((= rank-a rank-b) (compare-card-by-card hand-a hand-b))
          ((< rank-a rank-b) nil))))

(defun parse-rounds (input)
  (loop for line in (split-sequence:split-sequence #\newline input)
        for parts = (split-sequence:split-sequence #\space line)
        collect (list :hand (first parts) :bid (parse-integer (second parts)))))

(defun compare-rounds (round-a round-b)
  (compare-hands (getf round-a :hand)
                 (getf round-b :hand)))

(defun count-winnings (rounds)
  (let ((ranked-rounds (sort rounds #'compare-rounds)))
    (format t "~{~a~%~}" (reverse ranked-rounds))
    (loop for round in (reverse ranked-rounds)
          for rank from 1
            sum (* rank (getf round :bid)))))

(defun part-1 (input)
  (count-winnings (parse-rounds input)))

;; Part 2. Don't think you need to actually generate all the combinations of
;; hands that the jokers could create (e.g. KKQQJ could be KKQQK or KKQQQ or
;; even KKQQT etc). You might need to if there were straights or flushes, but
;; I think we can just add the J count to the highest other card count in the
;; hand.
;;
;; 1 1 1 1 J => 2 1 1 1 ("High card" becomes "One pair")
;;   2 1 1 J => 3 1 1   ("One pair" becomes "Three of a kind")
;;     2 2 J => 3 2     ("Two pair" becomes "Full house")
;;     3 1 J => 4 1     ("Three of a kind" becomes "Four of a kind")
;;       4 J => 5       ("Four of a kind" becomes "Five of a kind")
;;
;; Pretty sure the same logic holds true for multiple jokers.
;;
;; 1 1 1 J J => 3 1 1 ("High card" becomes "Three of a kind")
;;   2 1 J J => 4 1   ("One pair" becomes "Four of a kind")
;;     3 J J => 5     ("Three of a kind" becomes "Five of a kind")
;;
;; The only edge case to watch out for is a hand full of jokers (JJJJJ).
;; There won't be a "most common card" to add the jokers to, so it has
;; to be treated as a special case "Five of a kind".

(defun part-2 (input)
  (with-jokers
    (count-winnings (parse-rounds input))))

(assert (equal (part-1 *example*) 6440))
(assert (equal (part-2 *example*) 5905))

(assert (equal (part-1 *input*) 251136060))
(assert (equal (part-2 *input*) 249400220))
