#lang racket

(define (parse-graph str)
  (define trimmed (string-trim str))
  (define lines (string-split trimmed "\n"))
  (define connections (map parse-connection lines))
  (foldl connect (hash) connections))

(define (connect connection graph)
  (hash-set graph (car connection) (cdr connection)))

(define (parse-connection str)
  (define parts (string-split str " <-> "))
  (match-let ([(list node edge-string) parts])
    (cons node (string-split edge-string ", "))))

(define (find-group graph start [visited (set)])
  (define edges (hash-ref graph start))
  (define (visit-edge edge visited)
    (if (set-member? visited edge)
        visited
        (find-group graph edge visited)))
  (foldl visit-edge (set-add visited start) edges))

(define (solve graph)
  (define group (find-group graph "0"))
  (set-count group))

(provide parse-graph find-group)
