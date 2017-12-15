#lang racket

(define (parse-graph str)
  (define trimmed (string-trim str))
  (define lines (string-split trimmed "\n"))
  (define connections (map parse-connection lines))
  (foldl connect (hash) connections))

(define (connect connection graph)
  (hash-set graph (first connection) (rest connection)))

(define (parse-connection str)
  (define parts (string-split str " <-> "))
  (define node (first parts))
  (define edge-string (second parts))
  (define edges (string-split edge-string ", "))
  (cons node edges))

(define (find-group graph start [group (set)])
  (define edges (hash-ref graph start))
  (define (visit-edge edge group)
    (cond
      [(set-member? group edge) group]
      [else (find-group graph edge group)]))
  (foldl visit-edge (set-add group start) edges))

(define (solve graph)
  (define group (find-group graph "0"))
  (set-count group))

(provide parse-graph find-group)
