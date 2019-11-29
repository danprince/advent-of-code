#lang racket

(require "part-one.rkt")

(define (remove-edge edge graph)
  (hash-remove graph edge))

(define (remove-edges graph edges)
  (foldl remove-edge graph edges))

(define (find-all-groups graph [groups (list)])
  (define edges (list->set (hash-keys graph)))
  (define first-edge (set-first edges))
  (define group (find-group graph first-edge))
  (define remaining-edges (set-subtract edges group))
  (define new-graph (remove-edges graph (set->list group)))
  (define new-groups (cons group groups))
  (if (set-empty? remaining-edges)
      new-groups
      (find-all-groups new-graph new-groups)))

(define (solve graph)
  (define groups (find-all-groups graph))
  (length groups))
