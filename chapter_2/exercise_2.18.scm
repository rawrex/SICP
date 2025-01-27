#lang sicp

(define (reverse items)
  (cond ((null? items) '())
        ((null? (cdr items)) items)
        (else (list (reverse (cdr items)) (car items)))))

(reverse (list 1 2 3))
      