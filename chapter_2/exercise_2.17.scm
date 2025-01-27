#lang sicp

(define (last items)
  (cond ((null? items) '())
        ((null? (cdr items)) (car items))
        (else (last (cdr items)))))

(last (list 1 2 3))
      