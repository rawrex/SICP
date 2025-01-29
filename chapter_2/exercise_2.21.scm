#lang sicp

(define (square x)
  (* x x))

(define (square-list-a items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map square items))

(square-list-a (list 1 2 3 4 5))
(square-list-b (list 1 2 3 4 5))