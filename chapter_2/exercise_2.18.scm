#lang sicp

(define (reverse input-items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter input-items '()))

(define (reverse-2 items)
  (define (append a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b))))
  (if (null? items)
      '()
      (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 2 3 4 5))
(reverse-2 (list 1 2 3 4 5))
      