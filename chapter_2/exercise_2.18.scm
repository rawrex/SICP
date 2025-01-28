#lang sicp

(define (reverse items)
  (define (append a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b))))
  (if (null? items)
      '()
      (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 2 3 4 5))
      