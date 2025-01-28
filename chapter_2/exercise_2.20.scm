#lang sicp

(define (same-parity f . r)
  (define (same? x y)
    (= (remainder x 2) (remainder y 2)))
  (define (helper first rest)
    (cond ((null? rest)
           (list first))
          ((same? first (car rest))
           (cons first (helper (car rest) (cdr rest))))
          (else
           (helper first (cdr rest)))))
  (helper f r))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)