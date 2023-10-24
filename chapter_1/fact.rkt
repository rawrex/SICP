#lang sicp

(define (fac n)
  (define (iter c product)
    (cond ((> c n) product)
        (else (display product)(newline)(iter (+ 1 c) (* c product)))))
  (iter 1 1))

(fac 5)