#lang sicp

(define (exponent-recursive b n)
  (if (= n 0)
      1
      (* b (exponent-recursive b (- n 1)))))

(define (exponent-iterative b n)
  (define (iter n accumulator)
    (if (= n 0)
        accumulator
        (iter (- n 1) (* b accumulator))))
  (iter n 1))

(define (square x) (* x x))

(define (fast-exponent-recursive b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exponent-recursive b (/ n 2))))
        (else (* b (fast-exponent-recursive b (- n 1))))))

(exponent-recursive 2 32)
(exponent-iterative 2 32)
(fast-exponent-recursive 2 32)