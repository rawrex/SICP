#lang sicp

;; Copied from the exercise 1.16
;; As a mean of visual example
(define (square x) (* x x))

(define (fast-exponent-recursive b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exponent-recursive b (/ n 2))))
        (else (* b (fast-exponent-recursive b (- n 1))))))

(define (fast-exponent-iterative b n)
  (define (iter b n accumulator)
    (cond ((= n 0) accumulator)
          ((even? n) (iter (square b) (/ n 2) accumulator))
          (else (iter b (- n 1) (* b accumulator)))))
  (iter b n 1))


;; Our utility functions
(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2.0))


(define (fast-multiply-recursive x y)
  (cond ((= y 0) 0)
        ((even? y) (double (fast-multiply-recursive x (halve y))))
        (else (+ x (fast-multiply-recursive x (dec y))))))

(define (fast-multiply-iterative x y)
  (define (iter x y accumulator)
    (cond ((= y 0) accumulator)
          ((even? y) (iter (double x) (halve y) accumulator))
          (else (iter x (dec y) (+ accumulator x)))))
  (iter x y 0))

;; Test the recursive
(fast-multiply-recursive 3 7)
(fast-multiply-recursive 3 8)

;; Test the iterative
(fast-multiply-iterative 3 7)
(fast-multiply-iterative 3 8)