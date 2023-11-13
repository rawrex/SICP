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


;; The original straightforward implementation
(define (slow-multiply x y)
  (if (= y 0)
      0
      (+ x (slow-multiply x (dec y)))))

;; Our utility functions
(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2.0))


;; Fast multiplicaiton using the same reasoning as in the exercise 1.16
(define (fast-multiply x y)
  (cond ((= y 0) 0)
        ((even? y) (double (fast-multiply x (halve y))))
        (else (+ x (fast-multiply x (dec y))))))

(slow-multiply 3 7)
(fast-multiply 3 7)