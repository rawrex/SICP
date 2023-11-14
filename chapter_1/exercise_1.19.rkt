#lang sicp

; The clearest and simplest solution is present at:
; https://sicp-solutions.net/post/sicp-solution-exercise-1-19/
; Here I will not rewrite the whole reasoning.
;
; To get to the solution, we expand the Tpq(Tpq(a,b)) and refactor it,
; To get the p' and the q'.
;
; The Tpq(a,b) = (bq + aq + ap, bp + aq)
; Then we can go about Tpq(Tpq(a, b))
;
; Which will get us to the:
; p' = p^2 + q^2
; q' = 2pq + q^2


; Utility functions
(define (square x) (* x x))

(define (halve x) (/ x 2.0))

; Main function definition
(define (fib n)
  (define (p-dash p q) (+ (square p) (square q)))
  (define (q-dash p q) (+ (* 2 p q) (square q)))
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter a b (p-dash p q) (q-dash p q) (halve count)))
          (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (dec count)))))
  (fib-iter 1 0 0 1 n))

; Let's test
(fib 5)
(fib 55)
(fib 555)