#lang sicp

(define (increment x)
  (+ x 1))

(define (decrement x)
  (- x 1))

(define (sum-recursive a b)
  (if (= a 0)
      b
      (increment (sum-recursive (decrement a) b))))

(define (sum-iter a b)
  (if (= a 0)
      b
      (sum-iter (decrement a) (increment b))))

;; The recursive sum...
(sum-recursive 4 5)
;; ...will have the following form
;; (increment (increment (increment (increment 5)))) ;; -> 9

;; The iterative sum...
(sum-iter 4 5)
;; ...will have the following form
;; (sum-iter 3 6)
;; (sum-iter 2 7)
;; (sum-iter 1 8)
;; (sum-iter 0 9) ;; -> 9