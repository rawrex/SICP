#lang sicp

;; Multiplicators
(define (mul term a b next)
  (if (> a b)
    1.0
    (* (term a) (mul term (next a) b next))))

(define (mul-iter term a b next)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1.0))


;; Factorials
(define (fact n)
  (mul (lambda (x) x) 1.0 n inc))

(define (fact-iter n)
  (mul-iter (lambda (x) x) 1.0 n inc))


;; Pi estimation Wallis formula
(define (pi-wallis mul)
  (define accuracy 20)
  (define (even-term n)
    (* (* 2 n) (* 2 n)))
  (define (odd-term n)
    (* (- (* 2 n) 1) (+ (* 2 n) 1)))
  (* 2 (/ (mul even-term 1 accuracy inc)
     (mul odd-term 1 accuracy inc))))


;; Usage
(fact 3)
(fact-iter 3)
(pi-wallis mul)
(pi-wallis mul-iter)
