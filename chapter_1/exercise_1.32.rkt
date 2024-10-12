#lang sicp

;; Previous definitions

(define (sum term a next b)
  (if (> a b)
      0.0
      (+ (term a) (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0.0))

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


;; Accumulate

(define (make-accumulate combination initial)
  (define (accumulate term start end next)
    (if (> start end)
      initial
      (combination 
                 (term start) 
                 (accumulate term (next start) end next))))
  accumulate)

(define new-sum (make-accumulate + 0.0))
(define new-mul (make-accumulate * 1.0))

(new-sum (lambda (x) (* x x)) 2 5 inc)
(new-mul (lambda (x) (x)) 2 5 inc)
