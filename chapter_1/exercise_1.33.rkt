#lang sicp

(define (make-accumulate-filtered combination initial predicate)
  (define (accumulate term a b next)
    (cond ((> a b) initial)
          ((predicate a) (combination (term a) (accumulate term (next a) b next)))
          (else (accumulate term (next a) b next))))
  accumulate)

(define (make-accumulate-filtered-iter combination initial predicate)
  (define (accumulate term a b next)
    (define (iter a result)
      (cond ((> a b) result)
            ((predicate a) (iter (next a) (combination result (term a))))
            (else (iter (next a) result))))
    (iter a initial))
  accumulate)

(define sum-even (make-accumulate-filtered + 0.0 even?))
(define mul-even (make-accumulate-filtered * 1.0 even?))
(define sum-even-iter (make-accumulate-filtered-iter + 0.0 even?))
(define mul-even-iter (make-accumulate-filtered-iter * 1.0 even?))

;; Tests
(sum-even (lambda (x) x) 2 4 inc)
(mul-even (lambda (x) x) 2 4 inc)
(sum-even-iter (lambda (x) x) 2 4 inc)
(mul-even-iter (lambda (x) x) 2 4 inc)
