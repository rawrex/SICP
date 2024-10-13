#lang sicp

(define (make-accumulate combination initial)
  (define (accumulate term start end next)
    (if (> start end)
      initial
      (combination 
                 (term start) 
                 (accumulate term (next start) end next))))
  accumulate)

(define (make-accumulate-iter combination initial)
  (define (accumulate term start end next)
    (define (iter start result)
      (if (> start end)
        result
        (iter (next start) (combination result (term start)))))
    (iter start initial))
    accumulate)

(define new-sum (make-accumulate + 0.0))
(define new-mul (make-accumulate * 1.0))
(define new-sum-iter (make-accumulate-iter + 0.0))
(define new-mul-iter (make-accumulate-iter * 1.0))

;; Tests
(new-sum (lambda (x) x) 2 4 inc)
(new-mul (lambda (x) x) 2 4 inc)
(new-sum-iter (lambda (x) x) 2 4 inc)
(new-mul-iter (lambda (x) x) 2 4 inc)
