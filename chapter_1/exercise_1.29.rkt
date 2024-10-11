#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (new-integral f a b n)
  ;; Subinterval width
  (define h (/ (- b a) n))
  ;; The rule for each step
  ;; Note the weights 1, 2, 4
  (define (term x)
    (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h)))))
  ;; Advances the current point
  (define (next x)
    (+ x (* 2 h)))
  (* (/ h 3.0) (sum term a next (- b (* 2 h)))))

(define (cube x)
  (* x x x))

(define accuracy 10)

(new-integral cube 1 10 accuracy)
