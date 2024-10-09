#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (old-integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (self x ) x)

(define (new-integral f a b n)
  (define h (/ (- b a) n))
  (define (y_k k)
    (f (+ a (* k h))))
  (* (/ h 3.0)
     (+
        (y_k 0)
        (* 4 (sum self (y_k (dec n)) ...))
        (* 2 (sum self (y_k (dec n)) ...))
        (y_k n))))


(define (cube x)
  (* x x x))

(define accuracy 10)

(new-integral cube 1 10 accuracy)
