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

(define accuracy 3)

(define (new-integral f a b n)

  (define (h a b n)
    (/ (- b a) n))

  (define (y_k k)
    (f (+ a (* k (h a b n)))))

  (* (sum f (y_k (inc a)) y_k b)
     (/ (h a b n) 3.0)))

(new-integral inc 0 10 accuracy)
