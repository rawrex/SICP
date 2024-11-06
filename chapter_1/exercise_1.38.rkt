#lang sicp

(define (cont-frac-recursive n d k)
  (define (iter counter)
    (if (= counter k)
      (/ (n counter) (d counter))
      (/ (n counter) (+ (d counter) (iter (inc counter))))))
  (iter 1))

(define (cont-frac-iterative n d k)
  (define (iter counter product)
    (if (= counter 0)
      product
      (iter (dec counter) (/ (n counter) (+ (d counter) product)))))
  (iter k (/ (n 0) (d 0))))
(define (one i) 1.0)

(define (approximate-e approximator k)
  (define (D i)
    (cond ((= (remainder i 3) 0.0) (+ (* 2.0 (/ i 3.0)) 2.0))
          (else 1)))
  ;; The first two iterations are hardcoded here for implemnetation simplicity
  ;; Thus, the subtraction from the k
  (/ 1 (+ 1 (/ 1 (+ 2 (approximator one D (- k 2)))))))

(define e-recursive (+ (approximate-e cont-frac-recursive 100) 2))
(define e-iterative (+ (approximate-e cont-frac-iterative 100) 2))

e-recursive
e-iterative
