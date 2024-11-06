#lang sicp

(define (cont-frac-recursive n d counter)
  (if (= counter 0)
    (/ (n counter) (d counter))
    (/ (n counter) (+ (d counter) (cont-frac-recursive n d (dec counter))))))

(define (cont-frac-iterative n d k)
  (define (iter counter product)
    (if (= counter 0)
      product
      (iter (dec counter) (/ (n counter) (+ (d counter) product)))))
  (iter k (/ (n 0) (d 0))))

(define (one i) 1.0)

(define phi-recursive (expt (cont-frac-recursive one one 100) -1))
(define phi-iterative (expt (cont-frac-iterative one one 100) -1))

phi-recursive
phi-iterative
