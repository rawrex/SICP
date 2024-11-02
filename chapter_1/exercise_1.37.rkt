#lang sicp

(define (cont-frac-recursive n d counter)
  (if (= counter 0)
    (/ (n counter) (d counter))
    (/ (n counter) (+ (d counter) (cont-frac-recursive n d (dec counter))))))

(define (cont-frac-iterative n d counter)
  (define (iter counter product)
    (if (= counter 0)
      product
      (iter (dec counter) (/ (n counter) (+ (d counter) product)))))
  (iter (dec counter) (/ (n counter) (d counter))))

(define (one i) 1.0)

(define phi-recursive (expt (cont-frac-recursive one one 100) -1))
(define phi-iterative (expt (cont-frac-iterative one one 100) -1))

phi-recursive
phi-iterative
