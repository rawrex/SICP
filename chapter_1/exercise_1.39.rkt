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

(define (tan-cf x k approximator)
  (define (next-odd i)
    (+ (* i 2) 1))
  (let ((square-x (* x x)))
    (/ x (- 1 (approximator (lambda (i) square-x) next-odd (- k 1))))))

(define (tan-recursive x k)
   (tan-cf x k cont-frac-recursive))

(define (tan-iterative x k)
   (tan-cf x k cont-frac-iterative))

(tan-recursive 1.0 100)
(tan-iterative 1.0 100)
