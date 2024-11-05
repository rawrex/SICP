#lang sicp

(define (cont-frac-recursive n d k)
  (define (iter counter)
    (if (= counter k)
      (/ (n counter) (d counter))
      (/ (n counter) (+ (d counter) (iter (inc counter))))))
  (iter 1))

(define (cont-frac-iterative n d k)
  (define (iter counter product)
    (if (= counter k)
      product
      (iter (inc counter) (/ (n counter) (+ (d counter) product)))))
  (iter 1 0))

(define (tan-cf x k approximator)
  (define (next-odd i)
    (+ (* i 2) 1))
  (let ((square-x (* x x)))
    (/ x (- 1 (approximator square-x next-odd k)))))

(define (tan-iterative x k)
   (tan-cf x k cont-frac-iterative))

(define (tan-recursive x k)
   (tan-cf x k cont-frac-recursive))

(tan-iterative 10 100)
(tan-recursive 10 100)