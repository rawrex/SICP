#lang sicp

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car pair)
  (define (iter n counter)
    (if (= (remainder n 2) 0)
        (iter (/ n 2) (inc counter))
        counter))
  (iter pair 0))

(define (cdr pair)
  (define (iter n counter)
    (if (= (remainder n 3) 0)
        (iter (/ n 3) (inc counter))
        counter))
  (iter pair 0))

(car (cons 23 37))
(cdr (cons 23 37))