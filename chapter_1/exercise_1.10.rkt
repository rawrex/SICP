#lang sicp

(define (Ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Ackermann (- x 1)
                         (Ackermann x (- y 1))))))

;; 2n
(define (f n) (Ackermann 0 n))
(f 3) ;; -> 6

;;2^n
(define (g n) (Ackermann 1 n))
(g 3) ;; -> 8 

;;2^2^2... n times
(define (h n) (Ackermann 2 n))
(h 3) ;; -> 16