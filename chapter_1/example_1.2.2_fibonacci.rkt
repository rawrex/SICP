#lang sicp

(define (fib-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2))))))

(define (fib-iterative n)
  (define (iter a b counter)
    (if (= counter n)
        a
        (iter (+ a b) a (inc counter))))
  (iter 0 1 0))
  

(fib-recursive 5)
(fib-recursive 25)
(fib-iterative 5)
(fib-iterative 25)