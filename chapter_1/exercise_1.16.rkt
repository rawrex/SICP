#lang sicp

(define (square x) (* x x))

(define (fast-exponent-recursive b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exponent-recursive b (/ n 2))))
        (else (* b (fast-exponent-recursive b (- n 1))))))

(define (print-invariant b n accumulator)
  (display "b: ")(display b)(newline)
  (display "n: ")(display n)(newline)
  (display "a: ")(display accumulator)(newline)
  (display "a*b^n: ")(display (* accumulator (expt b n)))(newline)(newline))

(define (fast-exponent-iterative b n)
  (define (iter b n accumulator)
    (cond ((= n 0) accumulator)
          ((even? n)
           (print-invariant b n accumulator)
           (iter (square b) (/ n 2) accumulator))
          (else
           (print-invariant b n accumulator)
           (iter b (- n 1) (* b accumulator)))))
  (iter b n 1))

(fast-exponent-recursive 2 32)
(fast-exponent-recursive 3 32)

(fast-exponent-iterative 2 32)
(fast-exponent-iterative 3 32)