#lang sicp

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iter n)
  (define (iter a b c counter)
    (if (= counter n)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (inc counter))))
  (iter 2 1 0 0))

(display "Recursive:\n")
(f-recursive 1)
(f-recursive 2)
(f-recursive 3)
(f-recursive 4)
(newline)
(display "Iterative:\n")
(f-iter 1)
(f-iter 2)
(f-iter 3)
(f-iter 4)