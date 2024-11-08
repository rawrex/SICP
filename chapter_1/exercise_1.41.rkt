#lang sicp

(define (twice f)
  (lambda (x) (f (f x))))

;; 21, as with each new call to twice we double the number of calls
(((twice (twice twice)) inc) 5)
