#lang sicp

(define (for-each items function)
  (cond ((null? items) #t)
        (else (function (car items)) (for-each (cdr items) function))))

(for-each (list 1 2 3 4 5) (lambda (x) (display x) (newline)))