#lang sicp

(define (gradient f)
  (let* ((dx 0.0001)
         (df (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
         (LR 0.2))
    (define (update x)
      (- x (* LR (df x))))
    (define (iter start)
      (if (<= (df start) 0.001)
          start
          (iter (update start))))
    (iter (random 10))))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(gradient square)
(gradient (lambda (x) (square (square x))))