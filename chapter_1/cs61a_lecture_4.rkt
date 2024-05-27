#lang sicp

(define (compose f g) (lambda (x) (f (g x))))

; (define compose (lambda (f g) (lambda (x) (f (g x)))))

(define pi 3.1415)

(define (square x)
  (* x x))

(define absolute (compose sqrt square))

; (define (absolute) (compose sqrt square))

(define dx 0.0000001)

(define (deriv f)
  (lambda (x) (/
               (-
                (f (+ x dx))
                (f x))
               dx)))

(define (roots a b c)
  ((lambda (D)  (cons (/ (+ (- b) D) (* 2 a))  (/ (- (- b) D) (* 2 a))))
   (sqrt (- (* b b) (* 4 a c)))))

(define (roots1 a b c)
  (let ((D (sqrt (- (* b b) (* 4 a c)))))
   (cons (/ (+ (- b) D) (* 2 a))
         (/ (- (- b) D) (* 2 a)))))


           