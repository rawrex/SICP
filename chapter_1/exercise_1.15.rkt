#lang sicp

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; let's disassemble the call (sine 12.15)
(/ 12.15 3.0) ; -> 4.05
(/ 4.05 3.0)  ; -> 1.35
(/ 1.35 3.0)  ; -> 0.45
(/ 0.45 3.0)  ; -> 0.15
(/ 0.15 3.0)  ; -> 0.05
; => p is called 5 times

(display "Test: ")
(= (p (p (p (p (p 0.05)))))  (sine 12.15))

; The p will be applied on each complete power of three in the angle
; Thus, the sine has O(log(n)) time complexity