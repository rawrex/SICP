#lang sicp

; Triangle as is
;
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1

; Let's tilt it to make the pattern to appear
;
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1

; We can see that a number is formed
; by taking the sum of the number in the previous row in the same column
; and the number in the previous row in the previous column 

(define (pascal-triangle row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal-triangle (dec row) col)
                 (pascal-triangle (dec row) (dec col))))))

(pascal-triangle 1 1)
(pascal-triangle 4 2)
(pascal-triangle 5 3)
(pascal-triangle 25 14)