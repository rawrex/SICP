#lang sicp

(define (square x)
  (* x x))

;; Smallest Divisor Test

(define (prime-a? n)
  (define (divides? divisor number)
    (= (remainder number divisor) 0))
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (inc test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= (smallest-divisor n) n))


;; The Fermat Test

(define (prime-b? n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  (fast-prime? n 3))


(define (timed-prime-test n test-function)
  (define (report-prime result elapsed-time)
    (display "\nresult: ")
    (display result)
    (display "\ntime: ")
    (display elapsed-time)
    (newline))
  (define (start-prime-test n start-time)
    (if (test-function n)
        (report-prime "prime" (- (runtime) start-time))
        (report-prime "not prime" (- (runtime) start-time))))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(timed-prime-test 19999 prime-a?)
(timed-prime-test 19999 prime-b?)
(timed-prime-test 12419731 prime-a?)
(timed-prime-test 12419731 prime-b?)