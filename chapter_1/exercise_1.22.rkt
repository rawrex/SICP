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


;; The Profiling function

(define (timed-prime-test n test-function)
  (define (report result elapsed-time)
    (cond (result
           (display "\n\nnumber: ")
           (display n)
           (display "\ntime: ")
           (display elapsed-time)
           (newline)))
          ;; we do not report non-prime
    result)
  (define (start-prime-test n start-time)
    (if (test-function n)
        (report #t (- (runtime) start-time))
        (report #f (- (runtime) start-time))))
  (start-prime-test n (runtime)))


;; Range test

(define (find-primes begin end)
  (cond ((and (< begin end) (even? begin))
         (find-primes (inc begin) end))
        ((< begin end)
         (timed-prime-test begin prime-b?)
         (find-primes (+ 2 begin) end))))


;; Find specified number of primes starting from...

(define (find-n-primes from n test-function)
  (cond ((even? from) (find-n-primes (inc from) n test-function))
        ((and (> n 0) (timed-prime-test from test-function)) (find-n-primes (+ from 2) (dec n) test-function))
        ((> n 0) (find-n-primes (inc from) n test-function))))

(display "GCD test:")
(find-n-primes 100000 3 prime-a?)     ; average time: 50 
(find-n-primes 1000000 3 prime-a?)    ; average time: 150 
(find-n-primes 10000000 3 prime-a?)   ; average time: 385 

(display "\n* * *\nFermat test:")
(find-n-primes 10000000 3 prime-b?)   ; average time: 20 
(find-n-primes 100000000 3 prime-b?)  ; average time: 23 
(find-n-primes 1000000000 3 prime-b?) ; average time: 25 