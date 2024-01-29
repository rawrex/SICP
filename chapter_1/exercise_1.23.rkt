#lang sicp


(define (old-prime-a? n)
  (= (smallest-divisor n inc) n))

(define (new-prime-a? n)
  (= (smallest-divisor n odd-next) n))

(define (smallest-divisor n next-function)
  (find-divisor n 2 next-function))

(define (odd-next x)
  (if (= x 2)
      (inc x)
      (+ x 2)))

;; Let's redefine the find-divisor 
(define (find-divisor n test-divisor next-function)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-function test-divisor) next-function))))

(define (divides? divisor number)
  (= (remainder number divisor) 0))

(define (square n)
  (* n n))


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

;; Find specified number of primes starting from...
(define (find-n-primes from n test-function)
  (cond ((even? from) (find-n-primes (inc from) n test-function))
        ((and (> n 0) (timed-prime-test from test-function)) (find-n-primes (+ from 2) (dec n) test-function))
        ((> n 0) (find-n-primes (inc from) n test-function))))

(define (run-tests)
  (display "OLD test:")
  (find-n-primes 10000000 3 old-prime-a?)
  (display "\n\nNEW test:")
  (find-n-primes 10000000 3 new-prime-a?))

(run-tests)
