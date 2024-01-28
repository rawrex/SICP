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
  (define (report-prime result elapsed-time)
    (display "\n\nnumber: ")
    (display n)
    (display "\nresult: ")
    (display result)
    (display "\ntime: ")
    (display elapsed-time)
    (newline))
  (define (start-prime-test n start-time)
    (if (test-function n)
        (report-prime "prime" (- (runtime) start-time))
        ;; Do not report non-primes
        ;(report-prime "not prime" (- (runtime) start-time))))
        ))
  (start-prime-test n (runtime)))


;; Range test

(define (search-for-primes begin end)
  (cond ((and (< begin end) (even? begin))
         (search-for-primes (inc begin) end))
        ((< begin end)
         (timed-prime-test begin prime-b?)
         (search-for-primes (+ 2 begin) end))))

(search-for-primes 1000 1100)