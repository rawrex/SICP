(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s) 'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc)) (set! already-run? true)
                 result)
          result))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (display x)
  x)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define x (stream-map show (stream-enumerate-interval 0 10)))



(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))


(define (add-streams s1 s2) (stream-map + s1 s2))


(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams ints factorials)))

(define (part-sums s)
  (define (eq-curr? x)
    (eq? (stream-car s) x))
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (add-streams
                    (stream-filter (eq-curr? (stream-car s)) s))
                   (part-sums (stream-cdr s)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))



(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1) (stream-cdr s2)))))))))



;; Ex 3.56
(define a (cons-stream 1 (scale-stream a 2)))

(define c (cons-stream 1 (scale-stream c 5))) 

(define b (cons-stream 1 (scale-stream b 3)))

(define h (merge (megre a b) c))







(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))