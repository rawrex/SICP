#lang sicp

;; Common
(define (square x)
  (* x x))

;; Point
(define (make-point x y)
  (cons x y))

(define (point-x point)
  (car point))

(define (point-y point)
  (cdr point))


;; Segment
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (segment-start segment)
  (car segment))

(define (segment-end segment)
  (cdr segment))

(define (segment-length segment)
  (sqrt (+
         (square (- (point-x (segment-start segment)) (point-x (segment-end segment))))
         (square (- (point-y (segment-start segment)) (point-y (segment-end segment)))))))

;; Rectangle
;;      a
;;   A-----B
;; d |  /  | b
;;   D-----C
;;      c
;; diagonal: DB


;; Rectangle from points
;; Constructor
; (define (make-rect point-A point-C)
;   (cons point-A point-C))
; 
; ;; Selectors for vertices
; (define (rect-A rect)
;   (car rect))
; (define (rect-C rect)
;   (cdr rect))
; (define (rect-B rect)
;   (make-point (point-x (rect-C rect)) (point-y (rect-A rect))))
; (define (rect-D rect)
;   (make-point (point-x (rect-A rect)) (point-y (rect-C rect))))
; 
; ;; Selectors for sides
; (define (rect-a rect)
;   (make-segment (rect-A rect) (rect-B rect)))
; (define (rect-b rect)
;   (make-segment (rect-B rect) (rect-C rect)))
; (define (rect-c rect)
;   (make-segment (rect-C rect) (rect-D rect)))
; (define (rect-d rect)
;   (make-segment (rect-D rect) (rect-A rect)))


;; Rectangle from segment
;; Contsructor
(define (make-rect diagonal)
  diagonal)

;; Selectors for vertices
(define (rect-A rect)
  (let* ((D-x (point-x (segment-start rect)))
         (D-y (point-y (segment-start rect)))
         (B-x (point-x (segment-end rect)))
         (B-y (point-y (segment-end rect))))
    (make-point D-x B-y)))

(define (rect-B rect)
  (let* ((D-x (point-x (segment-start rect)))
         (D-y (point-y (segment-start rect)))
         (B-x (point-x (segment-end rect)))
         (B-y (point-y (segment-end rect))))
    (make-point B-x B-y)))

(define (rect-C rect)
  (let* ((D-x (point-x (segment-start rect)))
         (D-y (point-y (segment-start rect)))
         (B-x (point-x (segment-end rect)))
         (B-y (point-y (segment-end rect))))
    (make-point B-x D-y)))

(define (rect-D rect)
  (let* ((D-x (point-x (segment-start rect)))
         (D-y (point-y (segment-start rect)))
         (B-x (point-x (segment-end rect)))
         (B-y (point-y (segment-end rect))))
    (make-point D-x D-y)))

;; Selectors for sides
(define (rect-a rect)
  (let ((B (rect-B rect))
       (D (rect-D rect)))
    (make-segment (make-point (point-x D) (point-y B)) B)))

(define (rect-b rect)
  (let ((B (rect-B rect))
       (D (rect-D rect)))
    (make-segment B (make-point (point-x B) (point-y D)))))

(define (rect-c rect)
  (let ((B (rect-B rect))
       (D (rect-D rect)))
    (make-segment (make-point (point-x B) (point-y D)) D)))

(define (rect-d rect)
  (let ((B (rect-B rect))
       (D (rect-D rect)))
    (make-segment D (make-point (point-x D) (point-y B)))))

;; Operations on the rectangles
(define (rect-perimeter rect)
  (+
   (* 2 (segment-length (rect-a rect)))
   (* 2 (segment-length (rect-b rect)))))

(define (rect-area rect)
  (*
   (segment-length (rect-a rect))
   (segment-length (rect-b rect))))


;; Main
(define p1 (make-point 1 1))
(define p2 (make-point 4 3))
(define s (make-segment (make-point 1 1) (make-point 4 3)))

;(define r (make-rect p1 p2))
(define r (make-rect s))

(rect-perimeter r)
(rect-area r)
