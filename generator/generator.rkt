#lang racket
(define-struct point (x y) #:transparent)
(define-struct edge (start end) #:transparent)
(define-struct polygon (vertices) #:transparent)

(define (random-rational)
  (let ((rand1 (random 4294967087))
        (rand2 (random 4294967087)))
    (/ (min rand1 rand2) (max rand1 rand2))))

(define (random-cutting-edge)
  (let ((start (make-point 0 (random-rational)))
        (end (make-point 1 (random-rational))))
    (edge start end)))

;;cuts a polygon along an edge
(define (cut-polygon poly edge)
  (let ((vertices (polygon-vertices poly)))
    (values (polygon `(,@(left-points vertices edge) ,(edge-end edge) ,(edge-start edge)))
            (polygon `(,@(right-points vertices edge) ,(edge-end edge) ,(edge-start edge))))))

(define (left-points vertices edge)
  (filter (lambda (vertex) (negative? (distance vertex edge))) vertices))

(define (right-points vertices edge)
  (filter (lambda (vertex) (positive? (distance vertex edge))) vertices))

(define (distance vertex edge)
  (let ((x0 (point-x (edge-start edge)))
        (y0 (point-y (edge-start edge)))
        (x1 (point-x (edge-end edge)))
        (y1 (point-y (edge-end edge)))
        (x2 (point-x vertex))
        (y2 (point-y vertex)))
    (- (* (- x1 x0) (- y2 y0))
       (* (- x2 x0) (- y1 y0)))))

(define poly (polygon (list (point 0 0) (point 1 0) (point 1 1) (point 0 1))))
(cut-polygon poly (edge (point 1/2 0) (point 1/2 1)))