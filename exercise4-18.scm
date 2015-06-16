;; Exercise 4.18
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
;; Using the second strategy
(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (streamp-map f y)))
      (set! y a)
      (set! dy b)
      y)))
;; Which reduces to
(lambda (f y0 dt)
  ((lambda (y dy)
    ((lambda (a b)
      (set! y a)
      (set! dy b))
     (integral (delay dy) y0 dt) (stream-map f y)))
   '*unassigned* '*uniassigned*))
;; No, this will not work - the expression (stream-map f y)
;; Will be evaluated before being passed as an argument to the
;; inner lambda (the inner let) and y will still be '*unassigned*

;; Using the first strategy
(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
;; Which reduces to
(lambda (f y0 dt)
  ((lambda (y dy)
     (set! y (integral (delay dy) y0 dt))
     (set! dy (stream-map f y)))
   '*unassigned* '*unassigned*))
;; This will work because y is set prior to the evaluation of
;; (stream-map f y).
