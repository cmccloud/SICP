;; Rational Arithmetic
;; 2.1.1

;; Representation of Rational Numbers - Concrete Data Specification
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Interface
(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

;; Prodcedure definitions - operations on abstract rationals
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (subtract-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (multiply-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (divide-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equals-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Helpers

(define (gcd x y)
  (if (> y x) (gcd y x)
      (let ((r (remainder x y)))
        (if (= r 0) y
            (gcd y r)))))

;; Exercise 2.1

(define (make-rat-with-negs x y)
  (if (= (sign x) (sign y))
      (make-rat (abs x) (abs y))
      (make-rat (- (abs x))
                (abs y))))

(define (sign x)
  (if (>= x 0) 1 0))

;; Exercise 2.2

(define (make-segment x y)
  (cons x y))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (/ (+ (x-point start) (x-point end))
                   2)
                (/ (+ (y-point start) (y-point end))
                   2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3

(define (length-segment s)
  (let ((x (start-segment s))
        (y (end-segment s)))
    (abs (- (x-point x)
            (x-point y)))))

(define (height-segment s)
  (let ((x (start-segment s))
        (y (end-segment s)))
    (abs (- (y-point x)
            (y-point y)))))

;; rectangle defined as two segments
(define (make-rect s1 s2)
  (cons s1 s2))

(define (length-rect rect)
  (length-segment (car rect)))

(define (height-rect rect)
  (height-segment (cdr rect)))

(define (perimeter-rect rect)
  (* 2 (+ (length-rect rect)
          (height-rect rect))))

(define (area-rect rect)
  (* (length-rect rect)
     (height-rect rect)))

;; rectangle defined as three points, forming segments (a, b) (a, c)
(define (make-rect a b c)
  (cons (make-segment a b)
        (make-segment a c)))

;; rectangle defined as four points, forming segments (a, b) (c, d)
(define (make-rect a b c d)
  (cons (make-segment a b)
        (make-segment c d)))

;; Exercise 2.4
(define (my-cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (pair-int a b)
  (define (pow x y)
    (cond ((= y 0) 1)
          ((= y 1) x)
          (else (* x (pow x (- y 1))))))
  (* (pow 2 a) (pow 3 b)))

(define (divides-num num divisor)
  (define (iter x result)
    (cond ((= 0 (remainder x divisor))
           (iter (/ x divisor) (+ result 1)))
          (else result)))
  (iter num 0))

(define (first-int pair)
  (divides-num pair 2))

(define (second-int pair)
  (divides-num pair 3))

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add-church m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
