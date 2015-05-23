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

;; Interval Arithmetic
(define (add-interval x y)
  (let ((min (+ (lower-bound x) (lower-bound y)))
        (max (+ (upper-bound x) (upper-bound y))))
    (make-interval min max)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-vound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Exercise 2.10
(define (div-interval x y)
  (let ((upper (upper-bound y))
        (lower (lower-bound y)))
    (if (or (= upper 0) (= lower 0))
        (display "Error, cannot divide by Zero")
        (mul-interval x (make-interval (/ 1.0 upper)
                                       (/ 1.0 lower))))))

(define (make-interval a b)
  (cons a b))

;; Exercise 2.7
(define (upper-bound x)
  (let ((p1 (car x))
        (p2 (cdr x)))
    (if (> p1 p2) p1
        p2)))

(define (lower-bound x)
  (let ((p1 (car x))
        (p2 (cdr x)))
    (if (< p1 p2) p1
        p2)))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;; Exercise 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (assert-width x y)
  (let ((width-x (width-interval x))
        (width-y (width-interval y))
        (width-sum (width-interval (add-interval x y)))
        (sum-widths (add-interval width-x width-y))
        (width-diff (width-interval (sub-interval x y)))
        (diff-widths (sub-interval width-x width-y)))
    (display "Width of X")
    (display width-x)
    (display "Width of Y")
    (display width-y)
    (display "Width of Sum of Intervals X Y")
    (display width-sum)
    (display "Sum of Adding Width of X and Width of Y")
    (display sum-widths)
    (display "Width of the difference of X Y")
    (display width-diff)
    (dispaly "Difference of the widths of X Y")
    (display diff-widths)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(define (make-center-percent c t)
  (make-center-width (c (/ (* c t) 100.00))))

(define (percent-interval i)
  (/ (width i) (center i)))


;; Hierarchical Data and Closure Property

;; Define nil as end of list marker for convenience
(define nil '())

;; Exercise 2.17
(define (last-pair x)
  (cond ((null? x) nil)
        ((null? (cdr x)) x)
        (else (last-pair (cdr x)))))

;; Exercise 2.18
(define (reverse x)
  (define (reverse-helper x state)
    (if (null? x) state
        (reverse-helper (cdr x)
                        (cons (car x)
                              state))))
  (reverse-helper x nil))

;; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomiation coin-values))
                     coin-values)))))

(define (first-denomination x)
  (car x))

(define (except-first-denomination x)
  (cdr x))

(define (no-more? x)
  (null? x))

;; Exercise 2.20 - without map, filter, or other higher order functions
(define (same-parity x . y)
  (define (same-parity-helper x y)
    (cond ((null? y) nil)
          ((or (and (even? x) (even? (car y)))
               (and (odd? x) (odd? (car y))))
           (cons (car y)
                 (same-parity-helper x (cdr y))))
          (else (same-parity-helper x (cdr y)))))
  (cons x (same-parity-helper x y)))

;; Exercise 2.21
(define (square-list items)
  (if (null? items) nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22
;; In the first case Louis doesn't recognize that lists build onto the front and our iterative process starts from the front as well - he's using a queue like structure for storage when what he wants is a stack (i.e. the call stack)
;; In the second case Louis is trying to conjoin nil which is terminating the list

;; Exercise 2.23
(define (my-for-each f list)
  (cond ((null? list) #t)
        (else (f (car list))
              (for-each f (cdr list)))))

(define (my-for-each-2 f list)
  (if (null? list) #t
      (begin
        (f (car list))
        (my-for-each-2 f (cdr list)))))

;; Exercise 2.24
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (apply + (map count-leaves tree)))))

;; (list 1 (list 2 (list 3 4)))
;; Root node 1 with one child, 2, with two children 3 and 4

;; Exercise 2.25
(car (cdr (car (cdr (cdr (list 1 2 (list 5 7) 9))))))

(car (car (list (list 7))))

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ;; -> (1 2 3 4 5 6)
(cons x y) ;; ((1 2 3) 4 5 6)
(list x y) ;; ((1 2 3) (4 5 6))

;; Exercise 2.27
(define (deep-reverse x)
  (if (not (pair? x)) x
      (map deep-reverse (reverse x))))
