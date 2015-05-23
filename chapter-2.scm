;; Rational Arithmetic
;; 2.1.1

;; Representation of Rational Numbers - Concrete Data Specification
(define (make-rat n d)
  (cons n d))

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
