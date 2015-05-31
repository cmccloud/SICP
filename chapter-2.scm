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
;; In the first case Louis doesn't recognize that lists build onto the front and our
;; iterative process starts from the front as well - he's using a queue like structure
;; for storage when what he wants is a stack (i.e. the call stack)
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

;; Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (apply append (map fringe tree)))))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (list-ref mobile 0))

(define (right-branch mobile)
  (list-ref mobile 1))

(define (branch-length branch)
  (list-ref branch 0))

(define (branch-structure branch)
  (list-ref branch 1))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (balanced mobile)
  (and (= (torque (right-branch mobile))
          (torque (left-branch mobile)))
       (every balanced (submobiles mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (submobiles mobile)
  (let* ((branches (list (left-branch mobile) (right-branch mobile)))
         (structs (map branch-structure branches)))
    (filter pair? structs)))


;; When we redefine make mobile
(define (make-mobile left right) (cons left right))

(define (make-branch length structure) (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

;; Exercise 2.32

;; prepend helper
(define (prepend x) (lambda (y) (cons x y)))

(define (subsets s)
  (if (null? s) (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (prepend (car s)) rest)))))

;; Exercise 2.33
(define (accumulate op initial seq)
  (if (null? seq) initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (my-map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (ele count) (+ count 1)) 0 seq))

;; Exercise 2.34
(define (horder-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms)
                   (this-coeff))) 0 coefficient-sequence))

;; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Exercise 2.38
(fold-right / 1 (list 1 2 3)) ; -> (3/2)
(fold-left / 1 (list 1 2 3)) ; -> (1/6)
(fold-right list nil (list 1 2 3)) ; -> (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; ->  (((() 3) 2) 1)

;; For any binary procedure f, (fold-right f (n1....nn)) will equal (fold-left f (n1....nn))
;; if (f x y) === (f y x), i.e. f must be commutative

;; Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; Nested Mapping problem
;; Given some positive integer n, find all ordered pairs of distinct positive integers (i, j)
;; such that 1 <= j < i <= n and i + j is prime.

(define (range n m)
  (define (range-helper current max)
    (if (> current max) nil
        (cons current (range-helper (+ current 1) max))))
  (range-helper n m))

(define (flat-map f sequence)
  (fold-right append nil (map f sequence)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (summation series)
  (fold-right + 0 series))

(define (make-i-range n)
    (lambda (x) (map (lambda (y) (list y x))
                     (range (+ x 1) n))))

(define (prime? n)
  (cond ((< n 2) #f)
        ((= n 2) #t)
        (else (every (lambda (x) (= (gcd n x) 1))
                     (range 2 (+ 1 (floor (sqrt n))))))))

(define (gcd x y)
  (if (= y 0) x
      (gcd y (remainder x y))))

(define (find-prime-pairs n)
  (let* ((all-j (range 1 n))
         (all-pairs (flat-map (make-i-range n) all-j)))
    (filter (compose prime? summation) all-pairs)))

;; Exercise: Permutations of a set
(define (remove-member m set)
  (filter (lambda (x) (not (= m x))) set))

(define (permutations set)
  (if (null? set) (list nil)
      (flat-map (lambda (x)
                 (map (prepend x)
                      (permutations (remove-member x set)))) set)))

;; Exercise 2.40
(define (partial f x)
  (lambda (y) (f x y)))

(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (partial list i)
                   (range 1 (- i 1))))
            (range 1 n)))

;; Exercise 2.41
(define (ordered-triples n s)
  (filter (compose (partial = s) summation)
          (flat-map (lambda (i)
                      (flat-map (lambda (j)
                                  (map (lambda (k) (list i j k))
                                       (range 1 (- j 1))))
                                (range 1 (- i 1))))
                    (range 1 n))))

;; Picture Language Exercise
;; Notes: Language primitives provided in plt scheme - use Dr. Racket to evaluate code
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0) painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0) painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((above (beside up up))
              (side (below right right))
              (corner (corner-split painter (- n 1))))
          (let ((top-half (beside above corner))
                (bottom-half (beside painter side)))
            (below bottom-half top-half))))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; Exercise 2.45
(define (split f g)
  (lambda (painter n)
    (if (= n 0) painter
        (let ((smaller ((split f g) painter (- n 1))))
          (f painter (g smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; Exercise 2.46
(define (make-vect x-cor y-cor)
  (list x-cor y-cor))

(define (xcor-vect vect)
  (list-ref vect 0))

(define (ycor-vect vect)
  (list-ref vect 1))

(define (add-vect x y)
  (make-vect (+ (xcor-vect x) (xcor-vect y))
             (+ (ycor-vect x) (ycor-vect y))))

(define (sub-vect x y)
  (make-vect (- (xcor-vect x) (xcor-vect y))
             (- (ycor-vect x) (ycor-vect y))))

(define (scale-vect x n)
  (make-vect (* n (xcor-vect x))
             (* n (ycor-vect x))))

;; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (list-ref frame 0))

(define (edge1-frame frame)
  (list-ref frame 1))

(define (edge2-frame frame)
  (list-ref frame 2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame)
                            (start-segment segment))
                           ((frame-coord-map frame)
                            (end-segment segment))))
              segment-list)))

;; Exercise 2.48
(define (make-segment v1 v2) (cons v1 v2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

;; Exercise 2.49

(define outline
  (segments->painter
   (list
   (make-segment
    (make-vect 0.0 0.0)
    (make-vect 0.99 0.99))
   (make-segment
    (make-vect 0.99 0.0)
    (make-vect 0.0 0.99)))))

(define x-image
  (segments->painter
   (list
    (make-segment
     (make-vect 0.0 0.0)
     (make-vect 0.99 0.99))
    (make-segment
     (make-vect 0.99 0.0)
     (make-vect 0.0 0.99)))))

(define diamond
  (segments->painter
   (list
    (make-segment
     (make-vect 0.0 0.5)
     (make-vect 0.5 0.99))
    (make-segment
     (make-vect 0.99 0.5)
     (make-vect 0.5 0.99))
    (make-segment
     (make-vect 0.0 0.5)
     (make-vect 0.5 0.0))
    (make-segment
     (make-vect 0.99 0.5)
     (make-vect 0.5 0.0)))))

(define half-head-segments
  (list
   (make-segment
    (make-vect 0.75 0.99)
    (make-vect 0.5 0.5))
   (make-segment
    (make-vect 0.5 0.5)
    (make-vect 0.75 0.0))))

(define head
  (beside
   (segments->painter half-head-segments)
   (flip-horiz (segments->painter half-head-segments))))

;; etc.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
          (new-origin (m origin)))
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; Exercise 2.51
(define (beside painter1 painter2)
  (let* ((split-point (make-vect 0.5 0.0))
         (paint-left
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             split-point
                             (make-vect 0.0 1.0)))
         (paint-right
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.0)
                             (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-bottom
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point))
         (paint-top
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (below-with-beside painter1 painter2)
  (let* ((top (rotate-270 painter2))
         (bottom (rotate-270 painter1))
         (rotate-90 (lambda (f) (rotate-270 (rotate-180 f)))))
    (rotate-90 (beside top bottom))))

;; Exercise 2.53
(list 'a 'b 'c) ;-> (a b c)
(list (list 'george)) ;-> ((george))
(cdr '((x1 x2) (y1 y2))) ;-> ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;-> (y1 y2)
(pair? (car '(a short list))) ;-> #f
(memq 'red '((red shoes) (blue socks))) ;-> #f
(memq 'red '(red shoes blue socks)) ;-> (red shoes blue socks)


;; Exercise 2.54
(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((and (symbol? list1) (symbol? list2))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2))
         (and (eq? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        (else #f)))

;; Exercise 2.55
(car ''abracadabra)

(car '(quote abracadabra))

(car (quote (quote abracadabra)))

(car (list 'quote 'abracadabra)) ; -> quote

(cdr ''abracadabra) ; -> (abracadabra)

;; Symbolic Differentiation
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

;; Exercise 2.56
(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
        ((= exp 1) base)
        ((and (number? base) (number? exp)) (expt base exponent))
        (else (list '** base exp))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

;; Exercise 2.57
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;; Exercise 2.58
;; a.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? s) (and (pair? s) (eq? (cadr s) '*)))

(define (multiplier s) (car s))

(define (multiplicand s) (caddr s))

;; b.
(define (augend s)
  (let ((tail (cddr s)))
    (if (null? (cdr tail)) (car tail) tail)))

(define (multiplicand p)
  (let ((tail (cddr p)))
    (if (null? (cdr tail) (car tail) tail))))

;; Representing Sets
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (adjoin-set (car set1)
                                     set2)))))

;; Exercise 2.60
(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; Other procedures unchanged

;; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              (else
               (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-set set1 (cdr set2)))))))))

;; Sets as Binary Trees
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        (else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set))))))

;; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree) '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree) result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree '()))

;; tests
(define a (make-tree 7
                     (make-tree 3
                                (make-tree 1 '() '())
                                (make-tree 5 '() '()))
                     (make-tree 9
                                '()
                                (make-tree 11 '() '()))))

(define b (make-tree 3
                     (make-tree 1 '() '())
                     (make-tree 7
                                (make-tree 5 '() '())
                                (make-tree 9
                                           '()
                                           (make-tree 11 '() '())))))

(define c (make-tree 5
                     (make-tree 3
                                (make-tree 1 '() '())
                                '())
                     (make-tree 9
                                (make-tree 7 '() '())
                                (make-tree 11 '() '()))))

;; a. Same Results
;; b. No. (tree->list-2 x) grows more slowly

;; Exercise 2.64
(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0) (cons '() elts)
        (let* ((left-size (quotient (- n 1) 2))
               (left-result (partial-tree elts left-size))
               (left-tree (car left-result))
               (non-left-elts (cdr left-result))
               (right-size (- n (+ left-size 1)))
               (this-entry (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size))
               (right-tree (car right-result))
               (remaining-elts (cdr right-result)))
          (cons (make-tree this-entry
                           left-tree
                           right-tree)
                remaining-elts))))
  (car (partial-tree elements (length elements))))

;; Exercise 2.65
;; Merge-Sort Helper
(define (merge-sort sequence)

  (define (left-half sequence)
    (take sequence (quotient (length sequence) 2)))

  (define (right-half sequence)
    (define (right-helper sequence n)
      (if (= n 0) sequence
          (right-helper (cdr sequence) (- n 1))))
    (right-helper sequence (quotient (length sequence) 2)))

  (define (merge seq1 seq2)
    (cond ((and (null? seq1) (null? seq2)) '())
          ((null? seq1) seq2)
          ((null? seq2) seq1)
          (else
           (let ((x1 (car seq1))
                 (x2 (car seq2)))
             (cond ((<= x1 x2)
                    (cons x1 (merge (cdr seq1) seq2)))
                   ((< x2 x1)
                    (cons x2 (merge seq1 (cdr seq2)))))))))

  (if (>= 1 (length sequence)) sequence
      (merge (merge-sort (left-half sequence))
             (merge-sort (right-half sequence)))))

(define (list->balanced-tree sequence)
  (list->tree (merge-sort sequence)))

;; union-set
(define (union-set set1 set2)
  (define (union-helper seq1 seq2)
    (cond ((and (null? seq1) (null? seq2)) '())
          ((null? seq1) seq2)
          ((null? seq2) seq1)
          (else
           (let ((x1 (car seq1))
                 (x2 (car seq2)))
             (cond ((= x1 x2)
                    (cons x1
                          (union-helper (cdr seq1) (cdr seq2))))
                   ((< x1 x2)
                    (cons x1
                          (union-helper (cdr seq1) seq2)))
                   (else
                    (cons x2 (union-helper seq1 (cdr seq2)))))))))
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (union-helper list1 list2))))

;; tests
(define (union-tests)
  ;; union set should work with empty sets
  (define (test1)
    (let* ((set-a (list->tree '()))
           (set-b (list->tree '()))
           (set-c (union-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '())))
  ;; union set should work with duplicate sets
  (define (test2)
    (let* ((set-a (list->tree '(1 2 3 4 5)))
           (set-b (list->tree '(1 2 3 4 5)))
           (set-c (union-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '(1 2 3 4 5))))
  ;; union set should work with different sets
  (define (test3)
    (let* ((set-a (list->tree '(1 2 3 4 5)))
           (set-b (list->tree '(6 7 8 9 10)))
           (set-c (union-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '(1 2 3 4 5 6 7 8 9 10))))
  ;; union set should work with overlapping sets
  (define (test4)
    (let* ((set-a (list->tree '(1 3 5 6 7)))
           (set-b (list->tree '(2 4 6 8 10)))
           (set-c (union-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '(1 2 3 4 5 6 7 8 10))))
  (let ((results (list (test1) (test2) (test3) (test4))))
    (every (lambda (x) x) results)))

;; intersection-set
(define (intersection-set set1 set2)
  (define (intersection-helper list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (let ((x1 (car list1))
              (x2 (car list2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-helper (cdr list1) (cdr list2))))
                ((< x1 x2)
                 (intersection-helper (cdr list1) list2))
                ((< x2 x1)
                 (intersection-helper list1 (cdr list2)))))))
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (intersection-helper list1 list2))))

;; tests
(define (intersection-tests)
  ;; intersection should work with empty sets
  (define (test1)
    (let* ((set-a (list->tree '()))
          (set-b (list->tree '()))
          (set-c (intersection-set set-a set-b))
          (list-c (tree->list-2 set-c)))
      (equal? list-c '())))
  ;; intersection should work with duplicate sets
  (define (test2)
    (let* ((set-a (list->tree '(1 2 3 4 5)))
           (set-b (list->tree '(1 2 3 4 5)))
           (set-c (intersection-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '(1 2 3 4 5))))
  ;; intersection should work with different sets
  (define (test3)
    (let* ((set-a (list->tree '(1 2 3)))
           (set-b (list->tree '(4 5 6)))
           (set-c (intersection-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '())))
  ;; intersection should work with overlapping sets
  (define (test4)
    (let* ((set-a (list->tree '(1 2 3 4 5 11 13)))
           (set-b (list->tree '(2 3 7 9 11)))
           (set-c (intersection-set set-a set-b))
           (list-c (tree->list-2 set-c)))
      (equal? list-c '(2 3 11))))
  (let ((results (list (test1) (test2) (test3) (test4))))
    (every (lambda (x) x) results)))

;; Sets and information retrieval
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (if ((null? set-of-records)
       false
       (let ((entry-key (key (entry set-of-records))))
         (cond ((= entry-key given-key) (entry set-of-records))
               ((< given-key entry-key)
                (lookup given-key (left-branch set-of-records)))
               (else (lookup given-key (right-branch set-of-records))))))))

;; Huffman Encoding Trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ;-> (a d a b b c a)

;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contains? x seq)
  (cond ((null? seq) #f)
        ((eq? x (car seq)) #t)
        (else (contains? x (cdr seq)))))

(define (encode-symbol symbol tree)
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond ((contains? symbol (symbols left))
           (if (leaf? left) (list 0)
               (cons 0 (encode-symbol symbol left))))
          ((contains? symbol (symbols right))
           (if (leaf? right) (list 1)
               (cons 1 (encode-symbol symbol right))))
          (else (error ("Symbol not found: ENCODE SYMBOL" symbol tree))))))

;; tests
(define (encode-test)
  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree (make-leaf 'D 1)
                                     (make-leaf 'C 1)))))
  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  (let ((decoded (decode sample-message sample-tree)))
    (equal? sample-message (encode decoded sample-tree))))

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
       (adjoin-set
        (make-code-tree (car leaves)
                        (cadr leaves))
        (cddr leaves)))))

;; Exercise 2.70
(define alphabet '((A 2) (GET 2) (SHA 3) (WAH 1)
                   (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define my-tree (generate-huffman-tree alphabet))

(define my-message '(get a job sha na na na na na na na na get a job sha na na
                         na na na na na na na wah yip yip yip yip yip yip yip yip
                         sha boom))
(define my-encoding (encode my-message my-tree))
;; How many bits are required for encoding?
(length my-encoding)
;; What is the smallest number of bits that would be needed to encode this song
;; if we used a fixed-length code for the eight-symbol alphabet?
(* (length my-message) 3)

;; Exercise 2.71
(define (make-n-alphabet n)
  (map (lambda (x) (list x (expt 2 x))) (range 1 n)))

(define alphabet-5 (make-n-alphabet 5))
(define tree-5 (generate-huffman-tree alphabet-5))

(define alphabet-10 (make-n-alphabet 10))
(define tree-10 (generate-huffman-tree alphabet-10))
;; Most Frequent = 1
;; Least Frequent = N - 1

;; Exercise 2.72

;; Multiple Representations for Abstract Data

;; Tagging procedures
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; Typed operations
;; rectangular package
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

;; polar package
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (attan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Generic operations
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unkown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))`

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; higher order operations
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (* (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; using data directed programming
;; assumes global (put <op> <type> <item>) and
;; global (get <op> <type>)

;; rectangular package definition
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; polar package definition
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))
                (atan y x))))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Generic operations
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
        (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error
         "No method for these types: APPLY-GENERIC"
         (list op type-tags)))))

(define (real-part z) (apply-generic 'real-part z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Exercise 2.3.2
(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

;; a. Neither numbers nor variables are valid arguments for procedures
;; 'operator' and 'operands', on which our method retrieval system depends

;; b.
(define (install-sum-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list a1 '+ a2))))
  (define (addend x) (car x))
  (define (augend x) (cadr x))
  (define (deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  ;; interface to rest of system
  (put 'make-sum '(+) make-sum)
  (put 'deriv '(+) deriv)
  'done)

(define (install-product-deriv-package)
  ;; handle dependencies
  (if (get 'make-sum '+) 'done (install-sum-deriv-package))

  ;; internal procedures
  (define make-sum (get 'make-sum '+))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))
  (define (multiplier x) (car x))
  (define (multiplicand x) (cadr x))
  (define (deriv operands var)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands))))

  ;; interface to rest of system
  (put 'make-product '(*) make-product)
  (put 'deriv '(*) deriv)
  'done)

;; c.
(define (install-exp-deriv-package)
  ;; handle dependencies
  (if (get 'make-sum '+) 'done (install-sum-deriv-package))
  (if (get 'make-product '*) 'done (install-product-deriv-package))

  ;; internal procedures
  (define make-sum (get 'make-sum '+))
  (define make-product (get 'make-product '*))
  (define (make-exponentiation base exp)
    (cond ((= exp 0) 1)
          ((= exp 1) base)
          ((and (number? base) (number? exp)) (expt base exponent))
          (else (list '** base exp))))
  (define (base x) (car x))
  (define (exponent x) (cadr x))
  (define (deriv operands var)
    (make-product
     (make-product (exponent operands)
                   (make-exponentiation (base operands)
                                        (make-sum (exponent operands) -1)))
     (deriv (base operands) var)))

  ;; interface to rest of system
  (put 'make-exponentiation '(**) make-exponentiation)
  (put 'deriv '(**) deriv)
  'done)

;; d.
;; in install exp: (put 'deriv '(**) deriv) -> (put '(**) 'deriv deriv)
;; in install product: (put 'deriv '(*) deriv) -> (put '(*) 'deriv deriv)
;; in install sum: (put 'deriv '(+) deriv) -> (put '(+) 'deriv deriv)

;; Exercise 2.74
;; Generic Procedures
;; a.
;; division files should be placed into a list or cons cell, with a division identifier
;; in the form of a tagged pair in the first position, and file information in the second position.
;; The tag should have the form ('division-file id) where id is the unique division identifier in
;; form of a symbol.
;; Divisions must implement and introduce to the dispatch table a procedure 'get-record
;; indexed to their division-identifier which, when given an employee name
;; returns that employee record
(define (tag x) (car x))
(define (division-file? x) (and (pair? (tag x)) (eq? (car (tag x)) 'division-file)))
(define (division-identifier x) (cadr (tag x)))
(define (file-information x) (cdr x))
(define (get-record file name)
  (let* ((id (division-identifier file))
        (proc (get 'get-record id)))
    (if proc
        (proc name)
        (error "Unknown procedure: GET-RECORD" file))))

;; b.
(define (record? x) (and (pair? x) (eq? (tag x) 'record)))
(define (record-contents record) (cdr record))
(define (get-salary file name)
  (let* ((record (get-record file name))
         (id (division-identifier file))
         (proc (get 'get-salary id)))
    (if proc
        (proc (record-contents record))
        (error "Unkonwn procedure: GET-SALARY" file))))
;; employee records should be placed into a list or cons cell, with a tag 'record placed into
;; the first position and record information in the second position
;; Divisions must implemented and introduce to the dispatch table a procedure 'get-salary
;; indexed to their division-identifier which, when given an employee record, retrives from
;; that record the employee's salary.

;; c.
(define (find-employee-record files name)
  (let ((results (map (lambda (file) (get-record file name))) files))
    (filter record? results)))

;; d. All division will be responsible for creation of a package which behaves something like
;; the one below. Additionally they will be responsible for tagging their division files:
;; (tag (cons 'division-file division-id) division-file)
(define (sample-division-package)
  ;; internal procedures
  (define (employee-name record)
    (list-ref record 3))
  (define (get-employee-info employees name)
    (if (null? employees)
        '()
        (let ((record (car employees)))
          (if (eq? (employee-name record) name)
              record
              (get-employee-info (cdr employees) name)))))
  (define (get-employee-salary record)
    (list-ref 5 record))

  ;; interface to rest of system
  (define division-id '34)
  (define (tag x y) (cons x y))
  (put 'division-file division-id (tag (cons 'division-file division-id) our-division-file))
  (put 'get-record division-id
       (lambda (name)
         (tag 'record (get-employee-info master-personnel-file name))))
  (put 'get-salary division-id get-employee-salary)
  'done)

;; Message Passing
;; Dispatch on operation name
(define (make-from-real-imag x y)
  (lambda (op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)))))

(define (apply-generic op arg) (arg op))

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op)))))

;; Exercise 2.76
;; Generic operations with explicit dispatch:
;; - Adding types requires that operations be updated to handle the new type
;; - Adding operations requires nothing other than accounting for the existing types.

;; Data directed style:
;; - Adding types requires the creation of new operations for the type to be placed
;; into the dispatch table, or aliasing existing operations with the new type
;; - Adding operations requires the association of said operation with relevant types,
;; and ensuring that no previously defined operations are overridden.

;; Message passing style:
;; - Adding types requires the creation of a new dispatch object
;; - Adding operations requires updating existing dispatch objects with the new operation

;; Everything else being equal, in a system in which new operations must often be added,
;; either generic operations or a data directed style would likely be effective. In a system
;; in which there exist few types, the advantages of generic operations would be greatest.


;; Systems with Generic Operations

;; Generic Arithmetic Operations
(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

;; Scheme number package
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (gcd x y)
  (if (= y 0) x
      (gcd y (remainder x y))))

  (define (number x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Exercise 2.77
;; (define a-complex-number (make-complex-from-real-imag 3 4))
;; (magnitude a-complex-number)
;; -> Call to the generic magnitude procedure
;; (magnitude '(complex rectangular 3 4))
;; -> Expanded form of complex showing both tags
;; (apply-generic 'magnitude '(complex rectangular 3 4))
;; -> generic magnitude calls apply generic with 'magnitude tag
;; ((get 'magnitude 'complex) '(rectangular 3 4))
;; -> apply generic strips off top level tag, gets the 'magnitude procedure
;;    defined within the 'complex package, calls it on contents
;; (magnitude '(rectangular 3 4))
;; -> expanded form of new datum
;; (apply-generic 'magnitude '(rectangular 3 4))
;; -> magnitude calls apply generic with 'magnitude tag
;; ((get 'magnitude 'rectangular) '(3 4))
;; -> apply generic strips off top level tag, gets the 'magnitude procedure
;;    defined within the 'rectangular package, calls it on contents
;; (sqrt (+ (square (real-part-rectangular '(3 4)))
;;           (square (imag-part-rectangular '(3 4)))))
;; Produces the result

;; Exercise 2.78
(define (type-tag datum)
  (cond ((or (symbol? datum) (number? datum)) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((or (symbol? datum) (number? datum)) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

;; Exercise 2.79
;; in global namespace - or generic arithmetic package
(define (equ? x y)
  (apply-generics 'equ x y))
;; within ordinary numbers package
(put 'equ? '(scheme-number scheme-number) =)
;; within rational numbers package
(define (equ?? x y)
  (= (* (number x) (denom y)) (* (numer y) (denom x))))
(put 'equ? '(rational rational) equ?)
;; within complex numbers package
(define (equ?? x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex) equ?)

;; Exercise 2.80
;; in global namespace - or generic arithmetic package
(define (=zero? x) (apply-generics '=zero? x))
;; within oridinary numbers package
(define (=zero? x) (= x 0))
(put '=zero? '(scheme-number) =zero?)
;; within rational numbers package
(define (=zero? x) (= (numer x) 0))
(put '=zero? '(rational) =zero?)
;; within complex numbers package
(define (=zero? x) (= (magnitude x) 0))
(put '=zero? '(complex) =zero?)

;; Coercion and Generic Procedures
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let* ((type-tag (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* ((type1 (car type-tags))
                   (type2 (cadr type-tags))
                   (a1 (car args))
                   (a2 (cadr args))
                   (t1->t2 (get-coercion type1 type2))
                   (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                     (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                     (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags)))))
            (error "No method for these types"
                   (list op type-tags))))))

;; Exercise 2.81
;; a. With Louis's changes to apply-generic, calling exp with two complex numbers would
;; result in an infinite loop.
;; b. Louis is correct, in a sense. Apply-generic should guard against the possibility that a programmer
;; introduces a type coercion which hangs the system.
;; c.
(define (apply-generic op . args)
  ;; internal procedures
  (define (valid-length args)
    (= (length args) 2))
  (define (same-type args)
    (= (type-tag (car args))
       (type-tag (cadr args))))
  (define (try-coercion tags op args)
    (let* ((type1 (car tags))
           (type2 (cadr tags))
           (a1 (car args))
           (a2 (cadr args))
           (t1->t2 (get-coercion type1 type2))
           (t2->t1 (get-coercion type2 type1)))
      (cond (t1->t2
             (apply-generic op (t1->t2 a1) a2))
            (t2->t1
             (apply-generic op a1 (t2->t1 a2)))
            (else (error "No method for these types"
                         (list op tags))))))
  ;; procedure body
  (let* ((type-tag (map type-tag args))
         (proc (get op type-tags)))
    (cond (proc (apply proc (map contents args)))
          ((and (valid-length args)
                (not (same-type args)))
           (try-coercion type-tag op args))
          (else (error "No method for these types"
                       (list op type-tags))))))


