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
;; Given some positive integer n, find all ordered pairs of distinct positive integers (i, j) such that 1 <= j < i <= n and i + j is prime.

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
