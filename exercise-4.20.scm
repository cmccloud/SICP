;; Exercise 4.20
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (binding-vars bindings)
  (map car bindings))
(define (binding-exps bindings)
  (map cadr bindings))
(define (initialize vars)
  (map (lambda (var) (list var '*unassigned*)) vars))
(define (transform-body body vars exps)
  (if (null? vars)
      body
      (cons (list 'set! (car vars) (car exps))
            (transform-body body (cdr vars) (cdr exps)))))
(define (transform-letrec exp)
  (let ((vars (binding-vars (letrec-bindings exp)))
        (expressions (binding-exps (letrec-bindings exp))))
    (cons 'let
          (cons (initialize vars)
                (transform-body (letrec-body exp) vars expressions)))))

;; tests
(define (test4-20)
  (define expression
    '(letrec ((fact (lambda (n)
                      (if (= n 1) 1 (* n (fact (- n 1)))))))
       (fact 10)))
  (define target
    '(let ((fact *unassigned*))
       (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
       (fact 10)))
  (equal? (transform-letrec expression) target))

;; b - Louis's definition
(let ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
      (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
  (even? 10))
;; which reduces to
((lambda (even? odd?)
   (even? 10))
 (lambda (n) (if (= n 0) true (odd? (- n 1))))
 (lambda (n) (if (= n 0) false (even? (- n 1)))))
;; Because the bodies of even and odd are defined outside of the
;; scope of the lambda which represents our let form, their
;; internal calls to each other have no referents.
