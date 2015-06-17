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
