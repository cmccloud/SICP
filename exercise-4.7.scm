;; Exercise 4.7
;; Written as a data directed package
(define (let*-package install-proc eval)
  ;; Internal definitions
  (define (make-let binding body)
    (list 'let (list binding) body))
  (define (first-exp binding-exps)
    (car binding-exps))
  (define (rest-exps binding-exps)
    (cdr binding-exps))
  (define (let*-bindings exp)
    (cadr exp))
  (define (let*-body exp)
    (caddr exp))
  (define (let*->nested-lets exp)
    (define (let*-helper bindings)
      (if (null? bindings)
          (let*-body exp)
          (make-let
           (first-exp bindings)
           (let*-helper (rest-exps bindings)))))
    (let*-helper (let*-bindings exp)))
  ;; Interface
  (install-proc
   (list
    (list 'eval 'let* (lambda exp env)
          (eval (let*->nested-lets exp) env))))
  'ok)

;; tests
(define (test4-7)
  (define expression
    '(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
       (* x z)))
  (define target '(let ((x 3))
                    (let ((y (+ x 2)))
                      (let ((z (+ x y 5)))
                        (* x z)))))
  (define result (let*->nested-lets expression))
  (equal? result target))
