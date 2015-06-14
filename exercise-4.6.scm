;; Exercise 4.6
;; Installed as a package
(define (let-package install-proc eval)
  ;; Internal definitions
  (define (let-bindings exp) (cadr exp))
  (define (let-body exp) (cddr exp))
  (define (binding-exps bindings)
    (if (null? bindings)
        '()
        (cons (cadar bindings)
              (binding-exps (cdr bindings)))))
  (define (binding-var binding) (car binding))
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  (define (let->combination exp)
    (cons (make-lambda
           (map binding-var (let-bindings exp))
           (let-body exp))
          (binding-exps (let-bindings exp))))

  ;; Interface
  (install-proc
   (list
    (list 'eval 'let (lambda (exp env) (eval (let->combination exp) env)))))
  'let-package-installed!)

;; tests
(define (test4-6)
  (define expression
    '(let ((x 3) (y 4) (z 5))
       (* x y z)))
  (define target
    '((lambda (x y z)
        (* x y z)) 3 4 5))
  (define result (let->combination expression))
  (equal? result target))
