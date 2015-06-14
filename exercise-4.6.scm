;; Exercise 4.6
;; Installed as a package
(define (let-package install-proc eval)
  ;; Internal definitions
  (define (let-bindings exp) (cadr exp))
  (define (let-body exp) (caddr exp))
  (define (binding-var binding) (car binding))
  (define (binding-exp binding) (cadr binding))
  (define (let->combination exp)
    (list (make-lambda
           (map binding-var (let-bindings exp))
           (let-body exp))
          (map binding-exp (let-bindings exp))))

  ;; Interface
  (install-proc
   (list
    (list 'eval 'let (lambda (exp env) (eval (let->combination exp) env)))))
  'let-package-installed!)

