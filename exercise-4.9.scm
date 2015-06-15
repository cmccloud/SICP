;; Exercise 4.9
;; Installs do as a package
(define (do-package install-proc eval)
  ;; Internal Procedures
  (define (do? exp) (tagged-list? exp 'do))
  (define (do-test exp) (caaddr exp))
  (define (do-value exp) (car (cdaddr exp)))
  (define (do-command exp) (cadddr exp))
  (define (do-variable-set exp) (cadr exp))
  (define (var-name var) (car var))
  (define (var-init var) (cadr var))
  (define (var-step var)
    (if (null? (cddr var))
        (var-name var)
        (caddr var)))
  (define (do-vars set)
    (map var-name set))
  (define (do-inits set)
    (map var-init set))
  (define (do-steps set)
    (map var-step set))
  (define (make-fn-def proc-name args body)
    (make-call 'define (cons proc-name args) body))
  (define (make-if pred cons alt)
    (make-call 'if pred cons alt))
  (define (make-call procedure . parameters)
    (cons procedure parameters))
  (define (make-thunk . body)
    (apply make-call 'lambda '() body))
  (define (do->combination exp)
    (let* ((do-variables (do-variable-set exp))
           (recursive-call (apply make-call 'do-iter (do-steps do-variables)))
           ;; forms part of the recursive do-loop
           (do-loop (make-call 'begin (do-command exp) recursive-call))
           (loop-if (make-if (do-test exp) (do-value exp) do-loop))
           (do-iter (make-fn-def 'do-iter (do-vars do-variables) loop-if))
           (thunk (make-thunk do-iter 'do-iter))
           ;; create a new scope for our defined looping function
           (invoked-thunk (make-call thunk)))
      ;; calls do-iter from our thunk
      (apply make-call invoked-thunk (do-inits do-variables))))

  ;; Public Interface
  (install-proc
   (list
    (list 'eval 'do (lambda (exp env) (eval (do->combination exp) env)))))
  'do-package-installed!)
