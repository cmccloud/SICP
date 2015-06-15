;; Exercise 4.8
;; Installed as a package
(define (let-package install-proc eval)
  ;; Internal definitions
(define (named-let? exp)
  (symbol? (cadr exp)))
(define (named-let-name exp)
  (cadr exp))
(define (named-let-vars exp)
  (define (named-let-vars-helper bindings)
    (if (null? bindings) '()
        (cons (car (car bindings))
              (named-let-vars-helper (cdr bindings)))))
  (named-let-vars-helper (caddr exp)))
(define (named-let-body exp)
  (cadddr exp))
(define (make-def name parameters body)
  (list 'define (cons name parameters) body))
(define (named-let-args exp)
  (define (named-let-args-helper bindings)
    (if (null? bindings) '()
        (cons (cadr (car bindings))
              (named-let-args-helper (cdr bindings)))))
  (named-let-args-helper (caddr exp)))

(define (make-named-let exp)
  (let* ((definition (make-def (named-let-name exp)
                              (named-let-vars exp)
                              (named-let-body exp)))
         (thunk (make-lambda nil (list definition
                                       (named-let-name exp))))
         (args (named-let-args exp)))
    (cons (list thunk) args)))
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
    (cond ((named-let? exp) (make-named-let exp))
          (else
           (cons (make-lambda
                  (map binding-var (let-bindings exp))
                  (let-body exp))
                 (binding-exps (let-bindings exp))))))

  ;; Interface
  (install-proc
   (list
    (list 'eval 'let (lambda (exp env) (eval (let->combination exp) env)))))
  'let-package-installed!)

;; tests
(define (test4-8)
  (define expression
    '(let fib-iter ((a 1)
                    (b 0)
                    (count 20))
       (if (= count 0)
           b
           (fib-iter (+ a b) a (- count 1)))))
  (define target
    '(((lambda ()
         (define (fib-iter a b count)
           (if (= count 0)
               b
               (fib-iter (+ a b) a (- count 1))))
         fib-iter)) 1 0 20))
  (define result (let->combination expression))
  (equal? result target))
