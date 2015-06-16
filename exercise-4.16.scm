;; Exercise 4.16
;; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment frame))
             ((eq? var (car vars))
              (if (eq? (car vars) '*unassigned*)
                  (error "Unassianged Variable" (car vars))
                  (car vals)))
             (else (scan (cdr vars) (cdr vals))))))
    (if (eq? env the-empty-environment)
        (error "Variable not found" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; b
(define (scan-out-defines procedure-body)
  (define (defs-to-sets exp)
    (if (definition? exp)
        (cons 'set! (cdr exp))
        exp))
  (define (transform-procedure-body args)
    (cons 'let (cons args
                     (map defs-to-sets procedure-body))))
  (define (hoist-defs body)
    (cond ((null? body) body)
          ((definition? (car body))
           (cons (list (cadr (car body)) '*unassigned*)
                 (hoist-defs (cdr body))))
          (else (hoist-defs (cdr body)))))
  (let ((hoisted-defs (hoist-defs procedure-body)))
    (if (null? hoisted-defs)
        procedure-body
        (transform-procedure-body hoisted-defs))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; Scan out defines should be installed in make-procedure
;; so as to avoid having to call it multiple times
;; procedure-body is called at each application whereas
;; make-procedure will only be called once for each procedure
;; created.
