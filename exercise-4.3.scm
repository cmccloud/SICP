;; Exercise 4.3

;; Begin Package definitions
;; Packages accept as arguments an installation procedure
;; and the top level eval form with which to make recursive
;; eval calls
;; ==========================================================
(define (quote-package install-proc eval)
  (install-proc
   (list
    (list 'eval 'quote (lambda (exp env) (cadr exp)))))
  'quote-package-installed!)

(define (assignment-package install-proc eval)
  ;; Internal procedures
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env))
  (install-proc
   (list
    (list 'eval 'set! eval-assignment)))
  'assignment-package-installed!)

(define (definition-package install-proc eval)
  ;; Internal procedures
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  ;; Interface
  (install-proc
   (list
    (list 'eval 'define eval-definition)))
  'definition-package-installed!)

(define (if-package install-proc eval)
  ;; Internal Procedures
  (define (if-predicate exp) (cadr exp))
  (define (if-consquent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cdddr exp)
        'false))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  ;; Interface
  (install-proc
   (list
    (list 'eval 'if eval-if)))
  'if-package-installed!)

(define (lambda-package install-proc eval)
  ;; Internal Procedures
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (make-procedure exp env)
    ;; incomplete at this time
    )
  (install-proc
   (list
    (list 'eval 'lambda make-procedure)))
  'lambda-package-installed!)

(define (begin-package install-proc eval)
  ;; Internal Procedures
  (define (begin-actions exp) (cdr exp))
  (define (first-exp exps) (car exps))
  (define (rest-exps exps) (cadr exps))
  (define (last-exp? exps) (null? (cdr exps)))
  (define (eval-sequence exps env)
    (cond ((last-exp? exps)
           (eval (first-exp exps) env))
          (else
           (eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env))))

  (install-proc
   (list
    (list 'eval 'begin (lambda (exp env)
                         (eval-sequence
                          (begin-actions exp)
                          env)))))
  'begin-package-installed!)

(define (cond-package install-proc eval)
  ;; Internal definitions
  (define (cond-actions clause) (cdr clause))
  (define (cond-predicate clause) (car clause))
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause) (eq? (cond-predicate clauses) 'else))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq) (cons 'begin seq))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clauses? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF" clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  ;; Interface
  (install-proc
   (list
    (list 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))))
  'cond-package-installed!)

(define (self-evaluating-package install-proc eval)
  ;; Interface
  (install-proc
   (list
    (list 'eval 'self-evaluating (lambda (exp env) exp))))
  'self-evaluation-package-installed!)

(define (variable-package install-proc eval)
  ;; Internal definitions
  (define (lookup-variable-value exp env)
    ;; not yet defined
    )
  ;; Interface
  (install-proc
   (list
    (list 'eval 'variable lookup-variable-value)))
  'variable-package-installed!)
;; End Package Definitions
;; ==========================================================

(define (make-installer get put)
  ;; Builds an installer with given get and put operations
  ;; Installer then takes in a list of procedures and attempts to
  ;; install them using supplied get and put operations
  ;; Procedure :: (name:: symbol, type :: symbol, proc :: function)

  (define (install-procedure name type proc)
    ;; Attempts to install a given procedure under name for a given type
    ;; Installs procedure or signals error on namespace conflict
    (let ((conflict (get name type)))
      (cond ((null? conflict) (put name type proc))
            (else (error "Name-space conflict: INSTALL-PROCEDURE"
                         (list name type proc))))))

  (lambda (procedures)
    (for-each install-procedure procedures)))

(define table (make-table))
(define (get type name) ((table 'get) name type))
(define (put type name proc) ((table 'put) name type proc))
(define table-installer (make-installer get put))

;; Eval definition
(define (eval exp env)

  (define (application? exp) (pair? exp))

  (define (expression-type exp)
    (cond ((number? exp) 'self-evaluating)
          ((string? exp) 'self-evaluating)
          ((symbol? exp) 'variable)
          (else (car exp))))

  (let ((dispatch-procedure (get 'eval (expression-type exp))))
    (cond ((not (null? dispatch-procedure))
           (dispatch-procedure exp env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else (error "Unknown expression type: EVAL" exp)))))

;; Install packages
(self-evaluation-package table-installer eval)
(variable-package table-installer eval)
(quote-package table-installer eval)
(assignment-package table-installer eval)
(definition-package table-installer eval)
(if-package table-installer eval)
(lambda-package table-installer eval)
(begin-package table-installer eval)
(cond-package table-installer eval)

