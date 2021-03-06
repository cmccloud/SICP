;; SICP - Chapter 4

;; 4.1.1 - The Core of the Evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; Procedure arguments
(define (list-of-values exp env)
  (if (no-operands? exps) '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; Conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; Exercise 4.1 - Solution assumes only that the underlying lisp uses
;; applicative order evaluation

;; using let special form
(define (list-of-values-left exp env)
  (if (no-operands? exp) '()
      (let ((value-1 (eval (first-operand exps) env)))
        (cons value-1 (list-of-values-left (rest-operands-exps) env)))))

(define (list-of-values-right exp env)
  (if (no-operands? exp) '()
      (let ((rest-values (list-of-values-right (rest-operands-exps) env)))
        (cons (eval (first-operand exps) env) rest-values))))

;; using lambdas
(define (list-of-values-left exp env)
  (if (no-operands? exp) '()
      ((lambda (first-value)
         (cons first-value (list-of-values-left (rest-operands-exps) env)))
       (eval (first-operand exps) env))))

(define (list-of-values-right exp env)
  (if (no-operands? exp) '()
      ((lambda (rest-values)
         (cons (eval (first-operand exp) env)
               rest-values))
       (list-of-values-right (rest-operands-exp) env))))

;; 4.1.2 Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;; formal parameters
                   (cddr exp)))) ;; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cdddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clauses isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Exercise 4.2
;; a - The evaluator will attempt to apply the procedure define to x 3
;; The particulars of what define could evaluate to in the given environment
;; aside, we know by definition that x will always be undefined given that
;; it could never have been associated in any environment.

;; b
(define (_application? exp) (tagged-list? exp 'call))
(define (_operator exp) (cadr exp))
(define (_operands exp) (cddr exp))

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

;; Exercise 4.4
;; Installed as new special forms
(define (and-package install-proc eval)
  ;; Internal definitions
  (define (preds exp) (cdr exp))
  (define (first-pred and-exps) (car and-exps))
  (define (rest-preds and-exps) (cdr and-exps))
  (define (last-pred? and-exps) (null? (cdr and-exps)))
  (define (eval-and and-exps env)
    (if (null? and-exps)
        'true
        (let ((first-value (eval (first-pred and-exps) env)))
          (cond ((last-pred? and-exps) first-value)
                ((true? first-value)
                 (eval-and (rest-preds and-exps) env))
                (else 'false)))))
  ;; Interface
  (install-proc
   (list
    (list 'eval 'and (lambda (exp env) (eval-and (preds exp) env)))))
  'and-package-installed!)

(define (or-package install-proc eval)
  ;; Internal definitions
  (define (preds exp) (cdr exp))
  (define (first-pred or-exps) (car or-exps))
  (define (rest-preds or-exps) (cdr or-exps))
  (define (last-pred? or-exps) (null? (cdr or-exps)))
  (define (eval-or or-exps env)
    (if (null? or-exps)
        'false
        (let ((first-value (eval (first-pred or-exps) env)))
          (cond ((true? first-value) 'true)
                (else (eval-or (rest-preds or-exps) env))))))
  ;; Interface
  (install-proc
   (list
    (list 'eval 'or (lambda (exp env) (eval-or (preds exp) env)))))
  'or-package-installed!)

(and-package table-installer eval)
(or-package table-installer eval)

;; Installed as derived expressions
(define (and-package install-proc eval)
  (define (make-if pred cons alt)
    (list 'if pred cons alt))
  (define (eval-and exps env)
    (cond ((null? exps) 'true)
          ((null? (cdr exps))
           (eval (make-if (car exps) (car exps) 'false) env))
          (else (eval (make-if (car exps) (cons 'and (cdr exps)) 'false) env))))
  (install-proc
   (list
    (list 'eval 'and (lambda (exp env) (eval-and (cdr exps) env)))))
  'and-package-installed!)

(define (or-package install-proc eval)
  (define (make-if pred cons alt)
    (list 'if pred cons alt))
  (define (eval-or exps env)
    (if (null? exps)
        'false
        (eval (make-if (car exps) (car exps) (cons 'or (cdr exps))) env)))
  (install-proc
   (list
    (list 'eval 'or (lambda (exp env) (eval-or (cdr exps) env)))))
  'or-package-installed!)

;; Exercise 4.5
(define (extended-cond-package install-proc eval)
  ;; Internal definitions
  (define (cond-actions clause) (cdr clause))

  (define (cond-predicate clause) (car clause))

  (define (cond-clauses exp) (cdr exp))

  (define (arrow-clause? exp) (eq? (car (cond-actions exp)) '=>))

  (define (cond-else-clause? clause) (eq? (cond-predicate clauses) 'else))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

  (define (make-begin seq) (cons 'begin seq))

  (define (valid-recipient? exp env)
    (let ((recipient (eval exp env)))
      (and (lambda? recipient) ;; must be a function
           (pair? (cadr recipient)) ;; check for variadic lambda
           (= (length (cadr recipient)) 1))) ;; check for arity

  (define (else->exp first rest clauses)
    (if (null? rest)
        (sequence->exp (cond-actions first))
        (error "Else clause isn't last: COND->IF" clauses)))

  (define (arrow->if first rest env)
    (if (valid-recipient? (caddr first) env)
        (make-if (cond-predicate first)
                 (list (cadr (cond-actions first))
                       (cond-predicate first))
                 (expand-clauses rest))
        (error "Recipient must be single arity procedure: COND-IF"
               (caddr first))))

  (define (expand-clauses clauses env)
    (if (null? clauses) 'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((cond-else-clause? first) (else->exp first rest clauses))
                ((arrow-clause? first) (arrow->if first rest env))
                (else (make-if (cond-predicate first)
                               (sequence->exp (cond-actions first))
                               (expand-clauses rest)))))))

  (define (cond->if exp env)
    (expand-clauses (cond-clauses exp env)))
  ;; Interface
  (install-proc
   (list
    (list 'eval 'cond (lambda (exp env) (eval (cond->if exp env) env)))))
  'extended-cond-package-installed!)

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

