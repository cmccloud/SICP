;; Exercise 4.4
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

;; Installed as top level forms to eval
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
        ((and? exp) (eval (and->if (and-predicates exp)) env))
        ((or? exp) (eval (or->if (or-predicates exp) env)))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))
(define (first-pred preds) (car preds))
(define (rest-preds preds) (cdr preds))
(define (last-pred? preds) (null? (cadr preds)))
(define (and->if preds)
  (cond ((null? preds) 'true)
        ((last-pred? preds) (first-pred preds))
        (else (make-if (first-pred preds)
                       (and->if (rest-preds preds))
                       'false))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))
(define (or->if preds)
  (cond ((null? preds) 'false)
        (else (make-if (first-pred preds)
                       (first-pred preds)
                       (or->if (rest-preds preds))))))
