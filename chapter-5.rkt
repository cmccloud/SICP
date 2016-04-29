(require sicp)

;; Exercise 5.1 - Notebook

;; A specification of the GCD machine
(dath-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))
 (operations
  ((name rem) (inputs (register a) (register b)))
  ((name =) (inputs (register b) (constant 0)))))

(controller
 test-b                                 ;label
 (test =)                               ;test
 (branch (label gcd-done))              ;conditional branch
 (t<-r)                                 ;button push
 (a<-b)                                 ;button push
 (b<-t)                                 ;button push
 (goto (label test-b))                  ;unconditional branch
 gcd-done)                              ;label

;; Combined Notation
(controller
 test-b
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label test-b))
 gcd-done)

;; GCD Machine with Read and Print
(controller
 gcd-loop
 (assign a (op read))
 (assign b (op read))
 test-b
 (test (op =)
       (reg b)
       (const 0))
 (branch (label gcd-done))
 (assign t
         (op rem)
         (reg a)
         (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label test-b))
 gcd-done
 (perform (op print)
          (reg a))
 (goto (label gcd-loop)))

;; Register Machine Interface
;; (make-machine <register-names> <operations> <controller>)
;; (set-register-contents! <machine-model> <register-name> <value>)
;; (get-register-contents <machine-model> <register-name>)
;; (start <machine-model>)

;; Example of GCD-MACHINE
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)))
