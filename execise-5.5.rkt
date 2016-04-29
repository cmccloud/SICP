;; Exercise 5.5
;; A. Simulation of (fact 3)
;; Stack => ()
;; Reg n => 3
;; Reg Continue => fact-done
;; Stack => (fact-done)
;; Stack => (3 fact-done)
;; Reg n => 2
;; Reg Continue => after-fact
;; Stack => (after-fact 3 fact-done)
;; Stack => (2 after-fact 3 fact-done)
;; Reg n => 1
;; Reg Continue => after-fact
;; Reg val => 1
;; Reg n => 2
;; Stack => (after-fact 3 fact-done)
;; Reg Continue => after-fact
;; Stack => (3 fact-done)
;; Reg val => 2
;; Reg n => 3
;; Stack => (fact-done)
;; Reg Continue => fact-done
;; Stack => ()
;; Reg val => 6
;; FACT-DONE

;; B. Simulation of (fib 2)
;; Stack => ()
;; Reg n => 2
;; Reg continue => fib-done
;; Stack => (fib-done)
;; Reg continue => after-fib-n-1
;; Stack => (2 fib-done)
;; Reg n => 1
;; Reg val => 1
;; Reg n => 2
;; Stack => (fib-done)
;; Reg continue => fib-done
;; Stack => ()
;; Reg n => 0
;; Stack => (fib-done)
;; Reg continue => afterfib-n-2
;; Stack => (1 fib-done)
;; Reg val => 0
;; Reg n => 0
;; Reg val => 1
;; Stack => (fib-done)
;; Reg continue => fib-done
;; Stack => ()
;; Reg val => 1
;; FIB-DONE

