;; There are two possibilities:
;; 1: (try try) -> 'halted
;; 2: (try try) -> runs-forever
;; For 1 to be true, halted? would have identified the application
;; of (try try) as one which does not halt - so now we have a
;; contradiction; (try try) returns 'halted if and only if (try try)
;; does not halt, i.e does not return a value.
;; For 2 to be true, halted? would have identified the application
;; of (try try) as one which does halt - which causes
;; (try try) to run-forever. Another contraction, (try try) will
;; run forever if and only if (try try) returns a value.
