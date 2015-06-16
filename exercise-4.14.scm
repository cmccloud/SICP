;; Louis's map fails because, within our evaluator, all procedures,
;; primitive and defined, are represented as tagged data.
;; Internally, (map f '(1 2 3)) would be attempting to call
;; (map ('tag ....) '(1 2 3))
