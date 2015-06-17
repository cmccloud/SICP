;; Exercise 4.23
;; Alyssa's confusion seems reasonable - the version of analyze-sequence
;; in the text yields a fully formed depth-first tree of procedure calls,
;; whereas her version yields a function which, when executed, evaluates
;; the procedure expression in the same order as the tree.
;; Comparison of the two across a single call would not lead to any noticable
;; differences, both procedures have o(n) time complexity with respect to the
;; number of expressions within the sequence. The version in the book only
;; does this work once however, whereas Alyssa's has to do it for every call.
