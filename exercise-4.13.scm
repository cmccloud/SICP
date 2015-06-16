;; Exercise 4.13
;; make-unbound! will remove the given binding from the first frame
;; in which the binding exists. This form is useful if one wants to
;; access a shadowed variable.
;; unbind-all! uses make-unbound! to remove all references to the given
;; binding up through each frame of a given environment.
