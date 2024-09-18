;; Maxima code for working with operators, especially quantum mechanical operators.

;; Barton Willis
;; Professor of Mathematics
;; University of Nebraska at Kearney

;; Copyright © Barton Willis, 2021, 2022, 2024
;; GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007

(setf (get '$operator_adjoint 'dissym) '(#\꙳))
(setf (get '$operator_adjoint 'dimension) 'dimension-postfix)

(defprop $operator\_adjoint tex-postfix tex)
(defprop $operator\_adjoint ("^{\\star}") texsym)