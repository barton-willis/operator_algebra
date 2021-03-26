
;;; Declare the reduced Planck constant to be constant and positive.
(mfuncall '$declare '$ħ '$constant)
(mfuncall '$assume (take '(mgreaterp) '$ħ 0))

;;; Declare p,q, and U to be operators; p and q are the momentum and position operators,
;;; respectively, and U is a multiplication operator (a potential). 
(mfuncall '$declare '$p '$operator)
(mfuncall '$declare '$q '$operator)
(mfuncall '$declare '|$u| '$operator)

;;; True iff e has the form q(XXX).
(defun position-p (e)
    (and (consp e) (eq (caar e) '$q)))

;;; True iff e has the form p(XXX).
(defun momentum-p (e)  
    (and (consp e) (eq (caar e) '$p)))

;;; True iff e has the form U[xxx](YYY).
(defun potential-p (e)
    (and (consp e) (eq (caar e) 'mqapply) (eq (caaadr e) '|$u|)))

(mfuncall '$put '$q  #'(lambda (s) (mul '$x s)) '$formula)
(mfuncall '$put '$p  #'(lambda (s) (mul -1 '$%i '$ħ ($diff s '$x))) '$formula)

(defun simp-momentum-op (e y z)
    (declare (ignore y))
    (oneargcheck e)
    (let ((e (simplifya (cadr e) z))) ;specdisrep? I've forgotten...
      (cond 
          ((position-p e) ;P Q --> Q P  - %i*ħ 
            (add (take '($q) (take '($p) (cadr e))) (mul -1 '$%i '$ħ (cadr e))))
          ((potential-p e) ; P U[n] --> -%i ħ U[n+1] + U[n] P
            (let ((n (car (subfunsubs e))) (fn (car (subfunargs e))))
                (add
                    (mul -1 '$%i '$ħ (subfunmakes '|$u| (list (add 1 n)) (list fn)))
                    (subfunmakes '|$u| (list n) (list (take '($p) fn))))))
          ((mplusp e)
             (addn (mapcar #'(lambda (s) (take '($p) s)) (cdr e)) t))

          ((and (mtimesp e) ($constantp (second e)))
             (let ((cnst 1))
                (setq e (cdr e)) ; remove (mtimes simp)
                (while (and (cdr e) ($constantp (car e)))
                    (setq cnst (mul cnst (pop e))))
                (mult cnst (take '($p) (car e)))))

          (t (list (list '$p 'simp) e))))) 

(setf (get '$p 'operators) #'simp-momentum-op)

(defun simp-position-op (e y z)
    (declare (ignore y))
    (oneargcheck e)
    (let ((e (simplifya (cadr e) z)))
      (cond ((mplusp e)
             (addn (mapcar #'(lambda (s) (take '($q) s)) (cdr e)) t))

          ((and t (mtimesp e) ($constantp (second e)))
             (let ((cnst 1))
                (setq e (cdr e)) ; remove (mtimes simp)
                (while (and (cdr e) ($constantp (car e)))
                    (setq cnst (mul cnst (pop e))))
                (mult cnst (take '($q) (car e)))))

          (t (list (list '$q 'simp) e))))) 

(setf (get '$q 'operators) #'simp-position-op) 

(defun simp-potential-op (e y z)
    (declare (ignore y))
    (let ((a (mapcar #'(lambda (s) (simplifya s z)) (subfunsubs e)))
          (b (mapcar #'(lambda (s) (simplifya s z)) (subfunargs e))))

        (setq e (cadr e))

       (cond ((position-p (first b)) ; Do U[n] q --> q U[n]
                ;; The call to $expand xxx 0 0 is disapointing, but try
                ;; operator_simplify(U[n] . q^^4) without the call to $expand.
                (take '($q) ($expand (subfunmakes '|$u| a (cdar b)) 0 0)))

             ;;Start of rule that does U[n] . U[m] --> U[m] U[n] when
             ;; (say) (great m n).
            ;;;; ((and (potential-p (first b)) (great (cadar b) e))

             (t (subfunmakes '|$u| a b)))))

(setf (get '$|u| 'specsimp) #'simp-potential-op)

