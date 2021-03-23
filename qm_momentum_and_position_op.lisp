
;;; Declare the reduced Planck constant to be constant. That makes ħ outative for a 
;;; linear operator.

(mfuncall '$declare '$ħ '$constant)
(mfuncall '$declare '$p '$operator)
(mfuncall '$declare '$q '$operator)

(defun position-p (e)
    (and (consp e) (eq (caar e) '$q)))

(defun momentum-p (e)  
    (and (consp e) (eq (caar e) '$p)))

(defun potential-p (e)
    (and (consp e) (eq (caar e) 'mqapply) (eq (caaadr e) '|$u|)))

;;put(P, lambda([q], -%i*ħ*diff(q,x)), 'formula);
;;put(Q, lambda([q], x*q), 'formula);

(mfuncall '$put '$q  #'(lambda (s) (mul '$x s)) '$formula)
(mfuncall '$put '$p  #'(lambda (s) (mul -1 '$%i '$ħ ($diff s '$x))) '$formula)

(defun simp-momentum-op (e y z)
    (declare (ignore y))
    (oneargcheck e)
    (let ((e (simplifya (cadr e) z))) ;specdisrep? I've forgotten...
      (cond 
          ((position-p e) ;P Q --> Q P  + %i*ħ 
            (add (take '($q) (take '($p) (cadr e))) (mul '$%i '$ħ (cadr e))))
          ((potential-p e) ; P U[n] --> -%i ħ U[n+1] + U[n] P
            (let ((n (car (subfunsubs e))) (fn (car (subfunargs e))))
                (add
                    (mul -1 '$%i '$ħ (subfunmakes '|$u| (list (add 1 n)) (list fn)))
                    (subfunmakes '|$u| (list n) (list (take '($p) fn))))))
          ((mplusp e)
             (addn (mapcar #'(lambda (s) (take '($p) s)) (cdr e)) t))
          ((and (mtimesp e) ($constantp (second e)))
             (mult (second e) (take '($p) (muln (cddr e) t))))
          (t (list (list '$p 'simp) e))))) 

(setf (get '$p 'operators) #'simp-momentum-op)

(defun simp-position-op (e y z)
    (declare (ignore y))
    (oneargcheck e)
    (let ((e (simplifya (cadr e) z)))
      (cond ((mplusp e)
             (addn (mapcar #'(lambda (s) (take '($q) s)) (cdr e)) t))
          ((and (mtimesp e) ($constantp (second e)))
             (mult (second e) (take '($q) (muln (cddr e) t))))
          (t (list (list '$q 'simp) e))))) 

(setf (get '$q 'operators) #'simp-position-op) 

(defun simp-potential-op (e y z)
    (declare (ignore y))
   ;; (print `(e = ,e))
    e)

(setf (get '$|u| 'specsimp) #'simp-potential-op)

