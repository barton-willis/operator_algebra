
;;; Declare the reduced Planck constant to be constant. That makes ħ outative for a 
;;; linear operator.

(mfuncall '$declare '$ħ '$constant)
(mfuncall '$declare '$p '$operator)
(mfuncall '$declare '$q '$operator)

(defmfun position-p (e)
    (and (consp e) (eq (caar e) '$q)))

(defmfun momentum-p (e)  
    (and (consp e) (eq (caar e) '$p)))

(defmfun potential-p (e)
    (and (consp e) ($subvarp ($op e)) (eq (caar e) '$U)))

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
