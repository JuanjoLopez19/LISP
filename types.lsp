(defun is_variable (aux)
    (cond ((atom aux) NIL)
          ((and (symbolp (first aux)) (string= (first aux) '?)) T)  
    )
)

(defun is_atom(var)
    (cond ((atom var) T)
        ((eq (first var) '?) T)
        (T NIL))
)

(defun is_list(aux)
    (and (listp aux)
         (not (is_variable aux))
    )

)