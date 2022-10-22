(setf FALLO '"FALLO")

; Unify.lsp --> contains the main executation of the problem
(defun checker (E1 E2)
    (prog ()
        
        (when (equal E1 E2) (return NIL))
        
        (if (is_variable E1)
            (progn 
                (if (is_contained E1 E2)
                    (return '"FALLO")
                    (return (list (list E2 E1)))
                )
            )
            (progn
                (when (is_variable E2) (return (list (list E1 E2))))
            )
        )
        (return '"FALLO")
    )
)

(defun recursivity (E1 E2)
    (prog (F1 F2 G1 G2 Z1 Z2 T1 T2)
        
        ; First of each argument
        (setf F1 (first E1))
        (setf F2 (first E2))

        ; Rest of each argument
        (setf T1 (rest E1))
        (setf T2 (rest E2))
        (setf Z1 (unify F1 F2))
        
        
        (when (equal Z1 FALLO) (return "FALLO"))
    
        (setf G1 (apply_items Z1 T1))

        
        (setf G2 (apply_items Z1 T2))

        
        (setf Z2 (unify G1 G2))
    
        (when (equal Z2 FALLO) (return "FALLO"))

        (return (compose_items Z1 Z2))
    )

)

(defun unify (E1 E2)
    
    (cond ((is_atom E1) (checker E1 E2))
          ((is_atom E2) (checker E2 E1))
          (T (recursivity E1 E2))
    )
)
