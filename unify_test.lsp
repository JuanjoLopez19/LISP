(setf FALLO '"FALLO")

; Unify.lsp --> contains the main executation of the problem
(defun checker (E1 E2)
    (prog ()
        (princ "Check if the elements are equal to return NIL: ")
        (write (equal E1 E2))
        (terpri)
        (when (equal E1 E2) (return NIL))
        (princ "They are not, so check if E1 is a variable: ")
        (write (is_variable E1))
        (terpri)
        (if (is_variable E1)
            (progn
                (princ "E1 is a variable, so check if E1 is in E2: ")
                (write (is_contained E1 E2))
                (terpri)
                (if (is_contained E1 E2)
                    (return '"FALLO")
                    (return (list (list E2 E1)))
                )
            )
            (progn
                (princ "E1 is not a variable, so check if E2 is a variable: ")
                (write (is_variable E2))
                (terpri)
                (when (is_variable E2) (return (list (list E1 E2))))
            )
        )
        (princ "None of the conditions succeed so FALLO will be returned")
        (terpri)
        (return '"FALLO")
    )
)

(defun recursivity (E1 E2)
    (prog (F1 F2 G1 G2 Z1 Z2 T1 T2)
        (princ "Setting the variables that contains the first element")
        (terpri)
        ; First of each argument
        (setf F1 (first E1))
        (setf F2 (first E2))

        (princ "F1: ")
        (write F1)
        (terpri)

        (princ "F2: ")
        (write F2)
        (terpri)

        (princ "Setting the variables that contains the rest of the elements")
        (terpri)

        ; Rest of each argument
        (setf T1 (rest E1))
        (setf T2 (rest E2))

        (princ "T1: ")
        (write T1)
        (terpri)

        (princ "T2: ")
        (write T2)
        (terpri)

        (princ "Unify the first elements of the arguments")
        (terpri)

        (setf Z1 (unify_test F1 F2))

        (princ "Z1: ")
        (write Z1)
        (terpri)
        (terpri)

        (princ "Check if Z1 is FALLO: ")
        (write (equal Z1 FALLO))
        (terpri)

        (when (equal Z1 FALLO) (return "FALLO"))

        (princ "Apply the result to the rest of the first argument")
        (terpri)

        (setf G1 (apply_items Z1 T1))
        (princ "G1: ")
        (write G1)
        (terpri)
        (terpri)    

        (princ "Apply the result to the rest of the second argument")
        (terpri)  

        (setf G2 (apply_items Z1 T2))
        (princ "G2: ")
        (write G2)
        (terpri)


        (princ "Unify the result of the compositions")
        (terpri)  

        (setf Z2 (unify_test G1 G2))
        (princ "Z2: ")
        (write Z2)
        (terpri)
        (terpri)

        (princ "Check if Z2 is FALLO: ")
        (write (equal Z2 FALLO))
        (terpri)
        (when (equal Z2 FALLO) (return "FALLO"))

        (princ "The result is:")
        (write (compose_items Z1 Z2))
        (terpri)
        (terpri)

        (return (compose_items Z1 Z2))
    )
)
        
        

        
    
        
    
        
    

(defun unify_test (E1 E2)
    (princ "The first expression is: ")
    (princ E1)
    (terpri)
    (terpri)
    (princ "The second expression is: ")
    (princ E2)
    (terpri)
    (terpri)
    (cond ((is_atom E1) (checker E1 E2))
          ((is_atom E2) (checker E2 E1))
          (T (recursivity E1 E2))
    )
)
