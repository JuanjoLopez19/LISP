(setf FALLO '"FALLO")

; Unify.lsp --> contains the main executation of the problem
(defun checker (E1 E2)
    (prog ()
        (terpri)
        (princ "Condition Block start")
        (terpri)
        (princ "Check if E1 = E2")
        (when (equal E1 E2) (return NIL))
        (terpri)
        (princ "Check if E1 is a variable")
        (terpri)
        (if (is_variable E1)
            (progn 
                (princ "E1 is a variable")
                (terpri)
                (princ "Check if E1 is contained in E2")
                (terpri)
                (if (is_contained E1 E2)
                    (return '"FALLO")
                    (return (list E2 E1))
                )
            )
            (progn
                (princ "E1 is not a variable")
                (terpri)
                (princ "Check if E2 is a variable")
                (when (is_variable E2) (return (list E1 E2)))
            )
        )
        (return '"FALLO")
    )
)

(defun recursivity (E1 E2)
    (prog (F1 F2 G1 G2 Z1 Z2 T1 T2)
        (terpri)
        (princ "This is when neither of them are an atom")
        (terpri)
        ; First of each argument
        (setf F1 (first E1))
        (setf F2 (first E2))

        ; Rest of each argument
        (setf T1 (rest E1))
        (setf T2 (rest E2))
        (setf Z1 (unify F1 F2))
        
        (when (equal Z1 FALLO) (return "FALLO"))

        (setf G1 (apply_items Z1 T1))
        (terpri)
        (write G1)
        ;(setf G2 (apply_items Z1 T2))

        ;(setf Z2 (unify G1 G2))

        ;(when (equal Z2 FALLO) (return "FALLO"))

        ;(return (compose Z1 Z2))
    )

)

(defun unify (E1 E2)
    (princ "The first argument is: ")
    (write E1)
    (terpri)
    (princ "The second argument is: ")
    (write E2)

    (cond ((is_atom E1) (checker E1 E2))
          ((is_atom E2) (checker E2 E1))
          (T (recursivity E1 E2))
    )
)
