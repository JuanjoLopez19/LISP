; Unify.lsp --> contains the main executation of the problem
(defun checker (E1 E2)
    (prog ()
        (terpri)
        (princ "Condition Block start")
        (terpri)
        (princ "Check if E1 = E2")
        (when (equalp E1 E2) (return "NADA"))
        (terpri)
        (princ "Check if E1 is a variable")
        (terpri)
        (if (is_variable E1)
            (progn 
                (write "E1 is a variable")
                (terpri)
                (write "Check if E1 is contained in E2")
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
                (when (is_variable E2) (return (list E1 E2))
                )
            )
        )
        (return '"FALLO")
    )
)

(defun recursivity (E1 E2)
    (prog ()
        (pprint "This is when neither of them are an atom")
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