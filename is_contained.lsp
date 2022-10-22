(defun is_contained (value to_check)
    (if (is_list to_check) 
        (dolist (i to_check Nil) 
            (if (equal value i) (return T)
                (if (is_list i)
                    (if (is_contained value i) (return T) NIL)
                )
            )
        )
        NIL
    )
)
