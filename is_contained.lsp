(defun is_contained (value to_check)
    (if (is_list to_check) ; Check if is list --> T = do loop || F = return NIL
        (dolist (i to_check Nil) ; Loop in the list
            (if (equal value i) (return T)
                (if (is_list i)
                    (if (is_contained value i) (return T) NIL)
                )
            )
        )
        NIL
    )
)
