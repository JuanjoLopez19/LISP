(defun compose_items (l1 l2)
    (prog(composed)
        (setf composed (apply_items l2 l1))
        (dolist (iteml1 composed)
            (setf sustitutol1 (first iteml1))
            (setf sustituidol1 (nth 1 iteml1))
            (dolist (iteml2 l2)
                (setf sustitutol2 (first iteml2))
                (setf sustituidol2 (nth 1 iteml2))
                (if (equal sustitutol1 sustituidol2)
                    (delete iteml2 l2)
                )
            )
        )
        (setq composed (append composed l2))
        (return composed)
    )
)


