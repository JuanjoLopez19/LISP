(defun compose_items (l1 l2)
    (prog (composed)
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

(defun compose_items_test (l1 l2)
    (prog (composed)
        (princ "First of all apply the items of the first list to the second list")
        (terpri)
        (terpri)
        (setf composed (apply_items l2 l1))

        (princ "When is applyed it's time to iterate over the new list")
        (terpri)
        (terpri)

        (dolist (iteml1 composed)
            (princ "The item is: ")
            (princ iteml1)
            (terpri)

            (princ "Then we get the first and the rest element of the item")
            (terpri)
            (setf sustitutol1 (first iteml1))
            (setf sustituidol1 (nth 1 iteml1))

            (princ "The first element is: ")
            (princ sustitutol1)
            (terpri)
            (princ "The rest element is: ")
            (princ sustituidol1)
            (terpri)
            (terpri)

            (princ "Now we iterate over the second list")
            (terpri)
            (dolist (iteml2 l2)
                (princ "Then we get the first and the rest element of the item")
                (terpri)
                (setf sustitutol2 (first iteml2))
                (setf sustituidol2 (nth 1 iteml2))

                (princ "The first element of list2 is: ")
                (princ sustitutol2)
                (terpri)
                (princ "The rest element of list2 is: ")
                (princ sustituidol2)
                (terpri)
                (terpri)

                (princ "If the first element of list1 is equal to the rest element of list2")
                (if (equal sustitutol1 sustituidol2)
                    (delete iteml2 l2)
                )
            )

        )
        (princ "the original list to compose is: ")
        (write composed)
        (terpri)
        (terpri)

        (princ "the list composed is: ")
        (setq composed (append composed l2))
        (write composed)
        (terpri)
        (terpri)

        (return composed)
    )
)


