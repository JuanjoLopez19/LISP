
(defun apply_items (value items)  ; (H (? D)) (E R T (? D))
    (prog (aux substitute)

        (when (null value) (return items))
        (when (null items) (return NIL))

        (setf aux (first value))
        (setf substitute (rest value))
        
        (dolist (i items Nil)
            (if (is_list i) 
                (apply_items value i)
            )
        )
    )
)   



(defun apply_to (S1 P1)
	; Initialize local variables substitute_item, item_to_substitute and result_list to nil
	(prog (itemS1 itemP1 substitute_item item_to_substitute var_item result_list) 

		(cond 
			((null S1) (return P1)) 	; Finish recursivity
			((null P1) (return NIL))
		)

		(dolist (itemS1 S1) 

			(setf substitute_item (first itemS1))		    ; Get first element of itemS1 which is the substitute_item
			(setf item_to_substitute (nth 1 itemS1))	    ; Get last element of itemS1 which is the item_to_substitute

            (write substitute_item)
            (terpri)

            (write item_to_substitute)
            (terpri)

			(dolist (itemP1 P1)

				; If itemP1 is not a variable and is a list, is a list inside a list. So we apply recursivity
				(if (and (listp itemP1) (not(is_variable itemP1)) )                                 ; if-part
					(setf (nth (position itemP1 P1) P1) (apply_to (list itemS1) itemP1))            ; then-part
       			)   
                (write itemP1)
                (terpri)
				; If the items found are equal , we apply the rule 
				(if (equal itemP1 item_to_substitute)
					(setf (nth (position itemP1 P1) P1) substitute_item)          
				)
			)
		)

		(return P1) 
	)
)