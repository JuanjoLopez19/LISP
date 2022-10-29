(defun apply_items (arg1 arg2)  
    (prog (value aux substitute item)
        (when (null arg1) (return arg2))
        (when (null arg2) (return NIL))

		(dolist (value arg1)

			(setf substitute (first value))
        	(setf aux (first(rest value)))

			(dolist (item arg2)

				(if (is_list item)
					(setf (nth (position item arg2) arg2) (apply_items (list value) item)) 
				)

				(if (equal aux item)
					(setf (nth (position item arg2) arg2) substitute)
				)	
			)
		)
		(return arg2)
    )
)   


(defun apply_items_test (arg1 arg2)
	(prog (value aux substitute item)
		(terpri)
		(princ "Check if any argument is NIL")
		(terpri)
		(when (null arg1) (prog (princ "Argument 1 is null")(terpri)(princ "Argument 2 will be returned")(terpri)(return arg2)))
		(when (null arg2) (prog (princ "Argument 2 is null")(terpri)(princ "NIL will be returned")(terpri)(return NIL)))

		(princ "None of them was NIL so will iterate over the first argument")
		(terpri)
		(dolist (value arg1)
			
			(princ "The value is: ")
			(write value)
			(terpri)

			(princ "Take the element of the subsitute")
			(terpri)
			
			(setf substitute (first value))
			(setf aux (first (rest value)))
			
			(princ "The substitute is: ")
			(write substitute)
			(terpri)
			
			(princ "The substitution is: ")
			(write aux)
			(terpri)

			(princ "Iterate over the second argument")
			(terpri)
			(dolist (item arg2)
				(princ "The item is: ")
				(write item)
				(terpri)

				(princ "Check if the item is a list to apply the function recursively")
				(terpri)
				(if (is_list item)
					(prog  (terpri) (setf (nth (position item arg2) arg2) (apply_items (list value) item)))
				)

				(princ "Item is not a list")
				(terpri)
				(princ "Check if the item is equal to the substitution")
				(terpri)

				(if (equal aux item)
					(prog  (terpri) (setf (nth (position item arg2) arg2) substitute))
				)	

				(princ "Item is not equal to the substitution")
				(terpri)
				(princ "Will continue iterating")
				(terpri)
			)
		)
		(princ "Will return the second argument when the iteration is over and the first item applyed")
		(terpri)
		(princ "The result is: ")
		(write arg2)
		(terpri)
		(terpri)

		(return arg2)
	)
)


