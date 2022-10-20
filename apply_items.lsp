
(defun apply_items (arg1 arg2)  ; (F (? A)) (C X F ((? A) R))
    (prog (value aux substitute item)
        (when (null arg1) (return arg2))
        (when (null arg2) (return NIL))

		(dolist (value arg1)

			(setf substitute (first value))
        	(setf aux (first(rest value)))

			(dolist (item arg2)

				(if (is_list item)
					(apply_items (list value) item)
				)

				(if (equal aux item)
					(setf (nth (position item arg2) arg2) substitute)
				)	
			)
		)
		(return arg2)
    )
)   