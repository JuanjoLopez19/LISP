
(defun apply_items (value items)  ; (H (? D)) (E R T (? D))
    (prog (aux substitute item)

        (when (null value) (return items))
        (when (null items) (return NIL))

        (setf substitute (first value))
        (setf aux (rest value))

		(dolist(item items) 
		(if (is_atom item)
			(if (is_variable item)
				(setf (nth (position item items) items) substitute)
			)
		)
		(if (is_list item)
			(setf (nth (position item items) items) (apply_items value item))
		)
		)
		(return items)
    )
)   