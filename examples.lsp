(defun probe(arg1 arg2 expect result)
    (if (equal expect result)
			(format t "Arg1:~s~%Arg2:~s~%Returns: ~s~%Expected:~s~%~%Test PASSED~%" 
				arg1 arg2 result expect
			)
			(format t "Arg1:~s~%Arg2:~s~%Returns: ~s~%Expected:~s~%Test FAIL~%"  
				arg1 arg2 result expect
			)
		)

)

(princ "Práctica realizada por: Juan José López Gómez y Roberto Merchán González")
(terpri)
(terpri)

(princ "Example 1")
(terpri)
(probe '(P (? x) A) '(P (f h) (? y)) '( ((F H) (? X)) (A (? f)) ) (unify '(P (? x) A) '(P (f h) (? y)) ) )

(terpri)
(terpri)

(princ "Example 2")
(terpri)
(probe '( (FOO (? x)) (? y) ) '( A B ) '"FALLO" (unify '( (FOO (? x)) (? y) ) '( A B ) ) )