(defun load_files ()
    (load "types.lsp")
    (load "is_contained.lsp")
    (load "apply_items.lsp")
    (load "unify.lsp")

    (unify '(f (? y)a) '(f (? x) (? x)))
)