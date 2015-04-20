(defun c:test1 ( / e i n s x )
    (if (setq s (ssget))
        (progn
            (setq i 0
                  n (sslength s)
            )
            (while (< i n)
                (setq e (ssname s i)
                      x (cdr (assoc 0 (entget e)))
                      i (1+ i)
                )
                (print x)
            )
        )
    )
    (princ)
)

(ssget "_X" (list '(0 . "CIRCLE") '(-4 . "<AND") '(-4 . "*,*,>") (cons 10 (list 0. 0. (- (cadr item) precision))) '(-4 . "*,*,<") (cons 10 (list 0. 0.  + (car item) precision))) '(-4 . "AND>") )))