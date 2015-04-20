(defun c:test2 ( / e i n s x )
    (if (setq s (ssget))
        (progn
            (setq i 0
                  n (sslength s)
            )
            (repeat n
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