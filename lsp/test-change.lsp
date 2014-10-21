; Changes selected objects to Layer PL1
(defun c:test1 ()
  (change1
    (ssget) ;;selection
    "test-layer1"         ;;Layer
    )
  (princ)
)
(defun change1 ( ss lay / i e )
  ;;; ss - pickset
  ;;; lay -layer name
  (repeat (setq i (sslength ss))
    (entmod
      (subst
        (cons 8 lay)
        (assoc 8 (entget (setq e (ssname ss (setq i (1- i))))))
        (entget e)
        )
      )
    )
  ){}