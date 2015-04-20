; Adds a Prefix or Suffix to the selected TEXT
;=============================================================================
(defun PSAD (/ ans ss addit val put)
	(defun val (nr e) (cdr (assoc nr e)))
	(defun put (x nr e)(subst (cons nr x) (assoc nr e) e))
	(defun addit (p_s / addstr e i)
		(setq i 0)
		(if (setq addstr (getstring t (strcat "\n" p_s ": ")))
			(repeat (sslength ss)
				(setq
					e (entget (ssname ss i))
					i (1+ i)
				)
				(cond
					((= p_s "Prefix")(entmod (put (strcat addstr (val 1 e)) 1 e)))
					((= p_s "Suffix")(entmod (put (strcat (val 1 e) addstr) 1 e)))
				)
			)
		)
	)
	;; -- Start C:PFADD
	(setvar "CMDECHO" 0)
	(princ "\nSelect TEXT to add a Prefix or Suffix to.")
	(if	(setq ss (ssget '((0 . "TEXT"))))
		(progn
			(initget "Prefix Suffix")
			(if (setq ans (getkword "\nAdd: Prefix/Suffix: "))
				(addit ans)
			)
		)
	)
	(princ)
)
(psad)
