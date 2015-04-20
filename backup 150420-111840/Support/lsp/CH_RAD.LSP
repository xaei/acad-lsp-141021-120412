;Change the Radius of the selected CIRCLEs
(defun C:CH_RAD (/ get put e ss i l ans factor by_to rad)
        (setvar "cmdecho" 0)
        (defun get (nr e) (cdr (assoc nr e)))
        (defun put (x nr e)(subst (cons nr x) (assoc nr e) e))
        (if (setq ss (ssget '((0 . "CIRCLE"))))
                (progn
                        (initget "Absolute Relative")
                        (if (setq ans (getkword "\nChange Radius by: Absolute/Relative: "))
                                (progn
                                        (initget (+ 1 2 4))
                                        (if (= ans "Absolute")
                                                (setq
                                                        by_to "to "
                                                        factor (getdist "\nNew Radius: ")
                                                )
                                                (setq
                                                        by_to "by "
                                                        factor (getdist "\nChange Radius by: ")
                                                )
                                        )
                                        (princ (strcat "\nAll CIRCLE Radii will be changed " by_to (rtos factor 2 2) "."))
                                        (setq l (sslength ss) i 0)
                                        (while (< i l)
                                                (setq
                                                        e (entget (ssname ss i))
                                                        rad (get 40 e)
                                                        i (1+ i)
                                                )
                                                (if (= ans "Absolute")
                                                        (entmod (put factor 40 e))
                                                        (entmod (put (* rad factor) 40 e))
                                                )
                                        )
                                )
                        )
                )
                (princ "\nNo Circles selected.")
        )
        (princ)
)

