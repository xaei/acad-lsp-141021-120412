(defun c:plvertex (/ elst vnum vxcd vycd)
	(setq elst (entget (car (entsel "\nSelect Polyline: "))))
	(setq vnum 0)
	(if (= (cdr (assoc 0 elst)) "LWPOLYLINE")
		(while (setq elst (member (assoc 10 elst) elst))
			(setq vnum (+ vnum 1))
			(setq vxcd (cadr (car elst)))
			(setq vycd (caddr (car elst)))
			(princ (strcat "\nPolyline Vertex #" (itoa vnum) " X,Y Coordinates: " (rtos vxcd) ", " (rtos vycd)))
			(setq elst (cdr elst))
		)
		(princ "\nObject is not a 2D LWPolyline")
	)
	(princ)
)