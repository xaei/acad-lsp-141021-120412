
; This returns a list of tags and thier values that are in the desired block
(defun gettaglist ( ent / eg taglist )
(while (and
       (setq ent (entnext ent) eg (entget ent))
       (not (= "SEQEND" (CDR (ASSOC 0 EG))))
	   )
 (if (= "ATTRIB" (cdr (assoc 0 eg)))(setq taglist (cons (cons (cdr (assoc 2 eg)) (cdr (assoc 1 eg))) taglist)))
)
taglist
);d


; this will apply a list of tag values to a desired block.
(defun applytaglist (taglist ent / eg origent)
(while (and
       (setq ent (entnext ent) eg (entget ent))
       (not (= "SEQEND" (CDR (ASSOC 0 EG))))
	   )
 (if (and
     (= "ATTRIB" (cdr (assoc 0 eg)))
	 (assoc (cdr (assoc 2 eg)) taglist)
	 )
(entmod (subst (cons 1 (cdr (assoc (cdr (assoc 2 eg)) taglist))) (assoc 1 eg) eg))
)
)
(princ));d
