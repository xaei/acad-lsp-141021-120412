;;=============================================================
 
;;     Sel.lsp by Charles Alan Butler
 
;;   found at www.theswamp.org
 
;;
 
;;    Version 1.0 Beta  July 23,2004
 
;;    Version 1.1 Beta  July 13,2005
 
;;
 
;;   Creates a selection set of objects on a layer(s)
 
;;   User picks objects to determine the layer(s)
 
;;   Then User selects objects for ss or presses enter to
 
;;   get all objects on the selected layer(s)
 
;;   You may select the selection set before starting this
 
;;   routine. Then select the layers to keep in the set
 
;;=============================================================
 
(defun c:sel (/ ent lay ss lay:lst lay:prompt ss:first ent:lst)
 
;;  get anything already selected
 
(setq ss:first (cadr(ssgetfirst))
 
ss (ssadd))
 
;;  Get user selected layers
 
(if ss:first
 
(setq lay:prompt "\nSelect the object to choose layers to keep.")
 
(setq lay:prompt "\nSelect object for layer filter.")
 
)
 
(while (setq ent (entsel lay:prompt))
 
(setq ent:lst (cons (car ent) ent:lst))
 
(setq lay:lst
 
(cons (setq lay (cdr(assoc 8 (entget (car ent))))) lay:lst))
 
(prompt (strcat "\n*-* Selected Layer -> " lay))
 
)
 
;;  Un HighLite the entities
 
(and ent:lst (mapcar '(lambda (x) (redraw x 4)) ent:lst))
 
(if (> (length lay:lst) 0); got layers to work with
 
(progn
 
(setq lay "")
 
(setq lay:lst (vl-sort lay:lst '<)) ; removes douplicates
 
(foreach itm  lay:lst ; combine lay names into one , del string
 
(setq lay (strcat lay itm ",")))
 
(setq lay (substr lay 1 (1- (strlen lay)))); remove the last ,
 
(if ss:first ; ALREADY GOT SELECTION SET
 
(while (setq ent (ssname ss:first 0))
 
(if (member (cdr(assoc 8 (entget ent))) lay:lst)
 
(ssadd (ssname ss:first 0) ss)
 
)
 
(ssdel (ssname ss:first 0) ss:first)
 
)
 
(progn ; else get a selection set to work with
 
(prompt (strcat "\nOK >>--> Select objects for Selection set or "
 
"ENTER for All objects on layer(s) " lay))
 
;;  get objects using filter with user select
 
(if (null (setq ss (ssget (list (cons 8 lay)))))
 
;; or get ALL objects using filter
 
(setq ss (ssget "_X" (list (cons 8 lay))))
 
)
 
)
 
)
 
(if (> (sslength ss) 0)
 
(progn
 
(prompt (strcat "\n" (itoa (sslength ss))
 
" Object(s) selected on layer(s) " lay
 
"\nStart an ACAD command."))
 
(sssetfirst nil ss)
 
)
 
(prompt "\n***  Nothing Selected  ***")
 
)
 
)
 
)
 
(princ)
 
)
 
(prompt "\nSelect on Layer loaded, Enter Sel to run.")