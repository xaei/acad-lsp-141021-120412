;;; Distribute Horizontally

(/ olderr oldcmd oldosmode s n ej
st pqj midj xmin xmax H hj j
exj ej xj xdestj
)
(princ "\nDistribute objects Horizontally.")
(setq oldcmd (getvar "cmdecho")
oldosmode (getvar "osmode")
olderr *error*
)
(defun *error* (msg)
(if (not (member msg (list "Function cancelled")))
(princ (strcat "DV ERROR: " msg))
)
(setvar "cmdecho" oldcmd)
(setvar "osmode" oldosmode)
(setq *error* olderr)
(princ)
)
;(vl-load-com)
(if (setq s (ssget))
(progn
(setq n (sslength s)
exyzList nil
)
(while (setq ej (ssname s 0))
(setq st (ssadd))
(ssadd ej st)
(setq pqj (boundxs st)
midj (mapcar '/
(mapcar '+ (car pqj) (cadr pqj))
(list 2.0 2.0 2.0)
)
exyzList (append exyzList (list (list ej midj)))
)
(ssdel ej s)
)
(setq exyzList
(vl-sort exyzList
(function
(lambda (e1 e2) (< (car (cadr e1)) (car (cadr e2))))
)
)
xmin (car (cadar exyzList))
xmax (car (cadar (reverse exyzList)))
H (- xmax xmin)
hj (/ H (1- n))
j -1
)
(setvar "cmdecho" 0)
(setvar "osmode" 0)
(repeat n
(setq exj (nth (setq j (1+ j)) exyzList)
ej (car exj)
xyzj (cadr exj)
xdestj (list (+ xmin (* hj j)) (cadr xyzj) 0.0)
)
(command "_.move" ej "" xyzj xdestj)
)
)
(princ "\nNo object found!!!")
)
(setvar "cmdecho" oldcmd)
(setvar "osmode" oldosmode)
(setq *error* olderr)
(princ)
)