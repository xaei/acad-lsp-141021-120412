;;; ARCL.LSP - Arc Leader  (c) 1998, Yuqun Lian and Dan Crane II
;;; This routine draws an arc leader. The arc size is directly related to
;;; the dimasz variable, however, it prompts for a size and defaults to 
;;; dimasz. Larger sizes useful for multiple pipe rack widths.
;;;
;;; Modified from SW.LSP by DanCrane II, Bechard and Associates, Inc (4/98)
;;; SW.LSP author: Yuqun Lian - SimpleCAD, http://www.simplecad.com
;;; ------------------------------------------------------------------------
(defun C:ARCL (/ tempcmd templt tempplw temportho tempblip 
             aleng awidth pt1 pt2 ent1 ent2 ang ang2 pt3 pt4 dst size)
     (setq tempcmd (getvar "cmdecho"))
     (setq templt (getvar "celtype"))
     (setq tempplw (getvar "plinewid"))
     (setq temportho (getvar "orthomode"))
     (setq tempblip (getvar "blipmode"))
     (setvar "cmdecho" 0)
     (setvar "celtype" "bylayer")
     (setvar "orthomode" 0)
     (setvar "blipmode" 0)
     ; set arrow size
     (setq aleng (* (getvar "dimasz") (getvar "dimscale")))
     (setq awidth (/ aleng 3.))
     (command "color" "1")
     (if (setq pt1 (getpoint "\nLeader start:"))
        (progn
               (initget 1)
               (setq pt2 (getpoint pt1 "\nTo point: "))
               (command "color" "1" "line" pt1 pt2 "")
               (setq ent1 (entlast))
               (setq ang (angle pt1 pt2))
               (setq ang2 (polar pt1 (- ang 1.2) (* aleng 2.1)))
               (setvar "orthomode" 1 )
               (setq pt3 (getpoint  pt2 "\nTo point:"))
               (setvar "orthomode" 0 )
               (setq size (getdist "\nLoop size:(1-10) "))
               (if (null size)
                  (setq size 1)
               )
               (setq dst (* aleng size))
               (setq pt5 (polar pt1 ang dst))
               (if (null pt3)
                  (setq pt3 pt2)
               )
               (entdel ent1)
               (setvar "fillmode" 1)
               (command "color" "1" "pline" pt3 "w" "0" "0" pt2 pt1
                        "a" "d" ang2 pt5 "w" "0" "0" "")
         ) ;progn
     ) if pt1
     (setvar "orthomode" temportho)
     (setvar "celtype" templt)
     (setvar "plinewid" tempplw)
     (setvar "blipmode" tempblip)
     (setvar "cmdecho" tempcmd)
     (princ)
     (command "color" "bylayer")
) ;end arcl
(prompt "\nType ARCL to draw straight arrow leader")
(princ)