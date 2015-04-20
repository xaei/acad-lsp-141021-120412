;;;===========================================
 
;;;      Single Line Break Symbol Creator
 
;;;===========================================
 
;;;
 
;|     Created by C. Alan Butler  2003
 
Yet another Break symbol creator
 
Uses PloyLine created on the current layer
 
Ortho Mode is up to you to pre set or not
 
Symbol is proportional to the length
 
and doesn't break anything
 
|;
 
;
 
(defun c:brkl (/ p1 p2 p3 p4 p5 p6 dist ang usercmd)
 
;
 
; error function & Routine Exit
 
(defun *error* (msg)
 
(if
 
(not
 
(member
 
msg
 
'("console break" "Function cancelled" "quit / exit abort" "")
 
)
 
)
 
(princ (strcat "\nError: " msg))
 
) ; if
 
(setvar "CMDECHO" usercmd)
 
(setvar "osmode" useros)
 
(princ)
 
) ;
 
;end error function
 
(setq oldcmd (getvar "CMDECHO"))
 
(setvar "CMDECHO" 0)
 
(setq useros (getvar "osmode"))
 
(setq usercmd (getvar "CMDECHO"))
 
(setvar "CMDECHO" 0)
 
(setvar "plinewid" 0)
 
(if (and (setq p1 (getpoint "Starting point of line : "))
 
(setq p6 (getpoint p1 "\nSelect second point: "))
 
)
 
(progn
 
(setq dist (distance p1 p6)
 
ang  (angle p1 p6)
 
p2   (polar p1 ang (* 0.4167 dist))
 
p5   (polar p1 ang (* 0.5833 dist))
 
p3   (polar p2 (+ 1.25664 ang) (* 0.1667 dist))
 
p4   (polar p5 (+ 4.39824 ang) (* 0.1667 dist))
 
) ;_ end of setq
 
(setvar "osmode" 0)
 
(command "pline" p1 p2 p3 p4 p5 p6 "") ; Draw the Z-Line
 
)
 
)
 
(*error* "")
 
(princ)
 
)
 
(prompt
 
"\nBreak Symbol Creator loaded.  Type BRKL to run it."
 
)
 
(princ)