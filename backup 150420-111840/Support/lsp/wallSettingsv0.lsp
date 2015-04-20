;wall plans settings oneoff
(defun c:WALL_SETTINGS ()
(command "_layer" "color" "251" "*A-WALL-PATT*" "")
(command "_layer" "color" "251" "*AREA*" "")
(command "_layer" "ltype" "HIDDEN2" "*OVHD*" "")
(command "_layer" "lweight" "0.05" "*PATT*" "")
;(command "_layer" "pstyle" "50%" "*EXST*" "")
; (command "_layer" "lweight" "0.05" "*A-DEMO-PATT*" "")
; (command "_layer" "lweight" "0.05" "*A-DEMO-WALL-PATT*" "")
; (command "_layer" "lweight" "0.05" "*A-DEMO-NOTE*" "")
; (command "_layer" "lweight" "0.13" "*OVHD*" "")
(princ "lines!")
(command "LUPREC" "5")
(princ "units!")
; (if 1
; (command "_linetype" "load" "DEMO8" "acad.lin" "Y" "")
; )
; (command "_layer" "ltype" "DEMO8" "*DEMO*" "")
; (command "_layer" "ltype" "CONTINUOUS" "*A-DEMO-PATT*;*A-DEMO-WALL-PATT*" "")
; (command "_layer" "freeze" "*EQPM*" "")
; (command "_layer" "freeze" "*A-DEMO-ANNO*" "")
; (princ)
; (command "_layer" "transparency" "0" "*" "")
);end of defun
;
;
;scraps
;(command "xref" "reload" "*")
;(command "_script" "../Desktop/autolisp/demoPlansSettings.scr")
;(command "_layer" "ltype" "DEMO" "*DEMO*" "")
;(command "_layer" "ltype" "CONTINUOUS" "*A-DEMO-PATT*" "")
(princ "WALL SET!")