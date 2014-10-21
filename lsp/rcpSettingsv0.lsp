;wall plans settings oneoff
(defun c:RCP_SETTINGS ()
(command "_layer" "freeze" "*A-DOOR-PANE*" "")
(command "_layer" "freeze" "*A-DOOR-SWNG*" "")
(command "_layer" "freeze" "*A-WALL-PRHT*" "")
(command "_layer" "freeze" "*STRS*" "")
(command "_layer" "freeze" "*FLOR*" "")
(command "_layer" "freeze" "*ROOF-PATT*" "")
(command "_layer" "freeze" "*EQPM*" "")
; (command "_layer" "lweight" "0.05" "*A-DEMO-PATT*" "")
; (command "_layer" "lweight" "0.05" "*A-DEMO-WALL-PATT*" "")
; (command "_layer" "lweight" "0.05" "*A-DEMO-NOTE*" "")
; (command "_layer" "lweight" "0.13" "*OVHD*" "")
(princ "lines!")
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
