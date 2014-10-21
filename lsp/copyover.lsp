;;;hacky ctb stb switcher
(defun c:COPYOVERM()
;(setq oldf "/Users/this/Desktop/mill diagram 1.dwg")
;(command "open" oldf)
(command "model")
;copyover testing
(setq sel1 (ssget "x"))
(command "._copybase" "0,0" (sel1) "")
;get name of current model or layout tab
(setq c1 (getvar "ctab"))
;(setvar "ctab" c1)
(foreach x (layoutlist)
	(setvar 'ctab x)
	(princ "iterate!")
	(setq sel2 (command "._copybase" "0,0" sel1 ""))
);end of foreach
)

;(command "_ai_selall")