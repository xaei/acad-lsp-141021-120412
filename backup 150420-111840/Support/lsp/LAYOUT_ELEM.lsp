;*******************************************************
; 2012
; Command line interface for changing Layout elements for AutoCAD for Mac
; Author : Maxim Kanaev
; MARSS
; E-mail maximkanaev@gmail.com
; Command: L_ELEM
;

;*******************************************************

(defun c:L_ELEM ( / lelem_err old_cmd old_dynprompt old_dynmode string_1 string_2 string_3 le_type)

;Error handler
(defun lelem_err (s)
  (if (/= s "Function cancelled")
   (princ (strcat "\nError: " s))
  )
  (if old_dynmode (setvar "DYNMODE" old_dynmode))
  (if old_dynprompt (setvar "DYNPROMPT" old_dynprompt))
  (if old_cmd (setvar "cmdecho" old_cmd))
  (if olderr (setq *error* olderr))
(princ)
)
(setq olderr *error*
      *error* lelem_err)
;*****************************************

;*****************************************************************
;Enable Dynamic prompt
(setq old_dynmode (getvar "DYNMODE")
      old_dynprompt (getvar "DYNPROMPT"))
(setvar "DYNMODE" 3)
(setvar "DYNPROMPT" 1)
;************************************************************
(setq old_cmd (getvar "cmdecho"))
(setvar "cmdecho" 0)
;------------------------------

;*******************************************************
(if (eq (getenv "ShowPaperMargins") "1")
(setq string_1 "Hide printable area")
(setq string_1 "Display printable area")
)

(if (eq (getenv "ShowPaperBackground") "1")
(setq string_2 "Hide paper shadow")
(setq string_2 "Display paper shadow")
)

(if (eq (getenv "ShowPrintBorder") "1")
(setq string_3 "Hide paper background")
(setq string_3 "Display paper background")
)

(initget "1 2 3 X x")
(setq le_type (getkword (strcat "\nShow-Hide Layout Element [1 - " string_1 "/2 - " string_2 "/3 - " string_3 "/eXit] <eXit>: ")))
;Get current states and convert it to integer

(if (eq le_type "")
(setq le_type "EXIT")
)

(cond
((eq le_type "1") (progn (if (eq string_1 "Hide printable area") (setenv "ShowPaperMargins" "0") (setenv "ShowPaperMargins" "1"))))
((eq le_type "2") (progn (if (eq string_2 "Hide paper shadow") (setenv "ShowPaperBackground" "0") (setenv "ShowPaperBackground" "1"))))
((eq le_type "3") (progn (if (eq string_3 "Hide paper background") (setenv "ShowPrintBorder" "0") (setenv "ShowPrintBorder" "1"))))
((eq le_type "EXIT") (princ))
);end cond



(if (and (eq (getvar "CTAB") "Model") (not (eq le_type "EXIT")))
(princ)
(command-s "REGENALL")
)
;Restore variables
(if old_dynmode (setvar "DYNMODE" old_dynmode))
(if old_dynprompt (setvar "DYNPROMPT" old_dynprompt))
(if old_cmd (setvar "cmdecho" old_cmd))
(if olderr (setq *error* olderr))
(princ)
);end defun C:

(prompt "\n Type L_ELEM to change Layuot Elements.")
(princ)
