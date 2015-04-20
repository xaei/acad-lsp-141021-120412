;*******************************************************
; 2011
; Command line interface for changing UI colors for AutoCAD for Mac
; Author : Maxim Kanaev
; MARSS
; E-mail maximkanaev@gmail.com
; Command: UICOLOR
;

;*******************************************************

(defun c:uicolor ( / GetOleColor GetRGB old_color old_color_rgb color_for_dialog new_color new_color_rgb new_color_1 OLE_COLOR_LIST ACI_color->OLE_color c_type)
;*************************************************************
;From JTB World http://www.jtbworld.com/lisp/DisplayColorProperties.htm
;;; Miscellaneous commands related to Colors on the Display tab on the Options dialog
;;; By Jimmy Bergmark
(defun ACI_color->OLE_color (aci)
;;  ; black is 0
;;  ; white is 7
(if (and (>= aci 0) (<= 255))
(setq aci (nth aci OLE_COLOR_LIST)
   )
  )
)
;**************************************************************
;Color Conversion Functions from http://forum.dwg.ru/showthread.php?t=8264&page=1
;
;Александр Ривилис показал функции для RGB>>OLEColor<<RGB

(defun GetOleColor ( r g b )
  (+ r (lsh g 8) (lsh b 16))
)
;;;(GetOleColor 55 88 15)  -> 1005623

(defun GetRGB ( Olecolor )
 (list
  (logand Olecolor 255)            ;; R
  (logand (lsh Olecolor -8) 255)  ;; G
  (logand (lsh Olecolor -16) 255)  ;; B
 )
)
;;;(GetRGB 1005623)        -> (55 88 15)

;*******************************************************
(initget 1 "1 2 3 4 5")
 (setq c_type (getkword "\nChange color for [1 - Model autosnap marker/2 - Layout autosnap marker/3 - Model tooltip background/4 - Layout tooltip background/5 - Reset all colors]: "))
 
;Get current colors and convert it to integer
(cond
((eq c_type "1") (setq old_color (atoi (getenv "Model AutoSnap Color"))))
((eq c_type "2") (setq old_color (atoi (getenv "Layout AutoSnap Color"))))
((eq c_type "3") (setq old_color (atoi (getenv "Model bk color"))))
((eq c_type "4") (setq old_color (atoi (getenv "Layout bk color"))))
((eq c_type "5") (setq old_color nil))
);end cond
(if old_color
(progn
;Convert color to RGB list
(setq old_color_rgb (getrgb old_color))
;Create decimal value for color dialog from reversed RGB list
(setq color_for_dialog (cons 420 (GetOleColor (caddr old_color_rgb) (cadr old_color_rgb) (car old_color_rgb) )))
;Call color dialog with decimal value from reversed RGB list
(if (setq new_color (acad_truecolordlg color_for_dialog nil))
;(setq new_color (acad_truecolordlg '(420 . 1)))
(progn 
;Convert decimal color number to reverced for writing to Preferences file
(if (cdr (assoc 420 new_color))
(progn (setq new_color_rgb (getrgb (cdr (assoc 420 new_color))))
(setq new_color_1 (GetOleColor (caddr new_color_rgb) (cadr new_color_rgb) (car new_color_rgb) ));color to write to Registry
;;;;;;Set new color for autosnap marker
;;;;;**************************(setenv "Model AutoSnap Color" (rtos new_color_1))
);enf progn
(progn
;;;Color list for (ACI_color->OLE_color) JTB World
(setq OLE_COLOR_LIST
       '(0          255        65535      65280      16776960
         16711680   16711935   16777215   8421504    12632256
         255        8421631    166        5460902    128
         4210816    76         2500172    38         1250086
         16639      8429567    10662      5466278    8320
         4214912    4940       2502732    2598       1251366
         33023      8437759    21414      5471398    16512
         4219008    9804       2505036    4902       1252646
         49151      8445951    31910      5476774    24704
         4223104    14668      2507596    7462       1253670
         65535      8454143    42662      5482150    32896
         4227200    19532      2509900    9766       1254950
         65471      8454111    42620      5482129    32864
         4227184    19513      2509891    9757       1254945
         65408      8454079    42579      5482108    32832
         4227168    19494      2509881    9747       1254941
         65344      8454047    42537      5482088    32800
         4227152    19475      2509872    9738       1254936
         65280      8454016    42496      5482067    32768
         4227136    19456      2509862    9728       1254931
         4259584    10485632   2729472    6858323    2129920
         5275712    1264640    3165222    665088     1582611
         8453888    12582784   5481984    8169043    4227072
         6324288    2509824    3755046    1254912    1910291
         12582656   14679936   8168960    9545299    6324224
         7372864    3755008    4410406    1910272    2172435
         16776960   16777088   10921472   10921555   8421376
         8421440    5000192    5000230    2500096    2500115
         16760576   16768896   10910720   10916179   8413184
         8417344    4995328    4997926    2497792    2498835
         16744448   16760704   10900224   10910803   8404992
         8413248    4990464    4995366    2495232    2497811
         16728064   16752512   10889472   10905683   8396800
         8409152    4985600    4993062    2492928    2496531
         16711680   16744576   10878976   10900307   8388608
         8405056    4980736    4990502    2490368    2495251
         16711744   16744607   10879017   10900328   8388640
         8405072    4980755    4990512    2490378    2495256
         16711808   16744639   10879059   10900348   8388672
         8405088    4980774    4990521    2490387    2495261
         16711871   16744671   10879100   10900369   8388704
         8405104    4980793    4990531    2490397    2495265
         16711935   16744703   10879142   10900390   8388736
         8405120    4980812    4990540    2490406    2495270
         12517631   14647551   8126630    9524134    6291584
         7356544    3735628    4400716    1900582    2167590
         8388863    12550399   5439654    8147878    4194432
         6307968    2490444    3745356    1245222    1905446
         4194559    10453247   2687142    6837158    2097280
         5259392    1245260    3155532    655398     1577766
         5526612    5987163    10000536   12303291   14540253
         16777215
        )
)
(setq new_color_1 (ACI_color->OLE_color (cdr (assoc 62 new_color))))
);end progn
);end if
);end progn
(princ)
);end if
);end progn
(alert "All colors restored!")
);enf if old_color
;Set new colors
(cond
((and new_color (eq c_type "1")) (setenv "Model AutoSnap Color" (rtos new_color_1)) (princ "\nModel AutoSnap color changed."))
((and new_color (eq c_type "2")) (setenv "Layout AutoSnap Color" (rtos new_color_1)) (princ "\nLayout AutoSnap color changed."))
((and new_color (eq c_type "3")) (setenv "Model bk color" (rtos new_color_1)) (princ "\nModel tooltip background color changed."))
((and new_color (eq c_type "4")) (setenv "Layout bk color" (rtos new_color_1)) (princ "\nLayout tooltip background color changed."))
((eq c_type "5") (setenv "Model AutoSnap Color" "104449") (setenv "Layout AutoSnap Color" "117761") (setenv "Model bk color" "10066329") (setenv "Layout bk color" "16777215"))
)
(princ)
)
(princ "\nUser Interface Colors v.1.0 loaded. Type UICOLOR to change UI colors.")
