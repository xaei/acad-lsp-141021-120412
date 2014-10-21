;;------------------------=={ Segs }==------------------------;;
;;                                                            ;;
;;  Divides selected objects into an LWPolyline with a        ;;
;;  specified number of segments                              ;;
;;------------------------------------------------------------;;
;;  Author: Lee McDonnell, 2010                               ;;
;;                                                            ;;
;;  Copyright © 2010 by Lee McDonnell, All Rights Reserved.   ;;
;;  Contact: Lee Mac @ TheSwamp.org, CADTutor.net             ;;
;;------------------------------------------------------------;;
(defun c:Segs ( / *error* _StartUndo _EndUndo doc ss )
  (vl-load-com)
  ;; © Lee Mac 2010
  (defun *error* ( msg ) (and doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )
  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )
  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) *segs (cond ( *segs ) ( 10 )))
  (if
    (and
      (setq ss (ssget "_:L" '((0 . "ARC,CIRCLE,LWPOLYLINE,SPLINE,LINE,ELLIPSE"))))
      (progn (initget 6)
        (setq *segs (cond ((getint (strcat "\nSpecify Number of Segments <" (itoa *segs) "> : "))) ( *segs )))
      )
    )
    (
      (lambda ( j / e inc i pts ) (_StartUndo doc)
        
        (while (setq e (ssname ss (setq j (1+ j))))
          (
            (lambda ( inc i / pts )
              
              (repeat (1+ *segs)
                (setq pts (cons (trans (vlax-curve-getPointatDist e (* (setq i (1+ i)) inc)) 0 e) pts))
              )
              (if
                (entmake
                  (append
                    (list
                      (cons 0   "LWPOLYLINE")
                      (cons 100 "AcDbEntity")
                      (cons 100 "AcDbPolyline")
                      (cons 90 (length pts))
                      (cons 38 (caddar pts))
                      (cons 70 0)
                    )
                    (vl-remove-if 'null
                      (mapcar
                        (function (lambda ( d ) (assoc d (entget e)))) '(6 8 39 48 210)
                      )
                    )
                    (mapcar (function (lambda ( a ) (cons 10 a))) pts)
                  )
                )
                (entdel e)
              )
            )
            (/ (vlax-curve-getDistatParam e (vlax-curve-getEndParam e)) (float *segs)) -1
          )
        )
        (_EndUndo doc)
      )
      -1
    )
  )
  (princ)
)