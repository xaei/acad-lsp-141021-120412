;; Txt2Att  ( Lee Mac )
;; Converts Single-line Text to Attribute Definition

(defun c:txt2att ( / StringSubst RemovePairs ss ent eLst str dx73 )
  (vl-load-com)
  ;; Lee Mac  ~  27.04.10

  (defun StringSubst ( new pat str )
    (while (vl-string-search pat str)
      (setq str (vl-string-subst new pat str))
    )
    str
  )

  (defun RemovePairs ( lst pairs )
    (vl-remove-if
      (function
        (lambda ( pair )
          (vl-position (car pair) pairs)
        )
      )
      lst
    )
  )

  (if (setq ss (ssget "_:L" '((0 . "TEXT"))))
    
    ( (lambda ( i )
        
        (while (setq ent (ssname ss (setq i (1+ i))))
          (setq eLst (entget ent)
                str  (StringSubst "_" " " (cdr (assoc 1 eLst)))
                dx73 (cdr (assoc 73 eLst)))

          (setq eLst (RemovePairs eLst '( 0 100 1 73 )))

          (if (entmake (append '( (0 . "ATTDEF") ) eLst (list (cons 70    0)
                                                              (cons 74 dx73)
                                                              (cons 1   str)
                                                              (cons 2   str)
                                                              (cons 3   str))))
            (entdel ent)
          )
        )
      )
      -1
    )
  )

  (princ))