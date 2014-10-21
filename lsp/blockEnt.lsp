;; GetBlockEntities   by Lee McDonnell   [07.05.09]

;; ARGS:
;; Blk   ~  Block Name [Str]

;; RETURN:
;; List of Entities (Enames)

(defun GetBlockEntities  (Blk / tStr)
  (if (tblsearch "BLOCK" Blk)
    (GetObj (tblobjname "BLOCK" Blk))))

; Get Sub-Entities from Table Def
(defun GetObj  (bObj)
  (if (setq bObj (entnext bObj))
    (cons bObj (GetObj bObj))))