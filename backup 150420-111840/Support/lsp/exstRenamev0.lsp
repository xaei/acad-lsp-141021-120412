;wall plans settings oneoff
(defun c:EXST_RENAME ()
(if (tblsearch "LAYER" "A-EQPM-EXST") (command "RENAME" "LAYER" "A-EQPM-EXST" "A-EXST-EQPM" "") (princ "nope"))
(if (tblsearch "LAYER" "A-FLOR-OVHD-EXST") (command "RENAME" "LAYER" "A-FLOR-OVHD-EXST" "A-EXST-FLOR-OVHD" "") (princ "nope"))
(if (tblsearch "LAYER" "A-FLOR-STRS-EXST") (command "RENAME" "LAYER" "A-FLOR-STRS-EXST" "A-EXST-FLOR-STRS" "") (princ "nope"))
(if (tblsearch "LAYER" "A-WALL-FULL-EXST") (command "RENAME" "LAYER" "A-WALL-FULL-EXST" "A-EXST-WALL-FULL" "") (princ "nope"))
(if (tblsearch "LAYER" "A-WALL-PATT-EXST") (command "RENAME" "LAYER" "A-WALL-PATT-EXST" "A-EXST-WALL-PATT" "") (princ "nope"))
(if (tblsearch "LAYER" "A-WALL-PRHT-EXST") (command "RENAME" "LAYER" "A-WALL-PRHT-EXST" "A-EXST-WALL-PRHT" "") (princ "nope"))
(if (tblsearch "LAYER" "A-ANNO-DIMS-EXST") (command "RENAME" "LAYER" "A-ANNO-DIMS-EXST" "A-EXST-ANNO-DIMS" "") (princ "nope"))
(if (tblsearch "LAYER" "A-GLAZ-EXST") (command "RENAME" "LAYER" "A-GLAZ-EXST" "A-EXST-GLAZ" "") (princ "nope"))
(if (tblsearch "LAYER" "S-EXST-OVHD") (command "RENAME" "LAYER" "S-EXST-OVHD" "A-EXST-OVHD" "") (princ "nope"))
(princ "EXST!")
);end defun 
;FIX CASE OF DESTINATION EXISTING: LAYMRG IF EXST