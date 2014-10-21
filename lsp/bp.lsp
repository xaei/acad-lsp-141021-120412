;;; BP.LSP  - Batch processing 
;;;
;;; This routine can batch process drawing files in a directory.
;;; You can edit the following command line for your needs.
;;; For example, you can use this routine to batch audit drawings
;;; or batch convert drawings from one AutoCAD version to another.
;;; Written by Yuqun Lian - SimpleCAD  http://www.simplecad.com
;;; 09/09/97 - Initial version
;;; 08/19/98 - Add select dialog box, also fix long file and path problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *process 
;; Commands will be run after the drawing is open - can be edited
;--------------------------------------------------------------------;
;  "AUDIT Y"              ; for auditing  drawings
   "ZOOM E PLOT D 0"      ; for ploting  drawings
;--------------------------------------------------------------------;
) 

(defun C:BP(/ afile listpath tempfile filelist scrfile fp file)
   (setq afile (getfiled (strcat "Select a file in a desired "
             "directory for multiple selecting") "" "DWG" 0))
   (if afile
     (progn
       (setq listpath (get_path afile))
       (setq tempfile (strcat "\"" listpath "temp.dwg\""))
       (if (findfile tempfile)
         (command "_SAVE" tempfile "y")
         (command "_SAVE" tempfile)
       )
       (setq filelist (select_list (get_file_list listpath)))
     )
     (alert "You need to pick a file in the directory!")
   )

  (if filelist
   (progn
     (setq scrfile (strcat listpath "temp.scr"))
     (setq fp (open scrfile "w"))
     (foreach file filelist
       (write-line (strcat "_OPEN " "\"" listpath file "\"") fp)
       (write-line (strcat *process " _QSAVE") fp)
     )
     (close fp)
     (command "_SCRIPT" scrfile)
  ) ;progn
 ) ;if
 (princ)
)

;; return multiple selections in a list box
(defun select_list(from_list / lst_dcl select lst_mode)
     (setq lst_dcl (load_dialog "bp.dcl"))
     (if (not (new_dialog "bp_dcl" lst_dcl))(exit))
     (start_list "file_lst")
     (mapcar 'add_list from_list)
     (end_list)
     (action_tile "file_lst" "(setq select $value)")
     (action_tile "lst_ok" "(done_dialog 1)")
     (action_tile "lst_cancel" "(done_dialog 2)")
     (setq lst_mode (start_dialog))
     (unload_dialog lst_dcl)
     (if (= lst_mode 1)
       (if select
          (mk_list select from_list)
       )
     ) 
)

;; return to_list from from_list according to select value
(defun mk_list (select from_list / count item to_list)
  (setq count 1)
  (while (setq item (read select))
     (setq to_list (cons (nth item from_list) to_list))
     (while (and (/= " " (substr select count 1))
        (/= "" (substr select count 1)))
       (setq count (1+ count))
     )
     (setq select (substr select count))
   )
   (reverse to_list)
)


;; Return file list in listpath directory with extention ext
(defun get_file_list (listpath / listfile fp fileline linelen
                     startnum filelist scr file extention filename c)
   (setq listfile "temp.txt")
   (setq listfile (strcat listpath listfile))
   (setq fp (open listfile "w"))
   (write-line "File list for this directory:" fp)
   (close fp)
   (setq filelist '())
   (setq scr (strcat "dir \"" listpath "\" > \"" listfile "\""))
   (command "_shell" scr)
   (command "_delay" 2000)
   (setq fp (open listfile "r"))
   (while (setq fileline (read-line fp))
      (setq linelen (strlen fileline))

      ;; for windows 3.1 file name start position
      (setq startnum 40)  
      (if (or (= (substr fileline startnum 1) ":")	 ; for Windows 95
              (= (substr fileline (1+ startnum) 1) ":")	 ; for Windows NT
          )
          (setq startnum 45)  
      )

      ;; for windows 3.1 or windows 95 long file system     
      (if (> linelen (+ startnum 3))
        (progn
           (setq file (substr fileline startnum (1+ (- linelen startnum))))
           (setq extention (substr file (- (strlen file) 2) 3))
        )
        ;else for windows 95 without long file
        (progn
          (if (> linelen 12)
           (progn
             (setq extention (substr fileline 10 3))
	     (setq file (strcat (substr fileline 1 8) "." extention))
             (setq file (delete_space file))
           )
           (setq extention "")
          )
        );progn
       )

       ; if file has extention "dwg" add to file list
       (if (= (strcase extention) "DWG")
           (setq filelist (append filelist (list file)))
       )
   ); end while
   (close fp)
   (setq filelist filelist)
)

;; delete space in file name
(defun delete_space(in_file / out_file file_len count c)
   (setq file_len (strlen in_file) count 1 out_file "")
   (while (<= count file_len)
     (setq c (substr in_file count 1))
     (if (/= c " ")
       (setq out_file (strcat out_file c))
     )
     (setq count (1+ count))
   )
   (eval out_file)
)

(defun get_name (full_str / count full_count not_found)
  (if full_str (progn
    (setq count (strlen full_str))
    (setq full_count count)
    (setq not_found T)
    (while not_found
    (if (or(= (substr full_str count 1) "\\")
           (= (substr full_str count 1) "/"))
      (setq not_found nil)
      (setq count (- count 1))) 
     )
    (substr full_str (1+ count)  (- full_count 1) )
   )(eval "")) ;progn if
)

(defun get_path(fullname / path name)
   (setq name (get_name fullname))
   (setq path (substr fullname 1 (- (strlen fullname) (strlen name)))) 
)

(princ "\nType BP to run batch processing.")(princ)