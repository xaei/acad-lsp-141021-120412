       ;; Get a selection set of the blocks created
          (setq ss (ssget)) ;Select previous
            (progn
              (setq n 0) ;initialize counter
              (while (< n (sslength ss)) ;While counter is less than selection set's length
                (setq en (ssname ss n) ;Get the nth item from the selection set
                      ed (entget en) ;Get its DXF data list
                ) ;_ end of setq
                  (strcat (rtos (cadr (assoc 10 ed))) ;X value
                          ","
                          (rtos (caddr (assoc 10 ed))) ;Y value
                          ","
                          (rtos (cadddr (assoc 10 ed))) ;Z value
                  ) ;_ end of strcat
                 );_ end of while
                 );_ end of progrn