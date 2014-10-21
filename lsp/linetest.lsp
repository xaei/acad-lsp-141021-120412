(setq linelist '(("Dashed" . "acad.lin")
                 ("Hidden" . "acad.lin")
                 ))
(foreach  lin linelist
  (if (tblsearch "LTYPE" (car lin))
    (command ".-linetype" "_Load" (car lin) (cdr lin) "_Yes" "")
    (command ".-linetype" "_Load" (car lin) (cdr lin) "")
  )
)