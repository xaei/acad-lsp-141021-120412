(setq car-func
(cond
((= carnum 3) car)
((= carnum 4) cadr)
((= carnum 5) caddr)
((= carnum 6) cadddr)
) ; cond
) ; 
(setq flevel (car-func (xyval obj)))
(repeat (vla-get-count ssobj)
(setq obj (vla-item ssobj (setq j (1+ j))))
(setq enxy (car-func (xyval obj)))
(setq dist (- flevel enxy) base-pt '(0 0 0));