;dxf text boundbox codes
(setq bxr (entget (entlast))
ins (cdr (assoc 10 bxr))
txj (cdr (assoc 71 bxr))
txr (cdr (assoc 50 bxr))
addr (* 0.09 (getvar "dimscale"))
lg (+ addr (/ (cdr (assoc 42 bxr)) 2.0))
ht (+ addr (/ (cdr (assoc 43 bxr)) 2.0))
)