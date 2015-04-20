(setq X1 (apply 'min (mapcar 'car PointList)))
; The smallest 'X' value
(setq Y1 (apply 'min (mapcar 'cadr PointList)))
; The smallest 'Y' value
(setq X2 (apply 'max (mapcar 'car PointList)))
; The largest 'X' value
(setq Y2 (apply 'max (mapcar 'cadr PointList)))
; The largest 'Y' value
;or
(setq LowerLeft (List X1 Y1))
(setq LowerRight (List X2 Y1))
(setq UpperRight (List X2 Y2))
(setq UpperLeft (List X1 Y2))
;or
(setq CPWin (list (List X1 Y1) (list X2 Y1)
	    (list X2 Y2) (list X1 Y2)))
;A list of 4 points, can be passed to a 'ssget' function