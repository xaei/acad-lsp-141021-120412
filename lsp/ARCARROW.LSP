; TIP493B.LSP   improvement based on TIP493   (c)1990, CADalyst
(DeFun IN ()
  (SetQ VARS
    (MapCar
      '(Lambda (A B)
         (SetQ TEMP (GetVar A))
         (SetVar A B)
         (List A TEMP)
       )
       '("CmdEcho""BlipMode""DragMode")
       '(0 1 2)
    )
  )
  (Command "UNDO" "End" "UNDO" "Group")
  (GraphScr)
)

(DeFun OUT ()
  (Command "UNDO" "End" "CANCEL")
  (ForEach TEMP VARS
    (Apply 'SetVar TEMP)
  )
  (PrinC)
)

(DeFun CL-ERR (MSG)
  (PrinC "\nerror: ")
  (PrinC MSG)
  (SetQ *ERROR* ERR)
  (OUT)
)

(DeFun GET (A B)  (Cdr (Assoc A B)))

(DeFun C:CL (/ VARS ERR PT2 PT1 ASIZE
    ANG TSIZE X1 Y1 PT4)
  (SetQ ERR *ERROR* *ERROR* CL-ERR)
  (IN)
  (SetQ PT1 (GetPoint "\nStart point:"))
  (Prompt "\nPoint on arc segment: ")
  (Command "ARC" PT1 Pause)
  (Prompt "\nNext point: ")
  (Command Pause)
  (Setq
    PT3    (GetVar "LastPoint")
    DSCALE (GetVar "DimScale")
    ASIZE  (* (GetVar "DimAsz") DSCALE)
    TSIZE  (* (GetVar "DimTxt") DSCALE)
    BLOCK  (GetVar "DimBlk")
    90DEG  (/ Pi 2)
    ENT1   (EntLast)
    EDATA  (EntGet ENT1)
    CENTER (Get 10 EDATA)
    RADIUS (Get 40 EDATA)
    START  (Polar CENTER (Get 50 EDATA)
      RADIUS)
    ANG    (/ ASIZE RADIUS)
    ANG    ((If (< (Distance PT1 START)
               1.0e-6) + -)
             (Angle CENTER PT1) ANG)
    PT2    (Polar CENTER ANG RADIUS)
    ANG    (Angle PT1 PT2)
  )
  (Cond
    ((Eq BLOCK "")
      (SetQ ASIZE (* ASIZE 0.1667))
      (Command
        "SOLID" PT1 (Polar PT2 (+ ANG
           90DEG) ASIZE) (Polar PT2
           (- ANG 90DEG) ASIZE) "" ""
      )
    )
    (T (Command "INSERT" BLOCK PT1 ASIZE
          "" (AngToS (- ANG Pi))))
  )
  (Prompt "\nText: ")
  (If (<= (Nth 0 PT3) (Nth 0 PT1))
    (ProgN
      (SetQ
        X (- (Nth 0 PT3) TSIZE)
        Y (- (Nth 1 PT3) (* TSIZE 0.5))
      )
      (Command "DTEXT" "R" (List X Y)
        TSIZE 0)
    )
    (ProgN
      (SetQ
        X (+ (Nth 0 PT3) TSIZE)
        Y (- (Nth 1 PT3) (* TSIZE 0.5))
      )
      (Command "DTEXT"(List X Y)TSIZE 0)
    )
  )
  (OUT)
)
