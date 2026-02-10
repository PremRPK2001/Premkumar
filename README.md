# Premkumar
# CAD Automation for Grating in ACAD

;; ------------------------------------------------------------
;; Force ACI color on an entity (removes truecolor if present)
;; ------------------------------------------------------------
(defun _force-aci (e aci / ed)
  (setq ed (entget e))
  ;; Remove any existing 62 code OR add new one
  (if (assoc 62 ed)
    (setq ed (subst (cons 62 aci) (assoc 62 ed) ed))
    (setq ed (append ed (list (cons 62 aci))))
  )
  (entmod ed)
  (entupd e)
)
;;;------------------------------------------------------------
;;; Command: MGRT
;;; - Product-driven equal end-gap limits
;;; - All inner gaps fixed at 9.1 mm
;;; - Top lines over gaps (guarded to avoid duplicates)
;;; - Bottom line from inner face to inner face (ignores bar thickness)
;;; - Extra top piece as 3 lines (two vertical 10 mm, one bottom horizontal = thickness),
;;;   placed with 0.45 mm inset in the target gap, mirrored about the gap centerline,
;;;   and connected to adjacent bar top corners with lines.
;;;   (Runs only for mesh types 2, 5, 6, 8, 9)
;;; - New: one extra rectangle 2(w) x 18(h), top 13 mm below top line,
;;;        placed in center gap or, if none, the gap just left of center.
;;; - New: one 6 mm vertical centerline for the rectangle, moved 2 mm down,
;;;        and when center falls on a rib, additionally moved 4.55 mm left.
;;; - New: two short vertical lines inside the rectangle, 2 mm long, centered on
;;;        rectangle center Y, at ±offsLR where offsLR=3 mm for mesh 1,3,5,7
;;;        and offsLR=4 mm for mesh 2,4,6,8,9.
;;;------------------------------------------------------------
(defun c:MGRT (/ mesh_key mesh_name mesh_height mesh_thickness overall_width
                inner_height gap_fixed min_gap max_gap
                x_left x_right y_base avail n g_end ok x_curr
                bar_coords full_name_map all_bars
                prev_right gap_start gap_end)

  (setq inner_height 10.0)

  ;; Save/override system variables
  (setq origOSMODE (getvar "OSMODE"))
  (setq origSNAPMODE (getvar "SNAPMODE"))
  (setq origORTHOMODE (getvar "ORTHOMODE"))
  (setvar "OSMODE" 0) (setvar "SNAPMODE" 0) (setvar "ORTHOMODE" 0)

  ;; UCS World
  (command "._UCS" "W") 
  (command "DIMCLRT" 3) 
  (command "DIMSCALE" 5)
  (command "DIMGAP" 1.4)
  (command "DIMDEC" 1)
  (command "DIMEXE" 2)
  (command "TEXTSCR" )
(command "DIMTOFL" "OFF")
(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0)
        0.0)
)


  (setq full_name_map
        '((1 . ("Prib M6 TS 2 V2A/V4A" 5.0 11.0))
          (2 . ("Prib M8 TS 2 V2A/V4A" 5.0 11.0))
          (3 . ("Prib M6 TS 3 V2A/V4A" 5.0 11.0))
          (4 . ("Prib M8 TS 3 V2A/V4A" 5.0 11.0))
          (5 . ("Prib Koop. M6 TS 2 ZN/V2A" 4.55 10.0))
          (6 . ("Prib Koop. M8 TS 2 ZN/V2A" 4.55 10.0))
          (7 . ("Prib M8 ZN TS 3" 4.55 10.0))))

  (princ "\nSelect Mesh Type by number: ")
  (foreach item full_name_map
    (princ (strcat "\n" (itoa (car item)) ". " (nth 0 (cdr item))))
  )
  (setq mesh_key (getint "\nEnter selection number (1-7): "))
(setq mesh_id mesh_key)

  (if (not (assoc mesh_key full_name_map))
    (progn (princ "\nInvalid selection. Using default (1).") (setq mesh_key 1))
  )
  (setq mesh_name (nth 0 (cdr (assoc mesh_key full_name_map))))
  (setq min_gap   (nth 1 (cdr (assoc mesh_key full_name_map))))
  (setq max_gap   (nth 2 (cdr (assoc mesh_key full_name_map))))

  ;; Set gap_fixed based on mesh_key
  (cond
    ((member mesh_key '(1 2 3 4)) (setq gap_fixed 10.5))
    ((member mesh_key '(5 6 8 9)) (setq gap_fixed 9.1))
    ((= mesh_key 7)                (setq gap_fixed 8.1))
    (T                             (setq gap_fixed 9.1)) ; fallback
  )

  (setq mesh_height    (getreal "\nEnter Mesh Height for side bars: "))
  (setq mesh_thickness (getreal "\nEnter Mesh Thickness: "))
  (setq overall_width  (getreal "\nEnter Overall Mesh Width: "))
;; Determine spacing s based on mesh type selection
(cond
  ((member mesh_id '(1 2))
    (setq s (+ 31 mesh_thickness)))

  ((member mesh_id '(3 4))
    (setq s (+ 30 mesh_thickness)))

  ((member mesh_id '(5 6 8 9))
    (setq s (+ 31.3 mesh_thickness)))

  ((= mesh_id 7)
    (setq s (+ 30.3 mesh_thickness)))

  (T
    (setq s 33.3)) ; fallback
)

  (setq x_left 0.0
        y_base 0.0
        x_right (- overall_width mesh_thickness))

;; Side bars
(command "._PLINE"
         (list x_left y_base)
         (list (+ x_left mesh_thickness) y_base)
         (list (+ x_left mesh_thickness) (+ y_base mesh_height))
         (list x_left (+ y_base mesh_height))
         "C")
(setq sb1 (entlast))   ;; store first sidebar

(command "._PLINE"
         (list x_right y_base)
         (list (+ x_right mesh_thickness) y_base)
         (list (+ x_right mesh_thickness) (+ y_base mesh_height))
         (list x_right (+ y_base mesh_height))
         "C")
(setq sb2 (entlast))   ;; store second sidebar

;; -----------------------------
;; Hatch side bars
;; -----------------------------
(if (and sb1 sb2)
  (progn
    (command "._HATCH" "P" "ANSI31" "1" "0" "S" sb1 sb2 "" "")
    (prompt "\n✔ Side bars hatched.")
  )
  (prompt "\n⚠ ERROR: Side bar polylines not captured.")
)


  ;; Inner span and layout
  (setq avail (- overall_width (* 2.0 mesh_thickness)))
  (defun _g_end (n) (/ (- avail (* n mesh_thickness) (* (max 0 (1- n)) gap_fixed)) 2.0))

  (setq n (fix (/ (+ avail gap_fixed) (+ mesh_thickness gap_fixed))))
  (if (< n 0) (setq n 0))

  (setq ok nil)
  (while (and (not ok) (>= n 0))
    (setq g_end (_g_end n))
    (if (and (>= g_end min_gap) (<= g_end max_gap))
      (setq ok T)
      (setq n (1- n))
    )
  )
  (if (not ok)
    (progn (setq n 0) (setq g_end (max min_gap (min max_gap (_g_end 0))))))

;; Inner bars + Hatching
(setq x_curr (+ x_left mesh_thickness g_end))
(setq bar_coords '())

(setq totalBars n)
(setq isEvenBars (= (rem totalBars 2) 0))

;; ----------------------------------------------------------
;; middle indices
;; EVEN: 10 -> 4,5
;; ODD : 11 -> 4,5  (5th & 6th bars)
;; ----------------------------------------------------------
(if isEvenBars
  (progn
    (setq midRight (/ totalBars 2))
    (setq midLeft  (1- midRight))
  )
  (progn
    (setq midRight (fix (/ totalBars 2)))  ;; 11 -> 5
    (setq midLeft  (1- midRight))          ;; 4
  )
)

(setq barIndex 0)

(repeat n
  ;; Draw bar rectangle
  (command "._PLINE"
           (list x_curr (+ y_base mesh_height))
           (list (+ x_curr mesh_thickness) (+ y_base mesh_height))
           (list (+ x_curr mesh_thickness) (- (+ y_base mesh_height) inner_height))
           (list x_curr (- (+ y_base mesh_height) inner_height))
           "C")
  (setq barRect (entlast))

;; Hatch it (skip center bar ONLY for mesh type 4 and 7)
(setq barHatch nil)
(setq skipIndex nil)

;; skipping only for mesh types 4 and 7
(if (member mesh_key '(4 7))
  (progn
    ;; center bar index (0-based)
    ;; even 10 -> 4 (5th bar)
    ;; odd  5  -> 2 (3rd bar)
    (setq skipIndex (fix (/ (1- totalBars) 2.0)))
  )
)

(if (and barRect (or (null skipIndex) (/= barIndex skipIndex)))
  (progn
    (command "._HATCH" "P" "ANSI31" "1" "0" "S" barRect "" "")
    (setq barHatch (entlast))
  )
)


  ;; ----------------------------------------------------------
  ;; Move middle 2 hatches (works for even + odd)
  ;; midLeft  -> move LEFT  2.45
  ;; midRight -> move RIGHT 2.45
  ;; ----------------------------------------------------------
(if (and (member mesh_key '(2 5 6)) barHatch)

    (progn
      (if (= barIndex midLeft)
        (command "._MOVE" barHatch "" '(0 0) (list -2.45 0.0))
      )
      (if (= barIndex midRight)
        (command "._MOVE" barHatch "" '(0 0) (list 2.45 0.0))
      )
    )
  )

  ;; store coords
  (setq bar_coords (cons (list x_curr (+ x_curr mesh_thickness)) bar_coords))

  ;; next bar
  (setq x_curr (+ x_curr mesh_thickness gap_fixed))
  (setq barIndex (1+ barIndex))
)

(setq bar_coords (reverse bar_coords))


;; ----------------------------------------------------------
;; COLOR MIDDLE BAR FOR mesh 4 and 7
;; If odd: color exact center
;; If even: color left-center
;; ----------------------------------------------------------
(if (member mesh_key '(4 7))
  (progn
    (setq totalBars (length bar_coords))

    ;; Determine middle index
    (if (= (rem totalBars 2) 1)
      (setq midIndex (fix (/ totalBars 2)))        ;; odd → center
      (setq midIndex (1- (/ totalBars 2)))         ;; even → left-middle
    )

    ;; Get the bar span (x1 x2)
    (setq midBar (nth midIndex bar_coords))
    (setq x1 (car midBar))
    (setq x2 (cadr midBar))

    ;; Select the bar and color it yellow BEFORE explode
    (setq ssMid
      (ssget "_W"
             (list x1 (- y_base 500))
             (list x2 (+ y_base mesh_height 500))
      )
    )

    (if ssMid
      (command "._CHPROP" ssMid "" "COLOR" "2" "")
    )
  )
)

  ;; All bars including side bars
  (setq all_bars (cons (list x_left (+ x_left mesh_thickness)) bar_coords))
  (setq all_bars (append all_bars (list (list x_right (+ x_right mesh_thickness)))))

  ;; Top lines over gaps (guard against duplicates/degenerate spans)
  (setq prev_right (cadr (car all_bars)))
  (foreach bc (cdr all_bars)
    (setq gap_start prev_right)
    (setq gap_end   (car bc))
    (if (> (- gap_end gap_start) 1e-8)
      (command "._LINE"
               (list gap_start (+ y_base mesh_height))
               (list gap_end   (+ y_base mesh_height))
               "")
    )
    (setq prev_right (cadr bc))
  )
  ;; Optional cleanup for duplicates (uncomment if desired)
  ;; (command "._OVERKILL" "_ALL" "" "")

  ;; Bottom inner-face line
  (command "._LINE"
           (list mesh_thickness y_base)
           (list (- overall_width mesh_thickness) y_base)
           "")

  ;; ----------------------------------------------------------
  ;; Extra 3-line piece + mirror + top connectors (valid for 2,5,6,8,9)
  ;; ----------------------------------------------------------
  (defun _place_extra_lines_mirror_connect (/ cx hit idx bar
                                              gap_left gap_right rx
                                              topY botY axisX
                                              v1x v2x
                                              e1 e2 e3 e1m e2m e3m
                                              left_bar_right right_bar_left
                                              bot_start bot_end)

    (setq cx (/ overall_width 2.0))
    (setq hit nil idx 0)

    ;; Determine target gap
    (while (and (< idx (length all_bars)) (not hit))
      (setq bar (nth idx all_bars))
      (if (and (<= (car bar) cx) (>= (cadr bar) cx))
        (setq hit T)
        (setq idx (1+ idx))
      )
    )
    (if hit
      (progn
        (setq gap_left  (if (> idx 0) (cadr (nth (1- idx) all_bars)) mesh_thickness))
        (setq gap_right (car (nth idx all_bars)))
      )
      (progn
        (setq idx 0 gap_left nil gap_right nil)
        (while (< (1+ idx) (length all_bars))
          (setq gap_left  (cadr (nth idx all_bars)))
          (setq gap_right (car  (nth (1+ idx) all_bars)))
          (if (and (>= cx gap_left) (<= cx gap_right))
            (setq idx (length all_bars))
            (setq idx (1+ idx))
          )
        )
      )
    )

    ;; Place three lines anchored 0.45 mm from right edge of gap
    (setq rx (+ gap_right 0.45 mesh_thickness))
    (if (< rx gap_left) (setq rx gap_left))

    (setq topY (+ y_base mesh_height))
    (setq botY (- topY inner_height))
    (setq v1x rx)
    (setq v2x (+ rx mesh_thickness))

    ;; Bottom: extend 0.45 to the LEFT (length = thickness + 0.45)
    (setq bot_start (max gap_left (- v1x 0.45)))
    (setq bot_end   v2x)

    (command "._LINE" (list v1x topY) (list v1x botY) "") (setq e1 (entlast))
    (command "._LINE" (list bot_start botY) (list bot_end botY) "") (setq e2 (entlast))
    (command "._LINE" (list v2x topY) (list v2x botY) "") (setq e3 (entlast))

    ;; Mirror across centerline
    (setq axisX (/ (+ gap_left gap_right) 2.0))
    (command "._MIRROR" e1 e2 e3 "" (list axisX botY) (list axisX topY) "N")

    ;; Mirrored connector copies
    (setq v1x (+ axisX (- axisX v2x)))
    (setq v2x (+ axisX (- axisX v1x)))
    (command "._LINE" (list v1x topY) (list left_bar_right topY) "")
    (command "._LINE" (list v2x topY) (list right_bar_left topY) "")
  )

  (if (member mesh_key '(2 5 6 8 9))
    (_place_extra_lines_mirror_connect)
  )

;; --------------------------------------------------------
;; Insert fourth block "Verdect.dwg" 100mm left of origin
;; --------------------------------------------------------
(setq verdectBlockPath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\verdect.dwg")
(setq verdectBlockName "VerdectBlock") ;; replace with actual block name inside Verdect.dwg

;; Set insertion point 100 mm left of origin
(setq insertPt (list -100.0 0.0 0.0))

(if (findfile verdectBlockPath)
  (progn
    (command "-INSERT" (strcat verdectBlockName "=" verdectBlockPath) insertPt 1.0 1.0 0.0)
    (setq verdectBlockEnt (entlast))
    (prompt "\nVerdect block inserted 100 mm left of origin successfully.")
  )
  (prompt "\nERROR: Could not find Verdect DWG file at given path.")
)
(command "DIMTXSTY" "BEMSTIL")


  ;; ----------------------------------------------------------
  ;; New: center (or left-of-center) 18(w) x 2(h) rectangle and centerlines
  ;; ----------------------------------------------------------
  (defun _place_center_or_left_rect (/ cx topY rect_top rect_bot
                                       w h
                                       idx gap_left gap_right hit
                                       has_center_gap
                                       right_bar_x rect_cx rx
                                       rect_midY offsLR left_x right_x
                                       cline)

    (setq w 18.0)                 ;; width along X
    (setq h 2.0)                  ;; height along Y
    (setq cx (/ overall_width 2.0))
    (setq topY (+ y_base mesh_height))
    (setq rect_top (- topY 13.0))
    (setq rect_bot (- rect_top h))

    ;; Find whether overall center lies inside a gap
    (setq idx 0
          hit nil
          has_center_gap nil)

    (while (< (1+ idx) (length all_bars))
      (setq gap_left  (cadr (nth idx all_bars)))
      (setq gap_right (car  (nth (1+ idx) all_bars)))
      (if (and (>= cx gap_left) (<= cx gap_right))
        (progn
          (setq has_center_gap T)
          (setq hit T)
          (setq idx (length all_bars))   ;; break
        )
        (setq idx (1+ idx))
      )
    )

    (if has_center_gap
      (progn
        ;; Center the rectangle on overall width
        (setq rect_cx cx)
        (setq rx (- rect_cx (/ w 2.0)))
      )
      (progn
        ;; No center gap: choose the gap immediately left of center
        (setq idx 0
              gap_left nil
              gap_right nil)
        (while (< (1+ idx) (length all_bars))
          (if (< (car (nth (1+ idx) all_bars)) cx)
            (progn
              (setq gap_left  (cadr (nth idx all_bars)))
              (setq gap_right (car  (nth (1+ idx) all_bars)))
              (setq idx (1+ idx))
            )
            (setq idx (length all_bars)) ;; stop when next gap reaches/over center
          )
        )
        ;; Safety fallback: first interior gap if none strictly left
        (if (or (null gap_left) (null gap_right))
          (progn
            (setq gap_left  (cadr (nth 0 all_bars)))
            (setq gap_right (car  (nth 1 all_bars)))
          )
        )
        ;; Align rectangle center X to the right gap line (inner face of right bar)
        (setq right_bar_x gap_right)
        (setq rect_cx right_bar_x)
        (setq rx (- rect_cx (/ w 2.0)+4.55))
      )
    )

;; Draw rectangle by PLINE
(command "._PLINE"
         (list rx rect_top)
         (list (+ rx w) rect_top)
         (list (+ rx w) rect_bot)
         (list rx rect_bot)
         "C")

(setq rectEnt (entlast))  ;; capture rectangle
(setq MGRT_rectTopRight (list (+ rx w) rect_top))

;; ----------------------------------------------------------
;; EXTRA MOVE LOGIC FOR MESH 4 AND 7
;; ----------------------------------------------------------

(if (member mesh_key '(4 7))
  (progn
    ;; total number of bars
    (setq totalBars (length bar_coords))

    ;; half thickness
    (setq halfT (/ mesh_thickness 2.0))

    ;; determine MIDDLE bar index
    ;; odd → true center (e.g., 9 bars -> index 4)
    ;; even → left center (e.g., 8 bars -> index 3)
    (if (= (rem totalBars 2) 1)
      (setq midIndex (fix (/ totalBars 2)))   ;; odd count
      (setq midIndex (1- (/ totalBars 2)))    ;; even count
    )

    ;; decide direction + amount
    (cond
      ;; -------------- MESH 4 rules --------------
      ((= mesh_key 4)
        (if (= (rem totalBars 2) 1) ;; odd count
          (setq shift (+ 5.25 halfT))     ;; move RIGHT
          (setq shift (- (+ 5.25 halfT))) ;; move LEFT
        )
      )

      ;; -------------- MESH 7 rules --------------
      ((= mesh_key 7)
        (if (= (rem totalBars 2) 1) ;; odd count
          (setq shift (+ 5.25 halfT))     ;; move RIGHT
          (setq shift (- (+ 4.05 halfT))) ;; move LEFT
        )
      )
    )

    ;; EXECUTE the move
    (command "._MOVE" rectEnt "" (list 0.0 0.0) (list shift 0.0))
  )
)

    ;; Rectangle center Y
    (setq rect_midY (/ (+ rect_top rect_bot) 2.0))

;; ----------------------------------------------------------
;; Vertical red centerline
;; Even bars -> mesh_height+4 and move down (13-mesh_height)
;; Odd bars  -> 4mm and move down 1mm
;; ----------------------------------------------------------
(setq isOdd (= (rem (length bar_coords) 2) 1))

;; draw line
(command "._LINE"
         "_non" (list rect_cx rect_bot)
         "_non" (list rect_cx (+ rect_bot (if isOdd 4.0 (+ mesh_height 4.0))))
         "")
(setq cline (entlast))

;; color red
(_force-aci cline 1)

;; move down
(if isOdd
  (command "._MOVE" cline "" '(0 0) (list 0.0 -1.0))
  (command "._MOVE" cline "" '(0 0) (list 0.0 (- 13.0 mesh_height)))
)

;; Extra 4.55 mm left shift if center is on a rib
(if (not has_center_gap)
  (command "._MOVE" cline "" '(0 0) (list -4.55 0.0))
)

;; ----------------------------------------------------------
;; EXTRA MOVE LOGIC FOR CENTERLINE (cline) FOR MESH 4 & 7
;; ----------------------------------------------------------

(if (member mesh_key '(4 7))
  (progn
    ;; determine odd/even bar count
    (setq totalBars (length bar_coords))
    (setq halfT (/ mesh_thickness 2.0))

    ;; Determine shift based on mesh type
    (cond
      ;; ------------------- MESH 2 -------------------
      ((= mesh_key 4)
        (if (= (rem totalBars 2) 1)   ;; odd → move RIGHT
          (setq shift (+ 5.25 halfT))
          (setq shift (- (+ 5.25 halfT))) ;; even → move LEFT
        )
      )

      ;; ------------------- MESH 7 -------------------
      ((= mesh_key 7)
        (if (= (rem totalBars 2) 1)   ;; odd → move RIGHT
          (setq shift (+ 5.25 halfT))
          (setq shift (- (+ 4.05 halfT))) ;; even → move LEFT
        )
      )
    )

    ;; apply movement to cline
    (command "._MOVE" cline "" '(0 0) (list shift 0.0))
  )
)

;; Find the TOP endpoint of the red vertical line and store globally
(setq ed (entget cline))
(setq p1 (cdr (assoc 10 ed)))
(setq p2 (cdr (assoc 11 ed)))
(setq MGRT_redTop
      (if (> (cadr p1) (cadr p2)) p1 p2)) ;; global variable
;; ----------------------------------------------------------
;; EXTRA SHIFT FOR MESH 1, 2 & 3 WHEN BAR COUNT IS ODD
;; Move rectangle and red line LEFT by 0.7 mm
;; ----------------------------------------------------------

(if (and (member mesh_key '(1 2 3 4 7))
         (= (rem (length bar_coords) 2) 1))  ;; odd number of bars
  (progn
    ;; Move rectangle left
    (if rectEnt
      (command "._MOVE" rectEnt "" '(0 0) (list -0.7 0.0))
    )

    ;; Move red centerline left
    (if cline
      (command "._MOVE" cline "" '(0 0) (list -0.7 0.0))
    )

    ;; Update stored top point after move
    (if (and cline (setq ed (entget cline)))
      (progn
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))
        (setq MGRT_redTop (if (> (cadr p1) (cadr p2)) p1 p2))
      )
    )
  )
)

    ;; Two short vertical lines inside rectangle:
    ;; offsLR = 3.75 mm for mesh 1,3,5,7; otherwise 5 mm for 2,4,6,8,9
    (setq offsLR (if (member mesh_key '(1 3 5 7)) 3.75 5.0))

    ;; Base for inner lines:
    ;; - If center is on a rib, shift base 4.55 mm left from rectangle center
    ;; - Else use rectangle center directly
(setq base_x (if has_center_gap rect_cx (- rect_cx 5.25)))

;; ----------------------------------------------------------
;; EXTRA SHIFT FOR INNER SHORT LINES
;; mesh 5 & 6 + odd bars → shift RIGHT by 0.7 mm
;; ----------------------------------------------------------

(if (and (member mesh_key '(5 6))
         (= (rem (length bar_coords) 2) 1))  ;; odd number of bars
  (setq base_x (+ base_x 0.7))
)

;; ----------------------------------------------------------
;; EXTRA SHIFT LOGIC FOR base_x  (mesh 4 & 7)
;; ----------------------------------------------------------
(if (member mesh_key '(4 7))
  (progn
    (setq totalBars (length bar_coords))
    (setq halfT (/ mesh_thickness 2.0))

    (cond
      ;; ---------- MESH 4 ----------
      ((= mesh_key 4)
        (if (= (rem totalBars 2) 1)   ;; odd → shift RIGHT
          (setq baseShift (+ 5.25 halfT))
          (setq baseShift (- (+ 5.25 halfT))) ;; even → shift LEFT
        )
      )

      ;; ---------- MESH 7 ----------
      ((= mesh_key 7)
        (if (= (rem totalBars 2) 1)   ;; odd → shift RIGHT
          (setq baseShift (+ 5.25 halfT))
          (setq baseShift (- (+ 4.05 halfT))) ;; even → shift LEFT
        )
      )
    )

    ;; Apply shift to BASE_X (IMPORTANT)
    (setq base_x (+ base_x baseShift))
  )
)

    (setq left_x  (- base_x offsLR))
    (setq right_x (+ base_x offsLR))

    ;; Ensure rectangle center Y is available
    (if (not rect_midY)
      (setq rect_midY (/ (+ rect_top rect_bot) 2.0))
    )
;; ----------------------------------------------------------
;; Hatch + delete TEMP rectangles (LEFT + RIGHT)
;; Hatch pattern ANSI31, scale 1, angle 90
;; ----------------------------------------------------------

(if (member mesh_key '(1 2 3 4 5 6 7))
  (progn
    (if (and rx rect_top rect_midY left_x right_x w)
      (progn
        (setq rectRightX (+ rx w))
        (setq isOdd  (= (rem (length bar_coords) 2) 1))
        (setq isEven (= (rem (length bar_coords) 2) 0))

        ;; ==================================================
        ;; LEFT SIDE
        ;; ==================================================
        (setq leftStartX rx)
        (setq leftEndX   left_x)

        ;; mesh 1,2,3 odd => extend left start 0.7 to left
        (if (and (member mesh_key '(1 2 3)) isOdd)
          (setq leftStartX (- rx 0.7))
        )

        ;; mesh 4 => LEFT width = 4mm always + move depends odd/even (MOVE LEFT)
        (if (= mesh_key 4)
          (progn
            (setq leftStartX rx)
            (setq leftEndX (+ leftStartX 4.0))

            ;; move LEFT amount for mesh 4
            (setq moveX (if isOdd 6.05 -6.75))
            (setq leftStartX (+ leftStartX moveX))
            (setq leftEndX   (+ leftEndX   moveX))
          )
        )

        ;; mesh 7 => LEFT width = 4mm always + odd/even move rules
        (if (= mesh_key 7)
          (progn
            (setq leftStartX rx)
            (setq leftEndX (+ leftStartX 4.0))

            ;; mesh 7 EVEN => move LEFT rect LEFT by 5.55
            (if isEven
              (progn
                (setq leftStartX (- leftStartX 5.55))
                (setq leftEndX   (- leftEndX   5.55))
              )
            )

            ;; mesh 7 ODD => move LEFT rect RIGHT by 6.05
            (if isOdd
              (progn
                (setq leftStartX (+ leftStartX 6.05))
                (setq leftEndX   (+ leftEndX   6.05))
              )
            )
          )
        )

        (setq ptA (list leftStartX rect_top))
        (setq ptB (list leftEndX (- rect_midY 1.0)))

        (command "._RECTANG" "_non" ptA "_non" ptB)
        (setq tmpRectL (entlast))

        (if tmpRectL
          (progn
            (command "._HATCH" "P" "ANSI31" "1" "90" "S" tmpRectL "" "")
            (entdel tmpRectL)
          )
        )

        ;; ==================================================
        ;; RIGHT SIDE
        ;; ==================================================
        (setq rightStartX right_x)
        (setq rightEndX (- rectRightX 1.4)) ;; default
;; mesh 5 (odd+even) => RIGHT rect width = 5.25mm
(if (= mesh_key 5)
  (progn
    (setq rightStartX right_x)
    (setq rightEndX (+ rightStartX 5.25))
  )
)
;; mesh 6 (odd+even) => RIGHT rect width = 4mm
(if (= mesh_key 6)
  (progn
    (setq rightStartX right_x)
    (setq rightEndX (+ rightStartX 4.0))
  )
)


        ;; mesh 1,2,3 odd => +0.7 correction
        (if (and (member mesh_key '(1 2 3)) isOdd)
          (setq rightEndX (+ rightEndX 0.7))
        )
;; mesh 2 EVEN => RIGHT rect width = 4mm
(if (and (= mesh_key 2) isEven)
  (progn
    (setq rightStartX right_x)
    (setq rightEndX (+ rightStartX 4.0))
  )
)
;; mesh 1 EVEN => RIGHT rect width = 4mm
(if (and (= mesh_key 1) isEven)
  (progn
    (setq rightStartX right_x)
    (setq rightEndX (+ rightStartX 5.25))
  )
)
;; mesh 3 EVEN => RIGHT rect width = 5.25mm
(if (and (= mesh_key 3) isEven)
  (progn
    (setq rightStartX right_x)
    (setq rightEndX (+ rightStartX 5.25))
  )
)


        ;; mesh 4 => RIGHT width = 4mm always
        (if (= mesh_key 4)
          (progn
            (setq rightStartX right_x)
            (setq rightEndX (+ rightStartX 4.0))
          )
        )

        ;; mesh 7 => RIGHT width = 4mm always + odd/even move rules
        (if (= mesh_key 7)
          (progn
            (setq rightStartX right_x)
            (setq rightEndX (+ rightStartX 4.0))

            ;; mesh 7 EVEN => move RIGHT rect RIGHT by 1.25
            (if isEven
              (progn
                (setq rightStartX (+ rightStartX 1.25))
                (setq rightEndX   (+ rightEndX   1.25))
              )
            )

            ;; mesh 7 ODD => move RIGHT rect RIGHT by 1.25
            (if isOdd
              (progn
                (setq rightStartX (+ rightStartX 1.25))
                (setq rightEndX   (+ rightEndX   1.25))
              )
            )
          )
        )

        (setq ptC (list rightStartX (- rect_midY 1.0)))
        (setq ptD (list rightEndX rect_top))

        (command "._RECTANG" "_non" ptC "_non" ptD)
        (setq tmpRectR (entlast))

        (if tmpRectR
          (progn
            (command "._HATCH" "P" "ANSI31" "1" "90" "S" tmpRectR "" "")
            (entdel tmpRectR)
          )
        )

        (prompt "\n✔ Hatch created successfully (mesh rules applied).")
      )
      (prompt "\n⚠ ERROR: Required values NIL (rx/rect_top/rect_midY/left_x/right_x/w).")
    )
  )
)

(princ)

;; Draw two 2 mm verticals centered on rect_midY
(command "._LINE"
         (list left_x  (- rect_midY 1.0))
         (list left_x  (+ rect_midY 1.0))
         "")
(setq leftShortLine (entlast))

(command "._LINE"
         (list right_x (- rect_midY 1.0))
         (list right_x (+ rect_midY 1.0))
         "")
(setq rightShortLine (entlast))

;; ----------------------------------------------------------
;; Mesh 7: move short lines
;; left line  -> LEFT  by 1.25mm
;; right line -> RIGHT by 1.25mm
;; ----------------------------------------------------------
(if (= mesh_key 7)
  (progn
    (if leftShortLine
      (command "._MOVE" leftShortLine "" '(0 0) (list -1.25 0.0))
    )
    (if rightShortLine
      (command "._MOVE" rightShortLine "" '(0 0) (list 1.25 0.0))
    )
  )
)

  )

  ;; Always place for ALL mesh types
  (_place_center_or_left_rect)

;; ----------------------------------------------------------
;; MAIN DIMENSIONS AROUND THE MESH
;; ----------------------------------------------------------
  (command "DIMLFAC" 0.5)

(setq leftTop  (list x_left (+ y_base mesh_height)))            ;; left bar top-left
(setq rightTop (list (+ x_right mesh_thickness)
                     (+ y_base mesh_height)))                   ;; right bar top-right
(setq leftBot  (list x_left y_base))                            ;; left bar bottom-left

;; 1) Overall top: left bar → right bar, 22 mm above
(setq dimLoc1 (list (car leftTop) (+ (cadr leftTop) 33.0)))
(command "._DIMLINEAR"
         leftTop
         rightTop
         dimLoc1
         "")

;; 2) Left bar top → red line top, 10 mm above
;; 3) Red line top → right bar top, 10 mm above
(if (and (boundp 'MGRT_redTop) MGRT_redTop)
  (progn
    (setq dimLoc2 (list (car leftTop) (+ (cadr leftTop) 14.5)))
    (command "._DIMLINEAR"
             leftTop
             MGRT_redTop
             dimLoc2
             "")

    (setq dimLoc3 (list (car rightTop) (+ (cadr rightTop) 14.5)))
    (command "._DIMLINEAR"
             MGRT_redTop
             rightTop
             dimLoc3
             "")
  )
)

;; 4) Left bar height: top → bottom, 10 mm to the LEFT
(setq dimLoc4 (list (- x_left 12.5)
                    (/ (+ (cadr leftTop) (cadr leftBot)) 2.0)))
(command "._DIMLINEAR"
         leftBot
         leftTop
         dimLoc4
         "")

;; ----------------------------------------------------------
;; 5) Right bar top → rectangle top-right (always live position)
;; ----------------------------------------------------------

(if (and (boundp 'rectEnt) rectEnt)
  (progn
    ;; Get live rectangle extents
    (vla-getboundingbox
      (vlax-ename->vla-object rectEnt)
      'bbMin 'bbMax)

    (setq bbMin (vlax-safearray->list bbMin))
    (setq bbMax (vlax-safearray->list bbMax))

    ;; Actual rectangle top-right corner
    (setq rectTopRight (list (car bbMax) (cadr bbMax)))

    ;; Dimension location: 19 mm to the right, vertically centered
    (setq dimLoc5
          (list (+ (max (car rightTop) (car rectTopRight)) 19.0)
                (/ (+ (cadr rightTop) (cadr rectTopRight)) 2.0)))

    (command "._DIMLINEAR"
             rightTop
             rectTopRight
             dimLoc5
             "")
  )
)

  (command "DIMLFAC" 1)
;; -------------------------------------------------
;; Move all HATCH objects to layer "SCHRAFF"
;; -------------------------------------------------
(if (not (tblsearch "LAYER" "SCHRAFF"))
  (command "._-LAYER" "M" "SCHRAFF" "")
)

(setq ssH (ssget "_X" '((0 . "HATCH"))))

(if ssH
  (progn
    (command "._CHPROP" ssH "" "LA" "SCHRAFF" "")
    (prompt "\n✔ All hatches moved to SCHRAFF layer.")
  )
  (prompt "\n⚠ No hatch objects found.")
)

(princ)
  ;; ----------------------------------------------------------
  (defun _draw_top_view_custom (/ top_y rect_length x_start x_end bar offset_y first_end_x     last_start_x mid_x fill_step)
  ;; ----------------------------------------------------------
  ;; Ask user for manual length
  (setq rect_length (getreal "\nEnter gesamt: "))
(setq MGRT_rect_length rect_length)

  ;; Upward offset for center rectangles = bar thickness (auto)
  (setq offset_y mesh_thickness)

  ;; Vertical start position: 250 mm above section top + mesh thickness
  (setq top_y (+ y_base mesh_height 500.0 mesh_thickness))
(setq MGRT_top_y top_y)

  ;; ----------------------------------------------------------
  ;; 1st rectangle (left)
  ;; ----------------------------------------------------------
  (setq x_start 0.0)
  (setq x_end   mesh_thickness)
  (command "._PLINE"
           (list x_start top_y)
           (list x_end   top_y)
           (list x_end   (+ top_y rect_length))
           (list x_start (+ top_y rect_length))
           "C")
  (setq first_end_x x_end) ; store first rectangle top-right X

  ;; ----------------------------------------------------------
  ;; Last rectangle (right)
  ;; ----------------------------------------------------------
  (setq x_start (- overall_width mesh_thickness))
  (setq x_end   overall_width)
  (command "._PLINE"
           (list x_start top_y)
           (list x_end   top_y)
           (list x_end   (+ top_y rect_length))
           (list x_start (+ top_y rect_length))
           "C")
  (setq last_start_x x_start) ; store last rectangle top-left X
;; --------------------------------------------------------------
;; Dimension right vertical line of right-side rectangle
;; Place text 130 mm to the RIGHT
;; --------------------------------------------------------------

(setq rightTopPt    (list x_end top_y))
(setq rightBottomPt (list x_end (+ top_y rect_length)))

;; Location: 130 mm to the RIGHT of x_end, vertically centered
(setq dimLoc
      (list (+ x_end 130.0)
            (/ (+ (cadr rightTopPt) (cadr rightBottomPt)) 2.0)))

(command "._DIMLINEAR"
         rightTopPt
         rightBottomPt
         dimLoc
         "")
;; ==========================================================
;; Dimension: first rectangle top-left → last rectangle top-right
;; Place 68 mm ABOVE the rectangles
;; ==========================================================

(setq topY top_y)

(setq ptLeft  (list 0.0 topY))
(setq ptRight (list overall_width topY))

;; Dimension line 68 mm above topY
(setq dimLoc (list (/ (+ 0.0 overall_width) 2.0)
                   (- topY 68.0)))

(command "._DIMLINEAR"
         ptLeft
         ptRight
         dimLoc
         "")

  ;; ----------------------------------------------------------
  ;; Connect 1st and last rectangle with ONE straight baseline,
  ;; then optionally add parallel fillers at thickness spacing
  ;; ----------------------------------------------------------
  (setq topYline (+ top_y rect_length))                    ;; top edge Y
  (setq pL (list first_end_x  topYline))                   ;; left endpoint
  (setq pR (list last_start_x topYline))                   ;; right endpoint

  ;; Bottom straight baseline across the gap
  (setq botYline top_y)                                ;; bottom edge Y
  (setq pLbot (list first_end_x  botYline))            ;; left rect bottom-right
  (setq pRbot (list last_start_x botYline))            ;; right rect bottom-left
  (command "._LINE" pLbot pRbot "")


  ;; 1) Single straight line across the gap
  (command "._LINE" pL pR "")

  ;; 2) Optional: create N parallel lines offset upward by mesh_thickness
  ;;    Comment out the block below if only one line is desired
  (setq N 0)                      ;; set N>0 to draw extra parallels
  (setq i 1)
  (while (<= i N)
    (setq yoff (* i mesh_thickness))
    (command "._LINE"
             (list first_end_x  (+ topYline yoff))
             (list last_start_x (+ topYline yoff))
             "")
    (setq i (1+ i))
  )

  ;; ----------------------------------------------------------
  ;; Replicate gap horizontal lines at top view,
  ;; offset downward by mesh_thickness from the top edge
  ;; ----------------------------------------------------------
  (setq gapY (- (+ top_y rect_length) mesh_thickness))  ;; Y at top - thickness

  (setq prev_right (cadr (car all_bars)))
  (foreach bc (cdr all_bars)
    (setq gap_start prev_right)
    (setq gap_end   (car bc))
    ;; draw only if positive width (tolerance)
    (if (> (- gap_end gap_start) 1e-8)
      (command "._LINE"
               (list gap_start gapY)
               (list gap_end   gapY)
               "")
    )
    (setq prev_right (cadr bc))
  )

  ;; ----------------------------------------------------------
  ;; Replicate gap horizontal lines near BOTTOM of the top view band,
  ;; offset upward by mesh_thickness from the bottom edge
  ;; ----------------------------------------------------------
  (setq gapYbot (+ top_y mesh_thickness))  ;; Y at bottom + thickness

  (setq prev_right (cadr (car all_bars)))
  (foreach bc (cdr all_bars)
    (setq gap_start prev_right)
    (setq gap_end   (car bc))
    ;; draw only if positive width
    (if (> (- gap_end gap_start) 1e-8)
      (command "._LINE"
               (list gap_start gapYbot)
               (list gap_end   gapYbot)
               "")
    )
    (setq prev_right (cadr bc))
  )

;; ------------------------------------------------------------
;; Minimum end-gap rule:
;; - Mesh types 1,2,3,4  → 15.00 mm
;; - Mesh types 5,6,7,8,9 → 15.65 mm
;; ------------------------------------------------------------
(setq gLo (if (member mesh_key '(1 2 3 4))
              15.0          ;; mesh 1–4
              15.65))       ;; mesh 5–9

;; ===== Equal end gaps driven by visible limits =====
;; - Bottom dim (gapYbot -> yEndBot) >= gLo
;; - Top dim    (gapY    -> yEndTop) >= gLo + thickness
;;   If not satisfied, reduce k (remove one instance) and retry.
;; ================================================

(setq yTop topYline)
(setq yBot (- botYline mesh_thickness))
(setq H (- yTop yBot))

;; Start with maximum possible k and step down until limits are satisfied
(setq k (fix (/ H s)))
(setq g nil)

(while (and (>= k 0) (null g))
  ;; Candidate end gap g for this k
  (setq g (/ (- H (* k s)) 2.0))

  ;; End levels in drawing coordinates
  (setq yEndBot (+ yBot g))
  (setq yEndTop (- yTop g))

  ;; Visible dimensions:
  (setq BottomGap (- yEndBot gapYbot))
  (setq TopGap    (- gapY    yEndTop))  ;; always positive

  ;; Requirement per mesh type:
  ;; - Bottom >= gLo
  ;; - Top >= gLo + thickness
  (if (and (>= BottomGap gLo)
           (>= TopGap    (+ gLo mesh_thickness)))
    (setq g g)   ;; accept this k and g
    (progn
      ;; Remove one instance and try again
      (setq g nil)
      (setq k (1- k))
    )
  )
)

;; Fallback: if no k satisfies the limit, force center with k=0
(if (null g)
  (progn
    (setq k 0)
    (setq g (/ H 2.0))
    (setq yEndBot (+ yBot g))
    (setq yEndTop (- yTop g))
    (setq BottomGap (- yEndBot gapYbot))
    (setq TopGap    (- gapY    yEndTop))
  )
)

;; (yEndBot, yEndTop) are now final and respect your limits.
;; Draw for each interior gap [xs, xe]
(setq prev_right (cadr (car all_bars)))
(foreach bc (cdr all_bars)
  (setq gap_start prev_right)
  (setq gap_end   (car bc))
  (if (and (> (- gap_end gap_start) 1e-8)
           (< gap_start last_start_x)
           (> gap_end   first_end_x))
    (progn
      (setq xs (max gap_start first_end_x))
      (setq xe (min gap_end   last_start_x))
      (if (< xs xe)
        (progn
          ;; bottom end (main + companion)
          (command "._LINE" (list xs yEndBot) (list xe yEndBot) "")
          (command "._LINE" (list xs (+ yEndBot mesh_thickness)) (list xe (+ yEndBot mesh_thickness)) "")

          ;; interiors every s (excluding ends)
          (setq i 1)
          (while (< i k)
            (setq yk (+ yEndBot (* i s)))
            (command "._LINE" (list xs yk) (list xe yk) "")
            (command "._LINE" (list xs (+ yk mesh_thickness)) (list xe (+ yk mesh_thickness)) "")
            (setq i (1+ i))
          )

          ;; top end (main + companion) when k >= 1
          (if (>= k 1)
            (progn
              (command "._LINE" (list xs yEndTop) (list xe yEndTop) "")
              (command "._LINE" (list xs (+ yEndTop mesh_thickness)) (list xe (+ yEndTop mesh_thickness)) "")
            )
          )
        )
      )
    )
  )
  (setq prev_right (cadr bc))
)

(princ
  (strcat
    "\nEqual ends solved: BottomGap = " (rtos BottomGap 2 2)
    " mm, TopGap = " (rtos TopGap 2 2)
    " mm, k = " (itoa k)
    ", Limit = " (rtos gLo 2 2) " mm"
  )
)

;; ----------------------------------------------
;; DIMENSION: left-most vertical → next vertical (first gap)
;; ----------------------------------------------

(if (and all_bars (> (length all_bars) 1))
  (progn
    (setq xL (car  (nth 0 all_bars)))   ;; left-most vertical
    (setq xR (car  (nth 1 all_bars)))   ;; left face of second bar
    (setq yT (+ top_y rect_length))

    (setq dim_pt1 (list xL yT))
    (setq dim_pt2 (list xR (- yT mesh_thickness)))

    (command "._DIMLINEAR"
             dim_pt2
             dim_pt1
             (list (/ (+ xL xR) 2.0)
                   (+ yT 45.0))
             "")
  )
)

;; ----------------------------------------------
;; MIRROR that dimension about center of top horizontal line
;; Keep original (Do not erase source)
;; ----------------------------------------------

(setq lastDim (entlast))

;; Center of the top horizontal line = overall_width / 2
(setq mirrorX (/ overall_width 2.0))

;; Two points defining vertical mirror axis at mirrorX
(setq mirPt1 (list mirrorX (- yT 100.0)))
(setq mirPt2 (list mirrorX (+ yT 100.0)))

(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")   ;; N = keep original

;; ----------------------------------------------
;; DIMENSION: right face of LEFT bar → right face of RIGHT bar (middle gap)
;; ----------------------------------------------

(if (and all_bars (> (length all_bars) 2))
  (progn
    ;; middle gap index
    (setq midGapIdx (fix (/ (1- (length all_bars)) 2)))

    ;; Left and right bars around the middle gap
    (setq leftBar  (nth midGapIdx all_bars))
    (setq rightBar (nth (1+ midGapIdx) all_bars))

    ;; Faces to use
    (setq xL (cadr leftBar))   ;; right face of left bar  (as earlier)
    (setq xR (cadr rightBar))  ;; right face of right bar (new)

    (setq yT (+ top_y rect_length))

    (setq dim_pt1 (list xL yT))
    (setq dim_pt2 (list xR yT))

    ;; Create dimension
    (command "._DIMLINEAR"
             dim_pt1
             dim_pt2
             (list (/ (+ xL xR) 2.0)
                   (+ yT 100.0))
             "")

    ;; Move it DOWN by mesh_thickness
    (setq lastDim (entlast))
    (command "._MOVE"
             lastDim
             ""
             '(0 0)
             (list 0.0 (- mesh_thickness)))
  )
)
;; ----------------------------------------------
;; DIMENSION: from bottom band offset (gapYbot)
;; to the first equal-end horizontal line (yEndBot)
;; ----------------------------------------------

(setq dim_pt1 (list (- first_end_x mesh_thickness)
                    (- gapYbot mesh_thickness)))

(setq dim_pt2 (list first_end_x yEndBot))

(command "._DIMLINEAR"
         dim_pt1
         dim_pt2
         (list (- first_end_x 20.0)
               (/ (+ (+ gapYbot mesh_thickness) yEndBot) 2.0))
         "")

;; ----------------------------------------------
;; HORIZONTAL MIRROR of the above dimension
;; About center of top view band
;; Keep original
;; ----------------------------------------------

(setq lastDim (entlast))

;; Y of mirror axis = center of top-view rectangle
(setq mirrorY (+ top_y (/ rect_length 2.0)))

;; Use last_start_x only as X span reference (not for axis)
(setq mirPt1 (list (- last_start_x 200.0) mirrorY))
(setq mirPt2 (list (+ last_start_x 200.0) mirrorY))

(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "Y")   ;; keep original
;; ----------------------------------------------
;; DIMENSION: vertical spacing between middle horizontals
;; ----------------------------------------------

(if (and (numberp k) (> k 0))
  (progn
    ;; index of middle horizontal
    (setq midV (fix (/ k 2)))

    ;; Y of middle horizontal
    (setq yH1 (+ yEndBot (* midV s)))

    ;; Y of next horizontal above it
    (setq yH2 (+ yH1 s))

    ;; X at center of the mesh (just for snap)
    (setq xMid (/ overall_width 2.0))

    ;; Dimension points
    (setq dim_pt1 (list xMid yH1))
    (setq dim_pt2 (list xMid yH2))

    ;; Place dimension 33 mm to the RIGHT of xMid
    (setq dimLoc
          (list (+ xMid 33.0)
                (/ (+ yH1 yH2) 2.0)))

    (command "._DIMLINEAR"
             dim_pt1
             dim_pt2
             dimLoc
             "")

;; Move it right by (half width - thickness)
    (setq lastDim (entlast))
    (command "._MOVE"
             lastDim
             ""
             '(0 0)
             (list (- (/ overall_width 2.0) mesh_thickness) 0.0))
  )
)


;; ----------------------------------------------------------
;; Inner rectangles: aligned with inner bars, length - 2*thickness
;; Conditional skipping based on mesh type:
;;  - mesh 1,3  → draw ALL
;;  - mesh 4,7  → skip ONLY the middle rectangle (draw horizontal lines only)
;;  - others    → skip center & left-of-center (draw horizontal lines only)
;; ----------------------------------------------------------

(setq rect_length (- rect_length (* 2.0 mesh_thickness))) ; adjust length

(setq totalBars (length bar_coords))
(setq midIndex (fix (/ totalBars 2)))      ;; center index
(setq leftOfMid (1- midIndex))             ;; left of center
(setq idx 0)

(foreach bar bar_coords


  (setq x_start (car bar))
  (setq x_end   (cadr bar))

  (setq y1 (+ top_y offset_y))
  (setq y2 (+ top_y offset_y rect_length))

  (cond

    ;; =====================================================
    ;; CASE 1 — mesh 1 & 3 → draw ALL rectangles
    ;; =====================================================
    ((member mesh_key '(1 3))
      (command "._PLINE"
               (list x_start y1)
               (list x_end   y1)
               (list x_end   y2)
               (list x_start y2)
               "C")
    )

    ;; ============================================================
    ;; CASE — mesh 4 & 7 → skip ONLY the middle rectangle
    ;; Draw HORIZONTAL LINES ONLY for the skipped one
    ;; ============================================================
    ((member mesh_key '(4 7))
      (progn
        ;; Determine skip index one time
        (if (not (boundp 'MGRT_skipIndex))
          (progn
            (setq MGRT_total (length bar_coords))
            (if (= (rem MGRT_total 2) 1)
              (setq MGRT_skipIndex (fix (/ MGRT_total 2)))  ;; odd count
              (setq MGRT_skipIndex (1- (/ MGRT_total 2)))   ;; even count
            )
          )
        )

        (if (= idx MGRT_skipIndex)
          ;; skipped → draw only horizontal lines
          (progn
            (command "._LINE" (list x_start y1) (list x_end y1) "")
            (command "._LINE" (list x_start y2) (list x_end y2) "")
;; ===========================================
;; Draw vertical lines at ends of skipped horizontals
;; vertRise = 4*s + dimension + thickness
;; ===========================================
(setq vertRise (+ (* 3 s)
                  (- yEndBot gapYbot)
                  mesh_thickness))

;; -------------------------------------------
;; For BOTTOM horizontal y1 → verticals UP
;; -------------------------------------------
(command "._LINE"
         (list x_start y1)
         (list x_start (+ y1 vertRise))
         "")

(command "._LINE"
         (list x_end y1)
         (list x_end (+ y1 vertRise))
         "")
;; ======================================================
;; SECOND SET OF VERTICAL LINES (BOTTOM horizontal only)
;; Length = rect_length - 2*vertRise - 2*s (+ 2*thickness included)
;; ======================================================

(setq secondLen (- (+ rect_length (* 2 mesh_thickness))
                   (* 2 vertRise)
                   (* 2 s)))

;; Move amount upward
(setq moveUp2 (+ vertRise s (- mesh_thickness)))

(if (> secondLen 0)
  (progn

    ;; ---- draw LEFT vertical (original location) ----
    (command "._LINE"
             (list x_start y1)
             (list x_start (+ y1 secondLen))
             "")
    (setq vLeft (entlast)) ;; capture entity

    ;; ---- draw RIGHT vertical (original location) ----
    (command "._LINE"
             (list x_end y1)
             (list x_end (+ y1 secondLen))
             "")
    (setq vRight (entlast))

    ;; ---- MOVE BOTH UP by (vertRise + s - thickness) ----
    (command "._MOVE"
             vLeft vRight ""
             (list 0 0)
             (list 0 moveUp2)
    )
  )
  (prompt "\nSecond vertical length too small — skipped.")
)

;; -------------------------------------------
;; For TOP horizontal y2 → verticals DOWN
;; -------------------------------------------
(command "._LINE"
         (list x_start y2)
         (list x_start (- y2 vertRise))
         "")

(command "._LINE"
         (list x_end y2)
         (list x_end (- y2 vertRise))
         "")

          )

          ;; normal rectangle
          (command "._PLINE"
                   (list x_start y1)
                   (list x_end   y1)
                   (list x_end   y2)
                   (list x_start y2)
                   "C")
        )
      )
    )

    ;; =====================================================
    ;; CASE 3 — other mesh types
    ;; skip center & left-of-center → horizontal lines only
    ;; =====================================================
    (t
      (if (or (= idx midIndex) (= idx leftOfMid))
        ;; skipped → horizontal only
        (progn
          (command "._LINE" (list x_start y1) (list x_end y1) "")
          (command "._LINE" (list x_start y2) (list x_end y2) "")
;; ===========================================
;; Draw vertical lines at ends of skipped horizontals
;; vertRise = 4*s + dimension + thickness
;; ===========================================
(setq vertRise (+ (* 3 s)
                  (- yEndBot gapYbot)
                  mesh_thickness))

;; -------------------------------------------
;; For BOTTOM horizontal y1 → verticals UP
;; -------------------------------------------
(command "._LINE"
         (list x_start y1)
         (list x_start (+ y1 vertRise))
         "")

(command "._LINE"
         (list x_end y1)
         (list x_end (+ y1 vertRise))
         "")
;; ======================================================
;; SECOND SET OF VERTICAL LINES (BOTTOM horizontal only)
;; Length = rect_length - 2*vertRise - 2*s (+ 2*thickness included)
;; ======================================================

(setq secondLen (- (+ rect_length (* 2 mesh_thickness))
                   (* 2 vertRise)
                   (* 2 s)))

;; Move amount upward
(setq moveUp2 (+ vertRise s (- mesh_thickness)))

(if (> secondLen 0)
  (progn

    ;; ---- draw LEFT vertical (original location) ----
    (command "._LINE"
             (list x_start y1)
             (list x_start (+ y1 secondLen))
             "")
    (setq vLeft (entlast)) ;; capture entity

    ;; ---- draw RIGHT vertical (original location) ----
    (command "._LINE"
             (list x_end y1)
             (list x_end (+ y1 secondLen))
             "")
    (setq vRight (entlast))

    ;; ---- MOVE BOTH UP by (vertRise + s - thickness) ----
    (command "._MOVE"
             vLeft vRight ""
             (list 0 0)
             (list 0 moveUp2)
    )
  )
  (prompt "\nSecond vertical length too small — skipped.")
)

;; -------------------------------------------
;; For TOP horizontal y2 → verticals DOWN
;; -------------------------------------------
(command "._LINE"
         (list x_start y2)
         (list x_start (- y2 vertRise))
         "")

(command "._LINE"
         (list x_end y2)
         (list x_end (- y2 vertRise))
         "")

        )

        ;; normal rectangle
        (command "._PLINE"
                 (list x_start y1)
                 (list x_end   y1)
                 (list x_end   y2)
                 (list x_start y2)
                 "C")
      )
    )

  )

  (setq idx (1+ idx))
)

(princ "\nInner rectangles with skip → horizontal-lines-only done.")
)

;; Call the function after section view
(_draw_top_view_custom)

  ;; Feedback
  (princ
    (strcat
      "\nMesh grating created for: " mesh_name
      "\nBars: " (itoa n)
      ", Inner gap: 9.10"
      ", End gaps: " (rtos g_end 2 2) " (equal)"
      ", Rectangle, centerline, and inner shorts placed"
    )
  )

;; ----------------------------------------------------------
;; EXPLODE logic ONLY for mesh types 2,5,6,8,9
;; Center-based bar detection
;; ----------------------------------------------------------
(if (member mesh_key '(2 5 6 8 9))
  (progn
    (setq cx (/ overall_width 2.0))   ;; center X
    (setq barCenter nil)
    (setq barLeft nil)
    (setq barRight nil)

    ;; First check if center lies ON a bar
    (setq i 0)
    (while (< i (length all_bars))
      (setq b (nth i all_bars))
      (if (and (>= cx (car b)) (<= cx (cadr b)))
        (progn
          ;; center lies on this bar
          (setq barCenter b)
          ;; left bar if exists
          (if (> i 0)
            (setq barLeft (nth (1- i) all_bars)))
          ;; right bar if exists
          (if (< i (1- (length all_bars)))
            (setq barRight (nth (1+ i) all_bars)))
          (setq i (length all_bars)) ; break
        )
        (setq i (1+ i))
      )
    )

    ;; If center NOT on a bar, then it's inside a gap
    (if (not barCenter)
      (progn
        (setq j 0)
        (while (< (1+ j) (length all_bars))
          (setq L (cadr (nth j all_bars)))      ;; right face of left bar
          (setq R (car  (nth (1+ j) all_bars))) ;; left face of right bar

          (if (and (>= cx L) (<= cx R))
            (progn
              ;; left bar of gap
              (setq barLeft (nth j all_bars))
              ;; right bar of gap
              (setq barRight (nth (1+ j) all_bars))
              (setq j (length all_bars)) ;; break
            )
            (setq j (1+ j))
          )
        )
      )
    )

    ;; ---- Helper to explode bar by coords ----
    (defun explodeBar (bc)
      (if bc
        (progn
          (setq x1 (car bc))
          (setq x2 (cadr bc))
          ;; Select this bar PLINE with tall window covering its height
          (setq ssBar
            (ssget "_W"
                   (list x1 (- y_base 500))
                   (list x2 (+ y_base mesh_height 500))
            )
          )
          (if ssBar
            (command "._EXPLODE" ssBar "")
          )
        )
      )
    )

    ;; ---- Execute explodes ----
    (if barCenter
      (progn
        (explodeBar barCenter)
        (explodeBar barLeft)
        (explodeBar barRight)
      )
      (progn
        ;; center in gap: explode left AND right bar
        (explodeBar barLeft)
        (explodeBar barRight)
      )
    )
  )
)
;; ----------------------------------------------------------
;; COLOR the boundary lines of exploded bars (corrected logic)
;; ----------------------------------------------------------

(defun ColorBarLines (bc side)
  (if bc
    (progn
      (setq x1 (car bc))        ;; left face X
      (setq x2 (cadr bc))       ;; right face X

      ;; select exploded parts ONLY inside this bar range
      (setq ssExp
        (ssget "_C"
               (list x1 (- y_base 200))
               (list x2 (+ y_base mesh_height 200))
        )
      )

      (if ssExp
        (progn
          (setq n 0
                total (sslength ssExp))

          (while (< n total)
            (setq e (ssname ssExp n))
            (setq d (entget e))

            (if (= (cdr (assoc 0 d)) "LINE")
              (progn
                (setq p1 (cdr (assoc 10 d)))
                (setq p2 (cdr (assoc 11 d)))

                ;; LEFT SIDE color rule
                (if (and (= side "LEFT")
                         (= (car p1) x1) (= (car p2) x1))
                  (command "._CHPROP" e "" "COLOR" "2" "")
                )

                ;; RIGHT SIDE color rule
                (if (and (= side "RIGHT")
                         (= (car p1) x2) (= (car p2) x2))
                  (command "._CHPROP" e "" "COLOR" "2" "")
                )
              )
            )
            (setq n (1+ n))
          )
        )
      )
    )
  )
)

;; ----------------------------------------------------------
;; Apply coloring AFTER explode
;; ----------------------------------------------------------

(if barCenter
  (progn
    ;; CASE: center on bar
    (ColorBarLines barLeft "LEFT")     ;; left bar → left yellow
    (ColorBarLines barCenter "RIGHT")  ;; center bar → right yellow
    ;; barRight → no color
  )

  ;; CASE: center is gap
  (progn
    (ColorBarLines barLeft "LEFT")     ;; left bar → left yellow
    (ColorBarLines barRight "RIGHT")   ;; right bar → right yellow
  )
)
;; ----------------------------------------------------------
;; EXPLODE logic ONLY for mesh types 4 and 7
;; One-bar explode:
;;  - If center has a bar → explode center bar only
;;  - If center is inside a gap → explode left bar only
;; ----------------------------------------------------------
(if (member mesh_key '(4 7))
  (progn
    (setq cx (/ overall_width 2.0))   ;; center X
    (setq barCenter nil)
    (setq barLeft nil)

    ;; ---- CHECK IF CENTER LIES ON A BAR ----
    (setq i 0)
    (while (< i (length all_bars))
      (setq b (nth i all_bars))
      (if (and (>= cx (car b)) (<= cx (cadr b)))
        (progn
          (setq barCenter b)
          (if (> i 0)
            (setq barLeft (nth (1- i) all_bars)))
          (setq i (length all_bars))
        )
        (setq i (1+ i))
      )
    )

    ;; ---- If NO center bar → center lies inside a gap ----
    (if (not barCenter)
      (progn
        (setq j 0)
        (while (< (1+ j) (length all_bars))
          (setq L (cadr (nth j all_bars)))
          (setq R (car  (nth (1+ j) all_bars)))

          (if (and (>= cx L) (<= cx R))
            (progn
              (setq barLeft (nth j all_bars))
              (setq j (length all_bars))
            )
            (setq j (1+ j))
          )
        )
      )
    )

    ;; ---- Helper: explode bar then set TOP HORIZONTAL to WHITE ----
    (defun explodeBar47 (bc)
      (if bc
        (progn
          (setq x1 (car bc))   ;; left X
          (setq x2 (cadr bc))  ;; right X

          ;; Select full bar before exploding
          (setq ssBar
            (ssget "_W"
                   (list x1 (- y_base 500))
                   (list x2 (+ y_base mesh_height 500))
            )
          )

          ;; EXPLODE
          (if ssBar
            (command "._EXPLODE" ssBar "")
          )

          ;; Select exploded objects
          (setq ssExp
            (ssget "_C"
                   (list x1 (- y_base 500))
                   (list x2 (+ y_base mesh_height 500))
            )
          )

          (if ssExp
            (progn
              (setq n 0
                    total (sslength ssExp))

              (setq yTopBar (+ y_base mesh_height)) ;; TRUE top horizontal

              (while (< n total)
                (setq e (ssname ssExp n))
                (setq d (entget e))

                (if (= (cdr (assoc 0 d)) "LINE")
                  (progn
                    (setq p1 (cdr (assoc 10 d)))
                    (setq p2 (cdr (assoc 11 d)))

                    ;; =============================
                    ;; ONLY TOP HORIZONTAL → WHITE
                    ;; =============================
                    (if (and (= (cadr p1) yTopBar)
                             (= (cadr p2) yTopBar))
                      (command "._CHPROP" e "" "COLOR" "7" "")
                    )
                  )
                )
                (setq n (1+ n))
              )
            )
          )
        )
      )
    )

    ;; ---- EXECUTE ----
    (if barCenter
      (explodeBar47 barCenter)
      (explodeBar47 barLeft)
    )
  )
)

;; ----------------------------------------------------------
;; AFTER EXPLODE: Make ONLY the TOPMOST horizontal line WHITE
;; ----------------------------------------------------------
(if (member mesh_key '(4 7))
(progn
  ;; Select all exploded pieces in this bar region
  (setq ssExp
    (ssget "_C"
           (list x1 (- y_base 500))
           (list x2 (+ y_base mesh_height 500))
    )
  )

  (if ssExp
    (progn
      (setq highestY -1e9)
      (setq topLine nil)

      ;; Scan exploded entities
      (setq n 0 total (sslength ssExp))
      (while (< n total)
        (setq e (ssname ssExp n))
        (setq d (entget e))

        (if (= (cdr (assoc 0 d)) "LINE")
          (progn
            (setq p1 (cdr (assoc 10 d)))
            (setq p2 (cdr (assoc 11 d)))

            ;; Check if HORIZONTAL
            (if (= (cadr p1) (cadr p2))
              (progn
                ;; Higher Y = topmost
                (if (> (cadr p1) highestY)
                  (progn
                    (setq highestY (cadr p1))
                    (setq topLine e)
                  )
                )
              )
            )
          )
        )
        (setq n (1+ n))
      )

      ;; Change that ONE line to WHITE
      (if topLine
        (command "._CHPROP" topLine "" "COLOR" "7" "")
      )
    )
  )
)
)

;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 1)
;; ========================================================
(if (= mesh_key 1)
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 1.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

            ;; Move upward: 500 + 3*s + dimGap + 2*thickness
            (setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 500
     (* 3 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)


            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))
;; ------------------------------------------------------------
;; Insert "top view locking note.dwg" at Lock Hole block END point
;; ------------------------------------------------------------
(setq notePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\top view locking note.dwg")
(setq noteName "TopViewLockingNote")

(if (and lockHoleBlockEnt (findfile notePath))
  (progn
    ;; Get bounding box end point of lock hole block
    (setq obj (vlax-ename->vla-object lockHoleBlockEnt))

    (vla-GetBoundingBox obj 'minPt 'maxPt)
    (setq minPt (vlax-safearray->list minPt))
    (setq maxPt (vlax-safearray->list maxPt))

    ;; define end point = right end middle of block bbox
    (setq lockEndPt
      (list (car maxPt)
            (/ (+ (cadr minPt) (cadr maxPt)) 2.0)
            0.0
      )
    )

    ;; Insert locking note block at end point
    (command "-INSERT"
             (strcat noteName "=" notePath)
             lockEndPt
             1.0
             1.0
             0.0)
;; ------------------------------------------------------------
;; Explode inserted note block and move its text down by 100mm
;; (Safe method - no entprev needed)
;; ------------------------------------------------------------
(setq noteBlockEnt (entlast))

(if noteBlockEnt
  (progn
    ;; Explode note block
    (command "._EXPLODE" noteBlockEnt)

    ;; Select TEXT + MTEXT near the insertion point and move down
    (setq ssTxt (ssget "C"
                       (list (- (car lockEndPt) 200.0) (- (cadr lockEndPt) 200.0))
                       (list (+ (car lockEndPt) 200.0) (+ (cadr lockEndPt) 200.0))
                       (list (cons 0 "TEXT,MTEXT")
                             (cons 8 (getvar "CLAYER"))
                       )
               )
    )

    ;; if layer filter blocks it, try without layer restriction
    (if (null ssTxt)
      (setq ssTxt (ssget "C"
                         (list (- (car lockEndPt) 200.0) (- (cadr lockEndPt) 200.0))
                         (list (+ (car lockEndPt) 200.0) (+ (cadr lockEndPt) 200.0))
                         (list (cons 0 "TEXT,MTEXT"))
                 )
      )
    )

    (if ssTxt
      (progn
        (setq isOddBars (= (rem (length bar_coords) 2) 1))

(command "._MOVE"
         ssTxt
         ""
         '(0 0)
         (list (+ (/ overall_width 2.0) (if isOddBars 14.0 0.0)) 0.0 0.0)
)

        (prompt "\n✔ Note exploded and text moved down by 100mm.")
      )
      (prompt "\n⚠ No TEXT/MTEXT found to move.")
    )
  )
)



    (prompt "\nTop view locking note block inserted at lock hole end point.")
  )
  (prompt "\nERROR: Note DWG not found or lockHoleBlockEnt missing.")
)

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")
(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)
;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)
;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")
      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)

;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 2)
;; ========================================================
(if (= mesh_key 2)
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 2.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

            ;; Move upward: 500 + 3*s + dimGap + 2*thickness
            (setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 500
     (* 3 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)


            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")

(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)
;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)
;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")
      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)
;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 3)
;; ========================================================
(if (= mesh_key 3)
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 3.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

(setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 501
     (* 4 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)


            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")

(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)
;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)
;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")
      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)


;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 4)
;; ========================================================
(if (= mesh_key 4)
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 4.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

(setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 501
     (* 3 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)


            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")

(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)
;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)
;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")
      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)

;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 7)
;; ========================================================
(if (= mesh_key 7)
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 7.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

(setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 501
     (* 3 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)


            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")

(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)
;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)
;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")
      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)

;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 5)
;; ========================================================
(if (member mesh_key '(5 8))
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 5.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

            ;; Move upward: 500 + 3*s + dimGap + 2*thickness
(setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 500
     (* 3 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)

            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")
(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)

;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)

;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")

      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)
;; ========================================================
;; BLOCK INSERT + MOVE + MIRROR  (ONLY FOR mesh_key = 6)
;; ========================================================
(if (member mesh_key '(6 9))
  (progn

    ;; --------------------------
    ;; Insert block at red line
    ;; --------------------------
    (setq lockHolePath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Lock hole 6.dwg")
    (setq lockHoleName "LockHoleBlock")

    (if (and (boundp 'MGRT_redTop) MGRT_redTop)
      (progn
        (setq lockInsertPt (list (car MGRT_redTop) (cadr MGRT_redTop) 0.0))

        (if (findfile lockHolePath)
          (progn
            ;; Insert block
            (command "-INSERT"
                     (strcat lockHoleName "=" lockHolePath)
                     lockInsertPt
                     1.0
                     1.0
                     0.0)

            (setq lockHoleBlockEnt (entlast))

            ;; Move upward: 500 + 3*s + dimGap + 2*thickness
(setq dimGap (- yEndBot gapYbot))

(setq isOddBars (= (rem (length bar_coords) 2) 1))

(setq moveUp
  (+ 500
     (* 3 s)
     dimGap
     mesh_thickness
     mesh_thickness
     (if isOddBars 14.0 0.0)
  )
)


            (command "._MOVE"
                     lockHoleBlockEnt
                     ""
                     lockInsertPt
                     (list (car lockInsertPt)
                           (+ (cadr lockInsertPt) moveUp)
                           0.0))

            (prompt "\nLock Hole block inserted and moved up successfully.")
          )
          (prompt "\nERROR: Lock Hole DWG not found.")
        )
      )
      (prompt "\nERROR: MGRT_redTop not defined — red line missing.")
    )


    ;; --------------------------
    ;; MIRROR BLOCK
    ;; --------------------------
    (if (and (boundp 'lockHoleBlockEnt)
             lockHoleBlockEnt
             (boundp 'MGRT_top_y)
             (boundp 'MGRT_rect_length)
             MGRT_top_y
             MGRT_rect_length)

      (progn
        ;; Mirror axis at half the entered top-rectangle length
        (setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))
        (setq mirrorX 0.0)

        ;; Create selection set
        (setq ssLH (ssadd))
        (ssadd lockHoleBlockEnt ssLH)

        ;; Axis points
        (setq mirPt1 (list mirrorX mirrorY))
        (setq mirPt2 (list (+ mirrorX 10.0) mirrorY))

        ;; MIRROR (keep original — "N")
        (command "._MIRROR"
                 ssLH
                 ""
                 mirPt1
                 mirPt2
                 "N")

(setq lockHoleBlockMirEnt (entlast))
        (prompt "\nLock hole block mirrored successfully (mesh 1).")
(if (and lockHoleBlockEnt lockHoleBlockMirEnt)
  (progn
    (setq blkCenter1 (_blk-center lockHoleBlockEnt))
    (setq blkCenter2 (_blk-center lockHoleBlockMirEnt))

    ;; Mid X of the two block centers
    (setq midX (/ (+ (car blkCenter1) (car blkCenter2)) 2.0))

    ;; ✅ Place dimension to the RIGHT
    (setq dimX (+ midX 90.0 (/ overall_width 2.0)))

    (command "._DIMLINEAR"
             blkCenter1
             blkCenter2
             (list dimX
                   (/ (+ (cadr blkCenter1) (cadr blkCenter2)) 2.0)
                   0.0)
             "")

    (prompt "\n✅ Dimension placed to the RIGHT correctly.")
  )
)
;; ==========================================================
;; DIMENSION: Left → Block Center, and Block Center → Right
;; Offset DOWN by 50 mm
;; ==========================================================

(defun _blk-center (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn))
  (setq mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(if (and ptLeft ptRight lockHoleBlockEnt)
  (progn
    (setq blkCenter (_blk-center lockHoleBlockEnt))

    ;; First dimension: Left → Center
    (command "._DIMLINEAR"
             blkCenter
             ptLeft
             (list (/ (+ (car ptLeft) (car blkCenter)) 2.0)
                   (- (cadr ptLeft) 25.0))
             "")

    ;; Second dimension: Center → Right
    (command "._DIMLINEAR"
             blkCenter
             ptRight
             (list (/ (+ (car blkCenter) (car ptRight)) 2.0)
                   (- (cadr ptRight) 25.0))
             "")

    (prompt "\nTwo dimensions created successfully.")
  )
  (prompt "\n⚠ Required points not available.")
)
;; ==========================================================
;; DIMENSION: center of lock-hole block → bottom right end of line
;; Place dim 20 mm to the RIGHT
;; ==========================================================

;; 1) Get lock-hole block center
(vla-getboundingbox
  (vlax-ename->vla-object lockHoleBlockEnt)
  'bbMin 'bbMax)

(setq bbMin (vlax-safearray->list bbMin))
(setq bbMax (vlax-safearray->list bbMax))

(setq LH_midX (/ (+ (car bbMin) (car bbMax)) 2.0))
(setq LH_midY (/ (+ (cadr bbMin) (cadr bbMax)) 2.0))

(setq LH_center (list LH_midX LH_midY))

;; 2) Use BOTTOM endpoint of straight line
;; pRbot is already defined in your code:
;; (setq pRbot (list last_start_x botYline))

;; 3) Dimension location 20 mm to the right
(setq dimLoc
      (list (+ (car pRbot) 92.0)
            (/ (+ LH_midY botYline) 2.0)))

;; 4) Create the dimension
(command "._DIMLINEAR"
         LH_center
         pRbot
         dimLoc
         "")

;; ==========================================================
;; MIRROR the new dimension about center of the top length
;; Do NOT erase the source
;; ==========================================================

;; 1) Get last created entity (the dimension)
(setq lastDim (entlast))

;; 2) Mirror axis (same logic as your block mirror)
(setq mirrorX 0.0)
(setq mirrorY (+ MGRT_top_y (/ MGRT_rect_length 2.0)))

(setq mirPt1 (list mirrorX mirrorY))
(setq mirPt2 (list (+ mirrorX 10.0) mirrorY)) ;; vertical axis

;; 3) Mirror the dimension, keep original → "N"
(command "._MIRROR"
         lastDim
         ""
         mirPt1
         mirPt2
         "N")

      )

      (prompt "\nMirror skipped — top rectangle vars not ready.")
    )

  ) ;; end mesh_key=1

  ;; ELSE — do nothing for other mesh types
  (prompt "\nBlock insert/mirror skipped — mesh type is not 1.")
)

;; --------------------------------------------------------
;; Insert Logo block "Logo for mesh grate.dwg"
;; Insert point: move UP by mesh_height from origin
;; --------------------------------------------------------
(if (member mesh_key '(1 2 3 4))
    (progn
(setq logoBlockPath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\Logo for mesh grate.dwg")
(setq logoBlockName "LogoBlock") ;; replace with actual block name inside Logo DWG

;; Insert point: X = 0.0, Y = mesh_height, Z = 0
(setq logoInsertPt (list 0.0 mesh_height 0.0))

(if (findfile logoBlockPath)
  (progn
    (command "-INSERT"
             (strcat logoBlockName "=" logoBlockPath)
             logoInsertPt
             1.0 1.0 0.0)
    (setq logoBlockEnt (entlast))
    (prompt "\nLogo block inserted upward by mesh height from origin.")
  )
  (prompt "\nERROR: Could not find Logo DWG file at given path.")
)
;; --------------------------------------------------------
;; Move Logo block to SECOND bar's left-top point
;; Then shift LEFT by (gap_fixed / 2)
;; --------------------------------------------------------
(if (>= (length bar_coords) 2)
  (progn
    ;; Second bar (index 1)
    (setq secondBar (nth 1 bar_coords))
    (setq barLeftX (car secondBar))          ;; left face X of second bar
    (setq barTopY  (+ y_base mesh_height))   ;; top Y

    ;; Target point BEFORE left-shift
    (setq targetPt (list barLeftX barTopY 0.0))

    ;; Move block to second bar top-left
    (command "._MOVE"
             logoBlockEnt
             ""
             logoInsertPt
             targetPt)

    ;; Now move logo LEFT by half gap_fixed
    (setq shiftLeft (/ gap_fixed 2.0))

    (command "._MOVE"
             logoBlockEnt
             ""
             '(0 0 0)
             (list (- shiftLeft) 0.0 0.0))

    (prompt "\nLogo block positioned at second bar top-left, then shifted left by half gap-fixed.")
  )
  (prompt "\nERROR: Not enough bars — cannot move logo to second bar.")
)
;; --------------------------------------------------------
;; Mirror logo block about overall width center
;; Remove source ("Y")
;; --------------------------------------------------------
(setq midX (/ overall_width 2.0))   ;; center of full mesh width
(setq mirrorPt1 (list midX 0.0 0.0))
(setq mirrorPt2 (list midX 100.0 0.0)) ;; vertical mirror axis

(command "._MIRROR"
         logoBlockEnt
         ""
         mirrorPt1
         mirrorPt2
         "Y")   ;; remove source

;; --------------------------------------------------------
;; Insert TOP VIEW logo block on top of the main logo block
;; Then move it UP by 500
;; --------------------------------------------------------
(setq topLogoPath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\top view logo for mesh grate.dwg")
(setq topLogoName "TopViewLogo")

;; Get insertion point of the existing LogoBlock
(setq logoInsertData (entget logoBlockEnt))
(setq logoInsertPoint (cdr (assoc 10 logoInsertData)))  ;; insertion point of logo

(if (findfile topLogoPath)
  (progn
    ;; Insert the top view logo at same point
    (command "-INSERT"
             (strcat topLogoName "=" topLogoPath)
             logoInsertPoint
             1.0 1.0 0.0)

    (setq topLogoEnt (entlast))
    (setq dimGap (- yEndBot gapYbot))

;; --------------------------------------------------------
;; MOVE UP by: 500 + 3*thickness + dimension + (s/2)
;; --------------------------------------------------------
;; Safety checks
(if (not (numberp mesh_thickness)) (setq mesh_thickness 0))
(if (not (numberp dimGap)) (setq dimGap 0))
(if (not (numberp s)) (setq s 0))
(if (not (numberp MGRT_rect_length)) (setq MGRT_rect_length 0))

;; MoveUp = 500 + top-view-length - (2.5 * thickness) - dimGap - (s / 2)
(setq moveUp
      (+ 500
         MGRT_rect_length
         (* -0.5 mesh_thickness)
         (* -1 dimGap)
         (/ (- s) 2.0)
      )
)

(command "._MOVE"
         topLogoEnt
         ""
         '(0 0 0)
         (list 0 moveUp 0))


    (prompt "\nTop view logo inserted and moved up by 500.")
  )
  (prompt "\nERROR: Could not find top view logo DWG.")
)


))

;; ---------------------------------------------------------
(defun _yellow-to-verdeckt ()
  (setq ss (ssget "_X" '((0 . "LINE") (62 . 2)))) ; select all lines with color = yellow (ACI 2)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (entmod (subst (cons 8 "VERDECKT") (assoc 8 (entget (ssname ss i))) (entget (ssname ss i))))
        (entupd (ssname ss i))
        (setq i (1+ i))
      )
    )
  )
  (princ "\nAll yellow lines moved to layer VERDECKT.")
)
(_yellow-to-verdeckt)

;; ---------------------------------------------------------
(defun _red-to-mitte ()
  (setq ss (ssget "_X" '((0 . "LINE") (62 . 1)))) ; select all lines with color = red (ACI 1)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (entmod (subst (cons 8 "MITTE") (assoc 8 (entget (ssname ss i))) (entget (ssname ss i))))
        (entupd (ssname ss i))
        (setq i (1+ i))
      )
    )
  )
  (princ "\nAll red lines moved to layer MITTE.")
)
(_red-to-mitte)

(defun _rotated-dims-to-bemass-only (/ ss i ent ed)
  ;; Select all DIMENSION entities
  (setq ss (ssget "_X" '((0 . "DIMENSION"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        ;; Check if the dimension is ROTATED (DXF 70 flag = 32)
        (if (= (cdr (assoc 70 ed)) 32)
          (progn
            ;; Set layer to BEMASS
            (setq ed (subst (cons 8 "BEMASS") (assoc 8 ed) ed))
            (entmod ed)
            (entupd ent)
          )
        )
        (setq i (1+ i))
      )
      (princ "\n✔ All rotated dimensions moved to layer BEMASS.")
    )
    (princ "\n⚠ No dimensions found.")
  )
  (princ)
)
(_rotated-dims-to-bemass-only)

;; ---------------- Erase 4th block (Verdect) ----------------
(if (and verdectBlockEnt (entget verdectBlockEnt))
  (entdel verdectBlockEnt)
)


;; ====================================================================
;; ROTATE only objects whose TOP (max Y) is ≥ 400
;; ====================================================================
(setq ssAll (ssget "_ALL"))
(setq ssR   (ssadd))   ;; final rotate set

(if ssAll
  (progn
    (setq i 0)
    (while (< i (sslength ssAll))
      (setq e (ssname ssAll i))
      (setq ed (entget e))

      ;; get bounding box
      (vla-getboundingbox
        (vlax-ename->vla-object e)
        'minPt 'maxPt)

      (setq maxY (cadr (vlax-safearray->list maxPt)))

      ;; if TOP Y ≥ 400 → add to rotate set
      (if (>= maxY 400)
        (ssadd e ssR)
      )

      (setq i (1+ i))
    )

    ;; ROTATE final set
    (if (> (sslength ssR) 0)
      (command "._ROTATE" ssR "" (list 0 500) "270")
      (prompt "\nNothing above Y=400.")
    )
  )
)
;; ====================================================================
;; MOVE previous selection UP by overall_width
;; (Equivalent to: M → P → 0,0 → @0,overall_width)
;; ====================================================================

(if overall_width
  (command "._MOVE" "_P" "" '(0 0 0) (list 0 overall_width 0))
  (prompt "\n⚠ overall_width is not defined.")
)
;; ============================================================
;; ✅ Apply BEMASS$0 to ALL dimensions WITHOUT moving them
;; ============================================================
(if (tblsearch "DIMSTYLE" "BEMASS$0")
  (progn
    (setq ssAllDim (ssget "_X" '((0 . "DIMENSION"))))

    (if (and ssAllDim (> (sslength ssAllDim) 0))
      (progn
        ;; ✅ stop AutoCAD moving dim text when style changes
        (setvar "DIMTMOVE" 2)  ;; keep text position
        (setvar "DIMTAD" 1)    ;; centered vertical
        (setvar "DIMTVP" 0.0)  ;; no vertical offset
        (setvar "DIMTIH" 0)
        (setvar "DIMTOH" 0)
        (setvar "DIMTIX" 0)

        ;; apply style
        (command "._DIMSTYLE" "R" "BEMASS$0")
        (command "._DIMSTYLE" "A" ssAllDim "")

        (prompt "\n✅ BEMASS$0 applied to all dimensions without shifting.")
      )
      (prompt "\n⚠ No dimensions found.")
    )
  )
  (prompt "\n⚠ DIMSTYLE 'BEMASS$0' not found in drawing.")
)
(vl-load-com)

;; --------------------------------------------------------
;; Insert "section notes.dwg" at RIGHT EDGE end of rectangle
;; then move based on mesh_key 4 & 7 and odd/even bars
;; --------------------------------------------------------
(setq sectionNotesPath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\section notes.dwg")
(setq sectionNotesBlk  "SectionNotes") ;; must match block name inside DWG

(if (and MGRT_rectTopRight (findfile sectionNotesPath))
  (progn
    ;; Insert
    (command "-INSERT"
             (strcat sectionNotesBlk "=" sectionNotesPath)
             MGRT_rectTopRight
             1.0 1.0 0.0
    )
    (setq sectionEnt (entlast))

    (if sectionEnt
      (progn
        ;; --------------------------------------------------------
        ;; MOVE LOGIC
        ;; mesh 4 & 7:
        ;;   even bars -> left 19, down 2
        ;;   odd bars  -> down 2 only
        ;; others -> left 7, down 2 (original)
        ;; --------------------------------------------------------
        (cond
          ;; Only mesh 4 & 7
          ((member mesh_key '(4 7))
            (if (= (rem (length bar_coords) 2) 0)  ;; even bars
              (command "._MOVE" sectionEnt "" '(0 0) (list -13.5 -2.0))
              (command "._MOVE" sectionEnt "" '(0 0) (list  0.0 -2.0)) ;; odd bars
            )
          )

          ;; all other mesh types (your old default)
          (T
            (command "._MOVE" sectionEnt "" '(0 0) (list -7.0 -2.0))
          )
        )

        (prompt "\nSection notes block inserted and moved successfully.")
      )
      (prompt "\nERROR: Could not capture inserted section notes entity.")
    )
  )
  (prompt "\nERROR: section notes block path not found or MGRT_rectTopRight missing.")
)
(princ)

;; --------------------------------------------------------------
;; 1️⃣ Scale objects in first rectangle: (0,0) → (600,144)
;; --------------------------------------------------------------
(setq ss1 (ssget "C" (list 0.0 0.0) (list 2000.0 144.0)))

(if ss1
  (progn
    ;; Scale selected objects using (0,0) as base point
    (command "SCALE" ss1 "" (list 0.0 0.0) "2")
    (prompt "\n✔ Objects inside window from (0,0) to (2000,144) have been scaled by 2.")
  )
  (prompt "\n⚠ No objects found inside first window.")
)

;; --------------------------------------------------------
;; Insert "section box notes.dwg" at ORIGIN
;; move: Right (2*overall_width + 100) and Down 20
;; then EXPLODE
;; --------------------------------------------------------
(setq sectionBoxNotesPath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\section box notes.dwg")
(setq sectionBoxNotesBlk  "SectionBoxNotes") ;; must match block name inside DWG

(if (and (findfile sectionBoxNotesPath) overall_width)
  (progn
    ;; Insert at origin
    (command "-INSERT"
             (strcat sectionBoxNotesBlk "=" sectionBoxNotesPath)
             '(0 0 0)
             1.0 1.0 0.0
    )

    (setq sectionBoxEnt (entlast))

    (if sectionBoxEnt
      (progn
        ;; Move Right by (2*overall_width + 100) and Down by 20
        (command "._MOVE"
                 sectionBoxEnt
                 ""
                 '(0 0 0)
                 (list (+ (* 2.0 overall_width) 100.0) -20.0 0.0)
        )

        ;; Explode inserted block
        (command "._EXPLODE" sectionBoxEnt)

        (prompt "\nSection box notes inserted, moved and exploded successfully.")
      )
      (prompt "\nERROR: Could not capture inserted entity.")
    )
  )
  (prompt "\nERROR: Block path not found OR overall_width missing.")
)

(princ)

;; --------------------------------------------------------------
;; Ask user: Material + Surface
;; Then replace full Rostinfo note and update Breite using overall_width
;; MW line changes based on mesh_key
;; TS line changes based on mesh_height / mesh_thickness
;; --------------------------------------------------------------
(defun _replace-rostinfo-note
       ( / matOpt surfOpt matLine surfLine mwLine tsLine
           newBreiteTxt fullNote ss i ent ed txt cnt
       )

  ;; -----------------------------
  ;; Ask Material
  ;; -----------------------------
  (initget "Stainless Galvanized")
  (setq matOpt (getkword "\nSelect the material [Stainless/Galvanized]: "))

  (if (null matOpt)
    (setq matOpt "Stainless")
  )

  ;; Material line
  (cond
    ((= matOpt "Stainless")
      (setq matLine "Material: V2A/1.4301")
    )
    ((= matOpt "Galvanized")
      (setq matLine "Material: St 37/HOTZINK")
    )
  )

  ;; -----------------------------
  ;; Ask Surface type
  ;; -----------------------------
  (initget "Glatt Einfachgleitschutz")
  (setq surfOpt (getkword "\nSelect the surface [Glatt/Einfachgleitschutz]: "))

  (if (null surfOpt)
    (setq surfOpt "Glatt")
  )

  ;; Surface line
  (cond
    ((= surfOpt "Glatt")
      (setq surfLine "glatt / hladký")
    )
    ((= surfOpt "Einfachgleitschutz")
      (setq surfLine "Einfachgleitschutz (gekerbt)")
    )
  )

  ;; -----------------------------
  ;; MW Line based on mesh_key
  ;; -----------------------------
  (cond
    ((member mesh_key '(1 2))
      (setq mwLine "MW: 33x12.5")
    )
    ((member mesh_key '(3 4))
      (setq mwLine "MW: 33x13.5")
    )
    ((member mesh_key '(5 6 7))
      (setq mwLine "MW: 33.3x11.1")
    )
    (T
      (setq mwLine "MW: 33x12.5") ;; fallback
    )
  )

  ;; -----------------------------
  ;; TS Line dynamic
  ;; -----------------------------
  (setq tsLine
    (strcat
      "TS: "
      (rtos mesh_height 2 0)
      "/"
      (rtos mesh_thickness 2 0)
      "mm"
    )
  )

  ;; -----------------------------
  ;; Breite line dynamic
  ;; -----------------------------
  (setq newBreiteTxt (strcat "Breite: " (rtos overall_width 2 0) " (TS)"))

  ;; -----------------------------
  ;; Full note (MTEXT new line = \P)
  ;; -----------------------------
  (setq fullNote
    (strcat
      "Rostinfo (Gitterrost)\\P"
      surfLine "\\P"
      matLine "\\P"
      tsLine "\\P"
      mwLine "\\P"
      newBreiteTxt "\\P"
      "Rostarretierung: ja"
    )
  )

  ;; -----------------------------
  ;; Replace in drawing (TEXT+MTEXT containing Breite + TS)
  ;; -----------------------------
  (setq cnt 0)
  (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))

        (setq ent (ssname ss i))
        (setq ed  (entget ent))
        (setq txt (cdr (assoc 1 ed)))

        (if (and txt
                 (vl-string-search "BREITE:" (strcase txt))
                 (vl-string-search "(TS)"    (strcase txt))
            )
          (progn
            ;; Replace ENTIRE note
            (setq ed (subst (cons 1 fullNote) (assoc 1 ed) ed))
            (entmod ed)
            (entupd ent)
            (setq cnt (1+ cnt))
          )
        )

        (setq i (1+ i))
      )

      (prompt (strcat "\n✔ Rostinfo note updated: " (itoa cnt) " time(s)."))
    )
    (prompt "\n⚠ No TEXT/MTEXT found.")
  )

  (princ)
)

(_replace-rostinfo-note)
(princ)


;; --------------------------------------------------------
;; Explode inserted logo block at end (only for mesh 1,2,3,4)
;; --------------------------------------------------------
(if (and (member mesh_key '(1 2 3 4)) logoBlockEnt)
  (progn
    (command "_.EXPLODE" logoBlockEnt)
    (prompt "\nLogo block exploded successfully.")
  )
)
(princ)

(vl-load-com)

;; --------------------------------------------------------------
;; Explode ONLY "SectionNotes" block inside window (0,0)-(2000,144)
;; Works for mesh 1..7
;; --------------------------------------------------------------
(setq ssBlk (ssget "C"
                   (list 0.0 0.0)
                   (list 2000.0 144.0)
                   '((0 . "INSERT") (2 . "SectionNotes"))
           )
)

(if ssBlk
  (progn
    (setq i 0)
    (repeat (sslength ssBlk)
      (setq ent (ssname ssBlk i))
      (setq obj (vlax-ename->vla-object ent))

      ;; explode safely
      (vl-catch-all-apply 'vlax-invoke (list obj 'Explode))

      ;; delete the original insert after explode
      (vl-catch-all-apply 'vla-delete (list obj))

      (setq i (1+ i))
    )

    (prompt "\n✔ SectionNotes block exploded successfully in window.")
  )
  (prompt "\n⚠ No SectionNotes blocks found in window to explode.")
)
(princ)


(vl-load-com)

(setq dimScaleBlockPath "C:\\Users\\PRamesh\\Documents\\Inotec\\Mesh Grate blocks\\dim scale 0.5.dwg")
(setq dimScaleBlockName "DimScaleBlock")
(setq insertPt (list -100.0 0.0 0.0))

(if (findfile dimScaleBlockPath)
  (progn
    ;; Insert block
    (command "-INSERT" (strcat dimScaleBlockName "=" dimScaleBlockPath) insertPt 1.0 1.0 0.0)
    (setq blkEnt (entlast))

    (if blkEnt
      (progn
        ;; Explode
        (command "_.EXPLODE" blkEnt)

        ;; Exploded objects selection set
        (setq ssExp (ssget "P"))

        ;; Find one DIMENSION from exploded as source
        (setq srcDim nil)
        (if ssExp
          (progn
            (setq i 0)
            (repeat (sslength ssExp)
              (setq e (ssname ssExp i))
              (if (= (cdr (assoc 0 (entget e))) "DIMENSION")
                (setq srcDim e)
              )
              (setq i (1+ i))
            )
          )
        )

        ;; Window select only target dimensions
        (setq ss1 (ssget "C"
                         (list 0.0 0.0)
                         (list 2000.0 144.0)
                         '((0 . "DIMENSION"))
                   )
        )

        ;; Matchprop
        (if (and srcDim ss1)
          (progn
            (command "_.MATCHPROP" srcDim ss1 "")
            (prompt "\nSUCCESS: Matchprop applied.")
          )
          (prompt "\nERROR: Could not find source dim or target dims.")
        )

        ;; ✅ DELETE exploded geometry (the actual remaining objects)
        (if ssExp
          (progn
            (setq i 0)
            (repeat (sslength ssExp)
              (entdel (ssname ssExp i))
              (setq i (1+ i))
            )
            (prompt "\nExploded block objects deleted successfully.")
          )
          (prompt "\nNo exploded objects found to delete.")
        )
      )
      (prompt "\nERROR: Block insertion failed.")
    )
  )
  (prompt "\nERROR: Could not find DWG file at given path.")
)
;; --------------------------------------------------------------
;; Replace string based on mesh_key:
;;  mesh 1,2 : 31.3x18x2 -> 31x18x2
;;  mesh 3,4 : 31.3x18x2 -> 30x18x2
;;  mesh 7   : 31.3x18x2 -> 30.3x18x2
;; --------------------------------------------------------------
(defun _replace-size-based-on-mesh ( / ss i ent edata txt newtxt oldStr repStr)

  (setq oldStr "31.3x18x2")
  (setq repStr nil)

  ;; Decide replacement string
  (cond
    ((member mesh_key '(1 2)) (setq repStr "31x18x2"))
    ((member mesh_key '(3 4)) (setq repStr "30x18x2"))
    ((= mesh_key 7)           (setq repStr "30.3x18x2"))
    (T                        (setq repStr nil))
  )

  ;; If no replacement needed, exit
  (if (null repStr)
    (progn
      (prompt "\nNo replacement required for this mesh_key.")
      (princ)
    )
    (progn
      ;; Select all TEXT + MTEXT
      (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))

      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent   (ssname ss i))
            (setq edata (entget ent))
            (setq txt   (cdr (assoc 1 edata)))
            (setq newtxt txt)

            ;; Replace only if found
            (if (and txt (vl-string-search oldStr txt))
              (setq newtxt (vl-string-subst repStr oldStr txt))
            )

            ;; Update entity
            (if (/= txt newtxt)
              (progn
                (setq edata (subst (cons 1 newtxt) (assoc 1 edata) edata))
                (entmod edata)
                (entupd ent)
              )
            )

            (setq i (1+ i))
          )

          (prompt
            (strcat "\n✔ Done. Replaced " oldStr " -> " repStr))
        )
        (prompt "\n⚠ No TEXT/MTEXT found.")
      )
    )
  )
)

(_replace-size-based-on-mesh)

(princ)
;; --------------------------------------------------------------
;; Replace string based on mesh_key:
;; mesh 2,4,6,7 : 7.5x12 -> 10x14
;; --------------------------------------------------------------
(defun _replace-75x12-based-on-mesh ( / ss i ent edata txt newtxt oldStr repStr)

  (setq oldStr "7.5x12")
  (setq repStr nil)

  ;; Decide replacement string
  (if (member mesh_key '(2 4 6 7))
    (setq repStr "10x14")
    (setq repStr nil)
  )

  ;; If no replacement needed, exit
  (if (null repStr)
    (progn
      (prompt "\nNo replacement required for this mesh_key.")
      (princ)
    )
    (progn
      ;; Select all TEXT + MTEXT
      (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))

      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent   (ssname ss i))
            (setq edata (entget ent))
            (setq txt   (cdr (assoc 1 edata)))
            (setq newtxt txt)

            ;; Replace only if found
            (if (and txt (vl-string-search oldStr txt))
              (setq newtxt (vl-string-subst repStr oldStr txt))
            )

            ;; Update entity
            (if (/= txt newtxt)
              (progn
                (setq edata (subst (cons 1 newtxt) (assoc 1 edata) edata))
                (entmod edata)
                (entupd ent)
              )
            )

            (setq i (1+ i))
          )

          (prompt (strcat "\n✔ Done. Replaced " oldStr " -> " repStr))
        )
        (prompt "\n⚠ No TEXT/MTEXT found.")
      )
    )
  )
)

(_replace-75x12-based-on-mesh)


(command "._LTSCALE" "14")
(command "OSMODE" 16383)
(command "ORTHOMODE" 1)

  (princ)
)

