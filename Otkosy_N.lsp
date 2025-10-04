;; ---------------------------------------------
;; Откосы между двумя полилиниями (NanoCAD/AutoCAD)
;; Команда: OTKOSY
;; Слой: "Откос_Штриховка"
;; FULL  — сплошные через gap
;; TICKS — длинные через 2*gap (от линии до линии) +
;;         короткие со сдвигом gap (0.5 расстояния)
;; ---------------------------------------------

(defun _vec-sub (a b) (mapcar '- a b))
(defun _vec-add (a b) (mapcar '+ a b))
(defun _vec-len (v) (distance '(0 0 0) v))
(defun _vec-unit (v)
  (if (equal (_vec-len v) 0.0 1e-9)
    '(0 0 0)
    (mapcar '(lambda (c) (/ c (_vec-len v))) v)
  )
)
(defun _pt3 (p) (if (= (length p) 2) (append p (list 0.0)) p))

(defun _mk-layer (name col ltype / doc laytbl lay)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq laytbl (vla-get-Layers doc))
  (if (not (tblsearch "LAYER" name))
    (progn
      (setq lay (vla-Add laytbl name))
      (if col (vla-put-Color lay col))
      (if ltype
        (progn
          (if (not (tblsearch "LTYPE" ltype))
            (vl-catch-all-apply 'vla-Load (list (vla-get-Linetypes doc) ltype "acad.lin"))
          )
          (vla-put-Linetype lay ltype)
        )
      )
    )
  )
  name
)

(defun _pick-curve (msg / sel obj)
  (while (not obj)
    (setq sel (entsel msg))
    (cond
      ((null sel) (princ "\nНичего не выбрано."))
      (T
       (setq obj (vlax-ename->vla-object (car sel)))
       (if (vl-catch-all-error-p (vl-catch-all-apply '(lambda () (vlax-curve-getEndParam obj))))
         (progn (princ "\nОбъект не является кривой (полилиния/линия/дуга/сплайн).") (setq obj nil))
       )
      )
    )
  )
  obj
)

(defun _total-length (crv)
  (vlax-curve-getDistAtParam crv (vlax-curve-getEndParam crv))
)

(defun _draw-line (p1 p2 layername /)
  (entmakex
    (list
      (cons 0 "LINE")
      (cons 8 layername)
      (cons 10 (list (car p1) (cadr p1) (caddr p1)))
      (cons 11 (list (car p2) (cadr p2) (caddr p2)))
    )
  )
)

(defun _layer-ensure-on (name / doc lay)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (not (tblsearch "LAYER" name))
    (vla-Add (vla-get-Layers doc) name)
  )
  (setq lay (vla-item (vla-get-Layers doc) name))
  (vla-put-Lock lay :vlax-false)
  (vla-put-Freeze lay :vlax-false)
  (vla-put-LayerOn lay :vlax-true)
  name
)

(defun c:OTKOSY ( / crvA crvB gap mode len d pA pB vec unit pEnd *error* oldcmde oldlay layerName)
  (vl-load-com)
  (defun *error* (msg)
    (if oldcmde (setvar 'CMDECHO oldcmde))
    (if oldlay (setvar 'CLAYER oldlay))
    (command "_.UNDO" "_END")
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  (setq oldcmde (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (command "_.UNDO" "_BEGIN")

  ;; слой
  (setq layerName "Откос_Штриховка")
  (_mk-layer layerName 8 nil)
  (_layer-ensure-on layerName)
  (setq oldlay (getvar 'CLAYER))
  (setvar 'CLAYER layerName)

  ;; ❌ удаление отключено — теперь старые откосы сохраняются

  ;; выбор кривых
  (setq crvA (_pick-curve "\nВыберите первую полилинию/кривую (базовую): "))
  (setq crvB (_pick-curve "\nВыберите вторую полилинию/кривую (целевую): "))

  ;; расстояние между соседними штрихами (ед. чертежа, по умолчанию 2)
  (setq gap (getreal "\nРасстояние между длинным и коротким штрихом <2.0>: "))
  (if (or (null gap) (<= gap 0.0)) (setq gap 2.0))

  (initget "FULL TICKS")
  (setq mode (getkword "\nРежим линий: [FULL/TICKS] <TICKS>: "))
  (if (null mode) (setq mode "TICKS"))

  (setq len (_total-length crvA))

  (cond
    ;; FULL: линии через gap
    ((= mode "FULL")
      (setq d 0.0)
      (while (<= d len)
        (setq pA (vlax-curve-getPointAtDist crvA d)
              pB (vlax-curve-getClosestPointTo crvB pA))
        (if (and pA pB) (_draw-line (_pt3 pA) (_pt3 pB) layerName))
        (setq d (+ d gap))
      )
    )

    ;; TICKS: длинные через 2*gap; короткие со сдвигом gap (0.5 от расстояния)
    ((= mode "TICKS")
      ;; длинные
      (setq d 0.0)
      (while (<= d len)
        (setq pA (vlax-curve-getPointAtDist crvA d)
              pB (vlax-curve-getClosestPointTo crvB pA))
        (if (and pA pB) (_draw-line (_pt3 pA) (_pt3 pB) layerName))
        (setq d (+ d (* 2.0 gap)))
      )
      ;; короткие
      (setq d gap)
      (while (<= d len)
        (setq pA (vlax-curve-getPointAtDist crvA d)
              pB (vlax-curve-getClosestPointTo crvB pA))
        (if (and pA pB)
          (progn
            (setq pA (_pt3 pA) pB (_pt3 pB)
                  vec (_vec-sub pB pA))
            (if (> (_vec-len vec) 1e-8)
              (progn
                (setq unit (_vec-unit vec))
                (setq pEnd (_vec-add pA (mapcar '(lambda (c) (* c (* 0.5 (_vec-len vec)))) unit)))
                (_draw-line pA pEnd layerName)
              )
            )
          )
        )
        (setq d (+ d (* 2.0 gap)))
      )
    )
  )

  (setvar 'CLAYER oldlay)
  (setvar 'CMDECHO oldcmde)
  (command "_.UNDO" "_END")
  (princ (strcat "\nГотово. Старые откосы сохранены. Зазор = " (rtos gap 2 2) ". Команда: OTKOSY"))
  (princ)
)
