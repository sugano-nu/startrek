;; StarTrek for Emacs Lisp
;; Version 0.01
;; Translated by SUGANO Tsuyoshi
;; With Emacs Org-mode Babel envrionment and tangling Emacs Lisp codes.
;; 
;; I am so much grateful to:
;; Original BASIC version
;;     http://www.dunnington.u-net.com/public/startrek/startrek.txt
;;     (some information in http://www.dunnington.u-net.com/public/startrek/)
;;
;; Rewritten Version in Common Lisp by Shozo TAKEOKA (take atmark axe-inc.co.jp)
;; http://www.takeoka.org/~take/

(defun insert-params (x)
  (set-buffer (get-buffer-create "*startrek-params*"))
  (goto-char (point-max))
  (insert (format "%s\n" x))
  ;;(end-of-buffer)
  ;;(recenter-top-bottom)
  (set-buffer (get-buffer-create "*startrek*"))
  )

(defun startrek-initialize ()
  (require 'cl)
  ;; defstruct klingon
  (cl-defstruct klingon
    (x 0)
    (y 0)
    (energy 0))

  ;; defstruct quad 天体高度観測器
  (cl-defstruct quad
    (base 0)
    (star 0)
    (klingon 0)
    (visit nil))

  ;; 方向ベクトル
  (defconst *cx*
    '[-1 -1 0 1 1 1 0 -1 -1])
  (defconst *cy*
    '[0 1 1 1 0 -1 -1 -1 0])

  ;;定数 Constant
  (defconst *full-energy* 3000)
  (defconst *full-torpedo* 10) ; max of torpedoes
  (defconst *klingon-max-energy* 200)

  ;; グローバル変数 Global var

  (defvar *kkk* '[0 0 0 0])               ; => *kkk*

  ;; ~*ggg*~
  (defvar *ggg*
    '[[0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0]])

  ;;定数の初期設定 defvar
  (defvar *time* 0)                       ; => *time*
  (defvar *time0* 0)                      ; => *time0*
  (defvar *t-period* 0)                   ; => *t-period*

  (defvar *base-total* 0)    ; Base total No.
  (defvar *klingon-total* 0) ; Klingon total No.

  (defvar *c-klingons* 0) ; current-Klingons
  (defvar *c-bases* 0)    ; current-Bases
  (defvar *c-stars* 0)    ; current-Stars

  (defvar *bx* 0) ; Base-X pos in Quad
  (defvar *by* 0) ; Base-Y pos in Quad

  (defvar *ex* 0) ; Enterprise X pos in Quad
  (defvar *ey* 0) ; Enterprise Y pos in Quad
  (defvar *qx* 0) ; Quadorant X
  (defvar *qy* 0) ; Quadorant Y
  (defvar *energy* 0)
  (defvar *torpedo* 0)  ; No. of torpedoes
  (defvar *shield* 0)   ; shield
  (defvar *docked* nil) ; docked

  ;; グローバルフラグ global flags
  (defvar *klingon-attack* nil) ; turn of Klingon
  (defvar *success* nil)        ; success flag
  (defvar *mission-end* nil)    ; mission terminated

  (setq *sec*
        '[[nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]])

  ;;(insert-*sec*)
  )

(defun insert-GB (x)
        (insert (propertize x 'face '(:foreground "green" :background "black"))))
(defun insert-YB (x)
        (insert (propertize x 'face '(:foreground "yellow" :background "black"))))
(defun insert-RB (x)
        (insert (propertize x 'face '(:foreground "red" :background "black"))))
(defun insert-CB (x)
        (insert (propertize x 'face '(:foreground "cyan" :background "black"))))

(defun startrek-title ()
  (insert-GB "THE USS ENTERPRISE --- NCC-1701\n")
  (insert-GB "                  ,------*------,\n")
  (insert-GB "  ,-------------   '---  ------'\n")
  (insert-GB "   '-------- --'      / /\n")
  (insert-GB "       ,---' '-------/ /--,\n")
  (insert-GB "        '----------------'\n")
  )

(defmacro aset-anv (a n v)
  `(setf (aref ,a ,n) ,v))

(defmacro aset2-axyv (a x y  v)
  `(setf (aref ,a ,x ,y ) ,v))

(defun startrek-init ()
  ;;(insert-params "trek-init\n")

  (setq *damage-repair-magic-number* (/ (random 50) 100.0)) ;D4=.5*RND(1)
  ;; (setq *bbb* (make-array 2 :initial-element 0))
  (setq *success* nil)
  (setq *mission-end* nil)
  (setq *klingon-attack* nil)
  (setq *klingon-org* 0)
  (dotimes (i 4)
    (setf (aref *kkk* i) (make-klingon)))
  (setq *time* (* (+ (random 20) 20) 100)) ; current time
  (setq *time0* *time*) ; initial time
  (setq *t-period* (+ (random 10) 25)) ; end time
  (setq *docked* nil) ; docked
  (setq *energy* *full-energy*)
  (setq *torpedo* *full-torpedo*) ;No. of torpedoes
  (setq *shield* 0)  ;shield

  (setq *sec*
        '[[nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]
          [nil nil nil nil nil nil nil nil]])
  )

(defun startrek-init2 ()
  ;;(insert-params "(startrek-init2)\n")

  (setq *qx* (random 8)) ;Quadorant X
  (setq *qy* (random 8)) ;Quadorant Y
  (setq *ex* (random 8)) ;Sector X
  (setq *ey* (random 8)) ;Sector Y
  (setq *ddd* (make-vector 10 0)) ;no Damage

  (insert-params (format "Quadorant X *qx*: %s\n" *qx*))
  (insert-params (format "Quadorant Y *qy*: %s\n" *qy*))
  (insert-params (format "Sector X: *ex*: %s\n" *ex*))
  (insert-params (format "Sector Y: *ey*: %s\n" *ey*))
  (insert-params (format "no Damage: *ddd* %s\n" *ddd*))
  )

(defun klingon-distance (i)
  (let* (
         (k (aref *kkk* i))
         (xx1 (- (klingon-x k) *ex*))
         (xx2 (- (klingon-y k) *ey*)))
    (floor (+ (sqrt (+ (* xx1 xx1) (* xx2 xx2))) 0.5))))

;; 475 DEF FNR(R)=INT(RND(R)*7.98+1.01)
(defun rnd1-8 ()
  (1+ (random 8)))

(defun fnrand ()
  (random 8))

(defun startrek ()
    (interactive)

    (require 'cl)
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "*startrek*"))

    ;; (setq buffer-read-only t)
    (kill-all-local-variables)
    ;;(erase-buffer)

    (init-sector-reset-*sec*)

    (split-window-right)
    (startrek-title)

    (startrek-initialize)

    (other-window 1)
    (switch-to-buffer (get-buffer-create "*startrek-params*"))
    (erase-buffer)

    (other-window 1)
    (switch-to-buffer (get-buffer-create "*startrek*"))

    ;;   (loop
    ;;(goto-char (point-max))
    ;;   (insert "(startrek1 start)\n")
    (startrek1)
    ;;    ;;(if (not (more-mission)) (return))
    ;;(goto-char (point-max))
    ;;     (insert "(startrek1 end)\n")
;;    (let ((inhibit-read-only t))
      (insert-GB (format "\n\n*** END ***\n"))
;;      )
    ;;    )
    )

;;(defun startrek1 ()
  (cl-defun startrek1 ()
    (interactive)
    (switch-to-buffer (get-buffer-create "*startrek*"))

    (startrek-init)
    (startrek-init2)
    (make-galaxy)

;;      (let ((inhibit-read-only t))
        (print-mission)
;;        )

    ;; (if (not (acceptp))
    ;;     (progn
    ;;       (insert "N\n")
    ;;       (return-from startrek1))
    ;;   )

    ;;;
    ;;(insert "enter-quad\n")
    (enter-quad)

    ;;(insert "(catch 'game-end (mloop)) start\n")
    (catch 'game-end 
      (mloop))
    ;;(insert "(catch 'game-end (mloop)) end\n")

    (when (or *success* (<= *klingon-total* 0))
      (success)))

(cl-defun startrek1 ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*startrek*"))

  (startrek-init)
  (startrek-init2)

  (insert-*sec*)
  )

(defun make-galaxy1 ()
  (let (k3 b3)
    (setq *base-total* 0) ; Base total No.
    (setq *klingon-total* 0) ; Klingon total No.

    (dotimes (i 8)
      (dotimes (j 8)
        (incf *klingon-total*
              (setq k3
                    (cond ((> (setq r (random 100)) 98) 3)
                          ((> r 95) 2)
                          ((> r  8) 1)
                          (t 0))))
        (incf *base-total* 
              (setq b3
                    (cond ((> (random 100) 96) 1)
                          (t 0))))
        (setf
         ;;(aref *ggg* i j)
         (aref (aref *ggg* i) j)
         (make-quad 
          :klingon k3
          :base b3
          :star (rnd1-8)
          )
         )
        ))

    (set-buffer (get-buffer-create "*startrek-params*"))
    ;;(erase-buffer)
    (insert (format "*klingon-total*: %s\n" *klingon-total*))
    (insert (format "*base-total*: %s\n" *base-total*))
    (insert (format "*ggg*: %s\n" (length *ggg*)))

    (insert (format "*ggg*: %s\n" (aref (aref *ggg* 0) 0)))

    (insert-params "*ggg*\n")
    (insert-params-*ggg*)

    (set-buffer (get-buffer-create "*startrek*"))
    )
  )

(set-buffer (get-buffer-create "*startrek-params*"))
(make-galaxy1)

(defun insert-params-*ggg* ()
  (insert-params
   (format "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n"
           (cdr (append (aref (aref *ggg* 0) 0) nil))
           (cdr (append (aref (aref *ggg* 1) 1) nil))
           (cdr (append (aref (aref *ggg* 1) 2) nil))
           (cdr (append (aref (aref *ggg* 1) 3) nil))
           (cdr (append (aref (aref *ggg* 1) 4) nil))
           (cdr (append (aref (aref *ggg* 1) 5) nil))
           (cdr (append (aref (aref *ggg* 1) 6) nil))
           (cdr (append (aref (aref *ggg* 1) 7) nil))
           )))
;; (insert-params-*ggg*)

(defun insert-*sec* ()
;;    (let ((inhibit-read-only t))
      (insert
       (format "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n"
               (aref *sec* 0)
               (aref *sec* 1)
               (aref *sec* 2)
               (aref *sec* 3)
               (aref *sec* 4)
               (aref *sec* 5)
               (aref *sec* 6)
               (aref *sec* 7)
               ))
      (insert "\n")
;;      )
)
  ;; (insert-params-*sec*)

(defun *ggg*-element (x)
  (cdr (append (aref (aref *ggg* 1) 1) nil))
  )

(defun make-galaxy ()
  (make-galaxy1)
  (if(> *klingon-total* *t-period*) (setq *t-period* (1+ *klingon-total*)))
  (cond ((zerop *base-total*)
         ;;(cond ((< (quad-klingon (aref *ggg* *qx* *qy*)) 2)
         (cond ((< (quad-klingon (aref (aref *ggg* *qx*) *qy*)) 2)
                (incf *klingon-total*)
                ;;(incf (quad-klingon (aref *ggg* *qx* *qy*)))))
                (incf (quad-klingon (aref (aref *ggg* *qx*) *qy*)))))
         (setq *base-total* 1)
         ;;(incf (quad-base (aref *ggg* *qx* *qy*)))
         (incf (quad-base (aref (aref *ggg* *qx*) *qy*)))
         (setq *qx* (fnrand))
         (setq *qy* (fnrand))))
  (setq *klingon-org* *klingon-total*))

(defun print-mission ()
  (insert-GB (format "YOUR ORDERS ARE AS FOLLOWS:\n"))
  (insert-GB (format "--------------------------\n"))
  (insert-GB (format "   DESTROY THE %s KLINGON WARSHIPS WHICH HAVE INVADED\n"
                     *klingon-total*))
  (insert-GB (format "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS\n"))
  (insert-GB (format "   ON STARDATE %05.2f. THIS GIVES YOU %s DAYS.\n"
                     (+ *time0* *t-period*) *t-period* ))
  (insert-GB (format " THERE %s %s STARBASE%s IN THE GALAXY FOR RESUPPLYING YOUR SHIP.\n\n"
                     (if (eql *base-total* 1) "IS" "ARE")
                     *base-total*
                     (if (eql *base-total* 1) "" "S"))))

(defun acceptp ()
  (insert-GB "ARE YOU READY TO ACCEPT COMMAND? ('N' FOR End)")
  (not (string-equal
        "N"
        (read-string "ARE YOU READY TO ACCEPT COMMAND? ('N' FOR End) "))))

(defun enter-quad ()
  (insert-params "(enter-quad) start\n")

  (let (k)
    ;;(setf (quad-visit (aref *ggg* *qx* *qy*)) t) ; make known Quad
    (setf (quad-visit (aref (aref *ggg* *qx*) *qy*)) t) ; make known Quad

    (when
        (not (or (< *qx* 0) (> *qx* 7) (< *qy* 0) (> *qy* 7)))
      (disp-quad-name (quad-name *qy* *qx* 0))
      (enter-quad1)
      ;;comment out ;;(format "%s\n" "*c-bases*=~a *c-klingons*=~a *c-stars*=~a~%" *c-bases* *c-klingons* *c-stars*)
      (repo-entering-quad-stat)
      )

    ;;(insert-params "(init-sector)\n")
    ;;(insert "(init-sector)\n")

    (init-sector)

    ;;(insert-params "(short-range-sensor)\n")
    ;;(insert "(short-range-sensor)\n")
    (short-range-sensor)
    )

  ;;(insert-params "(enter-quad) end\n")
  ;;(insert "(enter-quad) end\n")
  )

(defun disp-quad-name (qq)
  (insert-params "disp-quad-name\n")
  (insert-params (format "qq: %s\n" qq))

  (cond
   ((eql *time0* *time*)
    (insert-GB (format "\nYOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED\n"))
    (insert-GB (format "IN THE GALACTIC QUADRANT, '%s'.\n" qq)))
   (t 
    (insert-GB (format "\nNOW ENTERING %s QUADRANT . . .\n" qq)))))

(defun repo-entering-quad-stat ()
  (when (/= *c-klingons* 0)
    (insert-GB "  COMBAT AREA      CONDITION RED  \n"))
  (when (<= *shield* 200)
    (insert-GB "      SHIELDS DANGEROUSLY LOW     \n"))
  )

(defun enter-quad1 ()
  (insert-params "(enter-quad1) start\n")

  ;;(let ((g (aref *ggg* *qx* *qy*)))
  (let ((g (aref (aref *ggg* *qx*) *qy*)))
    (setq *c-klingons* (quad-klingon g))
    (setq *c-bases* (quad-base g))
    (setq *c-stars* (quad-star g))
    )

;;    (let ((inhibit-read-only t))
  (insert (format "%s\n" *c-klingons*))
  (insert (format "%s\n" *c-bases*))
  (insert (format "%s\n" *c-stars*))

  (insert-params "(enter-quad1) end\n")
;;      )
  )

;;(defun energy-check()
(cl-defun energy-check ()
  (if (and (> (+ *shield* *energy*) 10)
           (or (> *energy* 10) (zerop (aref *ddd* 7))))
      ;;(return-from energy-check t))
      (cl-return-from energy-check t))

  (insert-GB (format "** FATAL ERROR **\n"))
  (insert-GB (format "YOU'VE JUST STRANDED YOUR SHIP IN SPACE.\n"))
  (insert-GB (format "YOU HAVE INSUFFICIENT MANEUVERING ENERGY,\n"))
  (insert-GB (format "AND SHIELD CONTROL IS PRESENTLY INCAPABLE OF\n"))
  (insert-GB (format "CROSS-CIRCUITING TO ENGINE ROOM!!\n"))
  nil
  ;;   PRINT:GOTO 6220
  )

(defun help-com ()
  (insert-GB (format "ENTER ONE OF THE FOLLOWING:\n"))
  (insert-GB (format "--------------------------\n"))
  (insert-GB (format "  W  (WARP)\n"))
  (insert-GB (format "  S  (FOR SHORT RANGE SENSOR SCAN)\n"))
  (insert-GB (format "  L  (FOR LONG RANGE SENSOR SCAN)\n"))
  (insert-GB (format "  P  (TO FIRE PHASERS)\n"))
  (insert-GB (format " (T) (TO FIRE PHOTON TORPEDOES)\n"))
  (insert-GB (format "  Z  (TO RAISE OR LOWER SHIELDS)\n"))
  (insert-GB (format "  R  (FOR DAMAGE CONTROL REPORTS)\n"))
  (insert-GB (format "  C  (TO CALL ON LIBRARY-COMPUTER)\n"))
  (insert-GB (format "  XXX  (TO RESIGN YOUR COMMAND)\n"))
  (insert-GB (format "  (zzz  break for debug)\n")))

(defun init-sector-reset-*sec* ()
  ;;(insert-params "(init-sector)\n")

  (let (x y k)
    ;;(setq *sec* (make-vector 8 (make-vector 8 nil))) ;; 同じ値を参照してしまい、失敗
    ;;(setf (aref *sec* *ex* *ey*) 'e) ;; 元の Lisp コード の aref は強力

    ;; ローカルの *sec* を初期化
    (setq *sec*
          '[[nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]])
    ))

(mapcar #'list
'(1 2 3 4 5)
)

(print
(mapcar* #'(lambda (x)(append (list (append x nil)) nil))
         '[[nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]]
         )
)

(with-temp-buffer
  (init-sector-reset-*sec*)
  (print (length *sec*))
  ;;(print (aref *sec* 0))
  (mapcar* #'(lambda (x)(append (list (append x nil)) nil))
           *sec*)
  ;;(buffer-string)
  )

(cl-defun init-sector ()
  ;;(insert-params "(init-sector)\n")

  (let (x y k)
    ;;(setq *sec* (make-vector 8 (make-vector 8 nil))) ;; これはダメ
    ;;(setf (aref *sec* *ex* *ey*) 'e)

    (setq *sec*
          '[[nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]])

      (insert-*sec*)

    ;; この実行で、エンタープライズ e が増えていく
    (setf (aref (aref *sec* *ex*) *ey*) 'e)

    (insert-*sec*)
    ;;(insert "\n")

    (dotimes (i 4) (setf (klingon-energy (aref *kkk* i)) 0))

    (dotimes (i *c-klingons*)
      (cl-loop
       (setq x (fnrand))
       (setq y (fnrand))
       ;;(when (not (aref *sec* x y))
       (when (not (aref (aref *sec* x) y))
         (setq k (aref *kkk* i))
         (setf (klingon-x k) x)
         (setf (klingon-y k) y)
         (setf (klingon-energy k) (* *klingon-max-energy* (+ 5 (random 10)) 0.1))
         ;;(setf (aref *sec* x y) 'k)
         (setf (aref (aref *sec* x) y) 'k)
         (cl-return))))

    (dotimes (i *c-bases*)
      (cl-loop
       (setq x (fnrand))
       (setq y (fnrand))
       ;;(when (not (aref *sec* x y))
       (when (not (aref (aref *sec* x) y))
         (setq *bx* x)
         (setq *by* y)
         ;;(setf (aref *sec* x y) 'b)
         (setf (aref (aref *sec* x) y) 'b)
         (cl-return))))

    (dotimes (i *c-stars*)
      (cl-loop
       (setq x (fnrand))
       (setq y (fnrand))
       ;;(when (not (aref *sec* x y))
       (when (not (aref (aref *sec* x) y))
         ;;(setf (aref *sec* x y) 's)
         (setf (aref (aref *sec* x) y) 's)
         (cl-return))))

    ;;(insert "*sec*: \n")
    ;;(insert-*sec*)
    ;;(insert "\n")

    (let ((inhibit-read-only t))
      (insert (format "*ex*: %s\n" *ex*))
      (insert (format "*ey*: %s\n" *ey*))
      )
    )
  )

(cl-defun mloop ()
  (interactive)
  ;;(insert "(mloop) start\n")
  ;;(insert-params "(mloop) start\n")

  (let (klatt)
    (cl-loop
     ;;1990
     (when (or *success* (<= *klingon-total* 0))
       (insert "*success*\n")
       (cl-return-from mloop (success)))

     (when (> *time* (+ *time0* *t-period*))
       (insert "*times*\n")
       (cl-return-from mloop (fail-mission)))

     (when *mission-end*
       (insert "*mission-end*\n")
       (cl-return-from mloop))

     (when *klingon-attack*
       (klingon-attack)
       (setq *klingon-attack* nil))

     (when (not (energy-check))
       (insert "(not (energy-check)\n")
       (fail-mission)
       (cl-return-from mloop nil))

     ;;(goto-char (point-max))
     (insert-GB "\nCOMMAND: ")

     ;(recenter-top-bottom)
     ;(recenter-top-bottom)

     ;;(setq aaa (read-string "aaa: "))
     ;;(setq aaa (string-to-char (read-string "COMMAND: ")))
     (setq aaa (read-string "COMMAND: "))

     (cond
      ((string-equal "w" aaa)
       (if (not (nav)) (cl-return-from mloop nil)))
      ((string-equal "s" aaa)
       (short-range-sensor))
      ((string-equal "l" aaa)
       (long-range-sensor))
      ((string-equal "p" aaa)
       (phaser))
      ;;((t) (torpedo))
      ((string-equal "(t)" aaa)
       (torpedo))
      ((string-equal "z" aaa)
       (shield))
      ((string-equal "r" aaa)
       (damage-report))
      ((string-equal "c" aaa)
       (computer))
      ((string-equal "xxx" aaa)
       (end-of-mission))
      ;;(?X (end-of-mission))
      ((string-equal "zzz" aaa)
       (cl-return-from mloop))
      ;;(?Z (return-from mloop))
      ((string-equal "t" aaa)
       (help-com))
      )

     ;; (cl-case aaa
     ;;   (?w 
     ;;    (if (not (nav)) (return-from mloop nil)))
     ;;   (?s (short-range-sensor))
     ;;   (?l (long-range-sensor))
     ;;   (?p (phaser))
     ;;   ;;((t) (torpedo))
     ;;   (?T (torpedo))
     ;;   (?z (shield))
     ;;   (?r (damage-report))
     ;;   (?c (computer))
     ;;   ;;(xxx (end-of-mission))
     ;;   (?X (end-of-mission))
     ;;   ;;(zzz (break))
     ;;   (?Z (return-from mloop))
     ;;   (?t (help-com))
     ;;   )

     )
    )
  )

(cl-case (read-char)
  (?a (do-a-thing))
  (?b (do-b-thing))
  ((?\r ?\n) (do-ret-thing))
  (t (do-other-thing)))

(cl-defun nav ()
  (let (c1 n w1)
    (when (not (setq c1 (input-course "LT. SULU"))) (cl-return-from nav t))
    ;;(format "%s\n" "c1=~a~%" c1)
    (when (not (setq w1 (nav-factor))) (cl-return-from nav t))
    ;;(format "%s\n" "w1=~a~%" w1)
    (when (not (setq n  (nav-energy w1))) (cl-return-from nav t))
    ;;(format "%s\n" "n=~a~%" n)
    (klingon-attack-warp)
    (repair-by-warp w1)
    (damage-by-warp)
    (when (not (nav4 c1 n w1)) (cl-return-from nav t))
    (warp-time w1)
    ))

(defun warp-time (w1)
  (let ((t8 1))
    (if (< w1 1) (setq t8 (/ (floor (* 10 w1)) 10)))
    (incf *time* t8)
    (cond
     ((> *time* (+ *time0* *t-period*))
      (fail-mission))
     (t t))))

(defun input-course (man)
  (insert-GB (format "COURSE (0-8, -1)\n"))
  ;;(setq c1 (read))
  (setq c1 (string-to-number (read-string "COURSE (0-8, -1)" )))

  (cond
   ((not (numberp c1)) nil)
   ((= c1 -1) nil)
   (t
    (cond
     ((or (< c1 0) (> c1 8))
      (insert-GB (format "   %s: 'INCORRECT COURSE DATA, SIR!'" man))
      t)
     (t
      (if(= c1 8)
          0
        c1))))))

(defun nav-factor ()
  (let* (
         (wdamage (aref *ddd* 1))
         (x (if (< wdamage 0) 0.2 8))
         w1)
    (insert-GB (format "WARP FACTOR (0-%s) ?\n" x))
    ;;(setq w1 (read))
    (setq w1 (string-to-number (read-string "WARP FACTOR ? ")))

    (cond
     ((not (numberp w1)) nil)
     ((= w1 0) nil)
     ((and (< wdamage 0) (> w1 0.2))
      (insert-GB "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2\n")
      nil)
     ((or (< w1 0) (> w1 8))
      (insert-GB (format "   CHIEF ENGINEER SCOTT: 'THE ENGINES WON'T TAKE WARP %s!'\n" w1))
      nil)
     (t w1))
    ))

(defun nav-energy (w1)
  (let (
        (n (floor (+ (* w1 8) 0.5))))
    (cond
     ((< *energy* n)
      (insert-GB (format "ENGINEERING:  'INSUFFICIENT ENERGY AVAILABLE\n"))
      (insert-GB (format "               FOR MANEUVERING AT WARP %s!'\n" w1))
      (cond
       ((or (< *shield* (- n *energy*)) (< (aref *ddd* 7) 0))
        t)
       (t
        (insert-GB (format "\nDEFLECTOR CONTROL ROOM:  %s UNITS OF ENERGY\n" *shield*))
        (insert-GB (format "                          PRESENTLY DEPLOYED TO SHIELDS.\n")))
       )
      t)
     (t n)
     )
    ))

(defun klingon-attack-warp ()
  (dotimes (i 4)
    (cond
     ((/= 0 (klingon-energy (aref *kkk* i)))
      ;;(format "%s\n" "k-att move ~a~%" i)
      (klingon-rand-move i))))
  (klingon-attack))

(defun repair-by-warp (w1)
  (let ((flag nil) x)
    (dotimes (i 9)
      (setq ii (1+ i))
      (cond
       ((<(aref *ddd* ii) 0)
        (setq x (incf (aref *ddd* ii)))
        (cond
         ((>= x 0)
          (setf (aref *ddd* ii) 0)
          (cond
           ((not flag)
            (insert (format "DAMAGE CONTROL REPORT:  "))
            (setq flag t)))
          (insert (format "%s REPAIR COMPLETED.\n" (device-name ii))))))))))

(defun damage-by-warp ()
  (let (damdev)
    (cond
     ((<= (random 10) 2)
      (setq damdev (rnd1-8))
      (incf (aref *ddd* damdev)
            (cond 
             ((<(random 10) 6)
              (insert-GB
               (format "DAMAGE CONTROL REPORT:  %s DAMAGED\n"
                       (device-name damdev)))
              (* -1 (1+ (/(random 500)100))))
             (t 
              (insert-GB
               (format "DAMAGE CONTROL REPORT:  %s STATE OF REPAIR IMPROVED\n"
                       (device-name damdev)))
              (1+ (/(random 300)100)))
             )
            )
      ))))

(defun cal-vec (va c1)
  (let (ci cr)
    (setq ci(floor c1))
    (setq cr (- c1 ci))
    (+ (aref va ci)
       (* (- (aref va (+ ci 1)) (aref va ci))
          cr))))

(cl-defun nav4 (c1 n w1)
  (let
      (
       (x *ex*) (y *ey*)
       (dx (cal-vec *cx* c1))
       (dy (cal-vec *cy* c1))
       (x0 *ex*) (y0 *ey*) (old-qx *qx*) (old-qy *qy*))

    ;;(setf (aref *sec* *ex* *ey*) nil)
    (setf (aref (aref *sec* *ex*) *ey*) nil)

    (dotimes (i n)
      (incf x dx) (incf y dy)
      (setq *ex* (floor (+ x 0.5)))
      (setq *ey* (floor (+ y 0.5)))

      (cond 
       ((or (< *ex* 0)(> *ex* 7)
            (< *ey* 0)(> *ey* 7))
        (cl-return-from nav4
          (cond ((exit-quad n x0 y0 dx dy w1 old-qx old-qy)
                 (enter-quad) t)
                (t nil))))
       (t (insert-GB (format "(%s,%s)" *ex* *ey*))))

      ;;(when (aref *sec* *ex* *ey*)
      (when (aref (aref *sec* *ex*) *ey*)
        (setq *ex* (floor (- x dx)))
        (setq *ey* (floor (- y dy)))
        (insert-GB "\nWARP ENGINES SHUT DOWN AT \n")
        (insert-GB (format "SECTOR %s , %s DUE TO BAD NAVAGATION\n" *ex* *ey*))
        (cl-return)))

    ;;(setf (aref *sec* *ex* *ey*) 'E)
    (insert "(setf (aref (aref *sec* *ex*) *ey*) 'E)\n")

    (setf (aref (aref *sec* *ex*) *ey*) 'E)
    (dec-energy n)
    (insert-GB "\n")
    (short-range-sensor)
    t)
  )

(defun exit-quad (n x y x1 y1 w1 old-qx old-qy)
  (let ((flag nil))
    (incf x (+ (* 8 *qx*) (* n x1)))
    (incf y (+ (* 8 *qy*) (* n y1)))
    (setq *qx* (floor (/ x 8)))
    (setq *qy* (floor (/ y 8)))
    (setq *ex* (floor (- x (* *qx* 8))))
    (setq *ey* (floor (- y (* *qy* 8))))
    (when (< *qx* 0) (setq flag t)(setq *qx* 0)(setq *ex* 0))
    (when (> *qx* 7) (setq flag t)(setq *qx* 7)(setq *ex* 7))
    (when (< *qy* 0) (setq flag t)(setq *qy* 0)(setq *ey* 0))
    (when (> *qy* 7) (setq flag t)(setq *qy* 7)(setq *ey* 7))
    (cond (flag
           (insert-GB (format "LT. UHURA: MESSAGE FROM STARFLEET COMMAND --\n"))
           (insert-GB (format "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER\n"))
           (insert-GB (format "  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'\n"))
           (insert-GB (format "CHIEF ENGINEER SCOTT:  'WARP ENGINES SHUT DOWN\n"))
           (insert-GB (format "  AT SECTOR %s , %s OF QUADRANT %s , %s.'\n"
                              *ex* *ey* *qx* *qy*))
           ;;(setf (aref *sec* *ex* *ey*) 'E)
           (setf (aref (aref *sec* *ex*) *ey*) 'E)
           (when (> *time* (+ *time0* *t-period*))
             (fail-mission)
             nil))
          (t
           (cond
            ((and (eql *qx* old-qx)(eql *qy* old-qy))
             (warp-time w1))
            (t
             (incf *time*)
             (dec-energy n)
             t))))
    )
  )

;;; dec energy
(defun dec-energy (n)
  (when (< (decf *energy* (+ n 10)) 0)
    (insert-GB "SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER.\n")
    (incf *shield* *energy*)
    (setq *energy* 0)
    (when (<= *shield* 0)
      (setq *shield* 0))))

;;; Long sensor
                                        ;4000
(defun long-range-sensor ()
  (let (x y qqq)
    (cond 
     ((< (aref *ddd* 3) 0)
      (insert-GB "LONG RANGE SENSORS ARE INOPERABLE.\n"))
     (t
      (insert-GB (format "LONG RANGE SCAN FOR QUADRANT %s , %s\n" *qx* *qy*))
      (dotimes (i 3)
        (dotimes (j 3)
          (setq x (+ *qx* i -1))
          (setq y (+ *qy* j -1))

          (cond
           ((and (>= x 0) (<= x 7) (>= y 0) (<= y 7))
            ;;(setq qqq (aref *ggg* x y))
            (setq qqq (aref (aref *ggg* x) y))
            (setf (quad-visit qqq) t)
            (insert-GB (format " %s %s %s"
                               (quad-klingon qqq)
                               (quad-base qqq)
                               (quad-star qqq))))
           (t (insert-GB " ***"))
           )
          (insert-GB (format "\n")))
        )
      )))
  )

(defun noememy ()
  (insert-GB "\nSCIENCE OFFICER SPOCK:  'SENSORS SHOW NO ENEMY SHIPS\n")
  (insert-GB "                         IN THIS QUADRANT'\n")
  )

;;; phaser
                                        ;4260
(defun phaser()
  (cond 
   ((< (aref *ddd* 4) 0)
    (insert-GB (format "PHASERS INOPERATIVE.\n")))
   (t
    (cond
     ((<= *c-klingons* 0)
      (noememy))
     (t (phaser1)))
    )
   )
  )

(defun phaser1 ()
  (let (x)
    (cond 
     ((< (aref *ddd* 8) 0)
      (insert-GB "COMPUTER FAILURE HAMPERS ACCURACY.\n"))
     (t
      (insert-GB "PHASERS LOCKED ON TARGET;  \n"))
     )
    (cond
     ((setq x (phaser3))
      (phaser4 x))
     )
    ))

(cl-defun phaser3 ()
  (let (x)
    (cl-loop
     (insert-GB (format "PHASERS LOCKED ON TARGET;  "))
     (insert-GB (format "ENERGY AVAILABLE = %s UNITS\n" *energy*))
     (insert-GB (format "NUMBER OF UNITS TO FIRE ?"))
     ;;(setq x (read))
     (setq x (string-to-number (read-string "NUMBER OF UNITS TO FIRE ? ")))

     (cond
      ((not (numberp x))(cl-return-from phaser3 nil))
      ((<= x 0)
       (cl-return-from phaser3 nil))
      (t 
       (cond
        ((>= (- *energy* x) 0)
         (cl-return-from phaser3 x))))))))

(defun phaser4 (x)
  (let (h ke kx ky k)
    (decf *energy* x)
    (cond
     ((< (aref *ddd* 8) 0)
      (setq x (random x))))
    (setq h1 (floor (/ x *c-klingons*)))
    (setq *klingon-attack* t)
    (dotimes (i 4)
      (setq k (aref *kkk* i))
      (setq kx (klingon-x k))
      (setq ky (klingon-y k))
      (cond
       ((> (klingon-energy k) 0)
        (setq h (floor (* (/ h1 (klingon-distance i)) (+ 2 (/ (random 10) 10)))))
        (cond
         ((<= h (* (klingon-energy k) 0.15))
          (insert-GB (format "\nSENSORS SHOW NO DAMAGE TO ENEMY AT %s , %s.\n"
                             kx ky)))
         (t
          (setq ke (decf (klingon-energy k) h))
          (insert-GB (format "%s UNIT HIT ON KLINGON AT SECTOR %s,%s.\n" h kx ky))
          (cond
           ((<= ke 0)
            (delete-klingon i))
           (t
            (insert-GB (format " (SENSORS SHOW %03.2f UNITS REMAINING)\n" ke)))
           )
          )
         )
        )
       )
      )))

(defun delete-star (x y)
  (setf (aref (aref *sec* x) y) nil)
  ;;(decf (quad-star (aref *ggg* *qx* *qy*)))
  (decf (quad-star (aref (aref *ggg* *qx*) *qy*)))
  )

*qx*                                    ; => 1
*qy*                                    ; => 3

(length (aref *ggg* *qx*))

(append (aref *ggg* *qx*) nil)

*qx*                                    ; => 1
*qy*                                    ; => 3
(aref (aref *ggg* *qx*) *qy*)           ; => [cl-struct-quad 0 7 0 t]
(aref (aref *ggg* 1) 3)                 ; => [cl-struct-quad 0 7 0 t]

(aref (aref *ggg* *qx*) *qy*)             ; => [cl-struct-quad 0 7 0 t]
(quad-star (aref (aref *ggg* *qx*) *qy*)) ; => 7

(decf (quad-star (aref (aref *ggg* *qx*) *qy*))) ; => 6

(defun delete-klingon (i)
  (let (kx ky k)
    (setq k (aref *kkk* i))
    (setq kx (klingon-x k))
    (setq ky (klingon-y k))
    (insert-GB (format "*** KLINGON DESTROYED ***\n"))
    (decf *c-klingons*)
    (decf *klingon-total*)
    ;;(setf (aref *sec* kx ky) nil)
    (setf (aref (aref *sec* kx) ky) nil)
    (setf (klingon-energy k) 0)
    ;;(decf (quad-klingon (aref *ggg* *qx* *qy*)))))
    (decf (quad-klingon (aref (aref *ggg* *qx*) *qy*)))
    )
  )

(defun delete-klingon-xy (x y)
  (let (k)
    (dotimes (i 4)
      (setq k (aref *kkk* i))
      (cond
       ((and
         (/= (klingon-energy k) 0)
         (= (klingon-x k) x)
         (= (klingon-y k) y))
        (delete-klingon i)
        (cl-return))
       (t nil)))))

(defun delete-base (x y)
  (let ()
    (decf *c-bases*)
    (decf *base-total*)
    ;;(setf (aref *sec* x y) nil)
    (setf (aref (aref *sec* x) y) nil)
    ;;(decf (quad-base (aref *ggg* *qx* *qy*)))))
    (decf (quad-base (aref (aref *ggg* *qx*) *qy*)))
    )
  )

(cl-defun torpedo ()
  (let (c1 obj)
    (cond
     ((<= *torpedo* 0)
      (insert-GB "ALL PHOTON TORPEDOES EXPENDED.\n"))
     ((< (aref *ddd* 5) 0)
      (insert-GB "PHOTON TUBES ARE NOT OPERATIONAL.\n"))
     (t
      (insert-GB "PHOTON TORPEDO ")
      (when (not (setq c1 (input-course "ENSIGN CHEKOV")))
        (cl-return-from torpedo t))
                                        ;      (format "%s\n" "c1=~a~%" c1)
      (decf *energy* 2)
      (decf *torpedo* 1)
      (torpedo-fire c1)
      (setq *klingon-attack* t)))
    t))

(defun torpedo-fire (c1)
  (let ((x *ex*) (y *ey*))
    (setq x1 (cal-vec *cx* c1))
    (setq y1 (cal-vec *cy* c1))

    (insert-GB "TORPEDO TRACK:")
    (cl-loop
     (incf x x1)
     (incf y y1)
     (setq x3 (floor (+ x 0.5)))
     (setq y3 (floor (+ y 0.5)))

     (cond
      ((or (< x3 0) (> x3 7) (< y3 0)(> y3 7))
       (insert-GB "\nTORPEDO MISSED.\n")
       (cl-return))

      ;;((eql (setq obj (aref *sec* x3 y3)) 'k)
      ((eql (setq obj (aref (aref *sec* x3) y3)) 'k)
       (insert-GB "\n")
       (delete-klingon-xy x3 y3)
       (when (<= *klingon-total* 0)
         (setq *success* t)
         (throw 'game-end t))
       (cl-return))

      ((eql obj 's)
       (insert-GB (format "\nSTAR AT %s, %s ABSORBED TORPEDO ENERGY.\n" x3 y3))
       (delete-star x3 y3)
       (cl-return))

      ((eql obj 'b)
       (insert-GB "\n*** STARBASE DESTROYED ***\n")
       (delete-base x3 y3)
       (destroy-base)
       (cl-return)))

     (insert-GB (format "(%s,%s)" x3 y3))
     )))

(defun destroy-base ()
  (cond 
   ((or (> *base-total* 0) (> *klingon-total* (- *time* *time0* *t-period*)))
    (insert-GB "STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER")
    (insert-GB "COURT MARTIAL!")
    (setq *docked* nil)
    (setq *klingon-attack* t))
   (t
    (insert-GB "THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND")
    (insert-GB "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!")
    (end-of-mission))))

;;; shield
                                        ;5530
(defun shield ()
  (let (x y)
    (cond 
     ((< (aref *ddd* 7) 0)
      (insert-GB "\nSHIELD CONTROL INOPERABLE.\n"))

     (t
      (insert-GB (format "\nENERGY AVAILABLE =%s.  NUMBER OF UNITS TO SHIELDS ?\n"
                         (+ *energy* *shield*)))
      ;;(setq x (read))
      (setq x (string-to-number (read-string "NUMBER OF UNITS TO SHIELDS ? ")))

      (cond
       ((or (< x 0) (= x *shield*))
        (insert-GB "<SHIELDS UNCHANGED>\n"))
       ((> x (+ *energy* *shield*))
        (insert-GB "SHIELD CONTROL:  'THIS IS NOT THE FEDERATION TREASURY.'\n" )
        (insert-GB "<SHIELDS UNCHANGED>\n"))
       (t

        ;;(insert (format "*energy* :%s\n" *energy*))
        ;;(insert (format "*shield* :%s\n" *shield*))
        ;;(insert (format "x :%s\n" x))

        (incf *energy* (- *shield* x))
        (setq *shield* x)
        (insert-GB "DEFLECTOR CONTROL ROOM:\n")
        (insert-GB (format "  'SHIELDS NOW AT %s UNITS PER YOUR COMMAND.'\n" *shield*))

        ;;(insert (format "*energy* :%s\n" *energy*))
        ;;(insert (format "*shield* :%s\n" *shield*))
        ;;(insert (format "x :%s\n" x))

        )
       )
      )
     )
    )
  )

;;; damage report
                                        ;5690
(defun damage-report ()
  (cond
   ((< (aref *ddd* 6) 0)
    (insert-GB (format "DAMAGE CONTROL REPORT NOT AVAILABLE.\n")))
   (t
    (show-stat-repair)))
  (docked-repair))

(defun show-stat-repair ()
  (insert-GB (format "DEVICE             STATE OF REPAIR\n"))
  (insert-GB (format "------             ---------------\n"))
  (dotimes (i 8)
    (insert-GB (format "%s %03.2f\n"
                       (device-name (1+ i))
                       (* (floor (* 100 (aref *ddd* (1+ i)))) 0.1))
               )))

(defun docked-repair ()
  (let (d3)
    (cond
     (*docked*
      (setq d3 0)
      (dotimes (i 8)
        (when (< (aref *ddd* (1+ i)) 0)
          (incf d3 0.1)))
      (cond
       ((= d3 0) nil)
       (t (need-repair d3)))))))

(defun need-repair (d3)
  (incf d3 *damage-repair-magic-number*)
  (setq d3 (if (>= d3 1)  0.9  d3))
  (insert-GB (format "TECHNICIANS STANDING BY TO EFFECT REPAIRS TO YOUR SHIP;"))
  (insert-GB (format "ESTIMATED TIME TO REPAIR: %03.2f STARDATES."
                     (* .01 (floor (* 100 D3)))))
  (insert-GB (format "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N)"))

  (setq read-string-repair-order (read-string "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N) "))

  (cond
   (
    ;;(eql 'y (read))
    (string-equal "y" read-string-repair-order)
    (repair-all)
    (incf *time* (+ d3 0.1))
    (show-stat-repair)
    )
   (t nil)))

(defun repair-all ()
  (insert "(repair-all) start")
  (dotimes (i 8)
    (cond((<  (aref *ddd* (1+ i)) 0)
          (aset-anv *ddd* (1+ i) 0)))))

;;; klingon attack
                                        ;6000
(cl-defun klingon-attack ()
  ;;(insert "(klingon-attack) start")

  (cond
   ((<= *c-klingons* 0) t)
   (*docked*
    (insert-GB "STARBASE SHIELDS PROTECT THE ENTERPRISE.\n")
    t)
   (t
    (dotimes (i 4)
      (let* ((k (aref *kkk* i))
             (ke (klingon-energy k))
             h)
        (when (> ke 0)
          (setq h (floor (*(/ ke (klingon-distance i))
                           (+ 2 (/ (random 10) 10)))))
          (decf *shield* h)
          (setf (klingon-energy k)(/ ke  (+ 3 (/ (random 10) 10))))
          (insert-GB
           (format "%s UNIT HIT ON ENTERPRISE FROM SECTOR %s , %s .\n"
                   h (klingon-x k) (klingon-y k)))
          (cond
           ((<= *shield* 0)
            (enterprise-destroyed)
            (cl-return-from klingon-attack))
           (t
            (insert-GB (format "      <SHIELDS DOWN TO  UNITS>%s\n" *shield*))
            (when(>= h 20)
              (when (and (<= (random 10) 6) (> (/ h *shield*) 0.02))
                (setq r1 (rnd1-8))
                (decf (aref *ddd* r1) (+ (/ h *shield*) (/ (random 50) 100)))
                (insert-GB
                 (format "DAMAGE CONTROL: '%s DAMAGED BY THE HIT'"
                         (device-name r1)))
                )))
           )))))))

;;; Fail 1 energy==0 or timeout
;;6220
(defun fail-mission ()
  (insert-GB (format "IT IS STARDATE %05.2f.\n" *time*))
  (end-of-mission))

;;; Fail destroyed
;;6240
(defun enterprise-destroyed ()
  (insert-GB "\n\nTHE ENTERPRISE HAS BEEN DESTROYED.\n")
  (insert-GB "THE FEDERATION WILL BE CONQUERED.\n")
  (fail-mission))

;;; end of mission
;;6270
(defun end-of-mission ()
  ;;(insert "(end-of-mission)\n")

  (insert-GB (format "THERE WERE %s KLINGON BATTLE CRUISERS LEFT AT\n" *klingon-total*))
  (insert-GB (format "THE END OF YOUR MISSION.\n"))
  (setq *mission-end* t)

  ;;(insert "(throw 'game-end nil) begin\n")
  (throw 'game-end nil)
  ;;(insert "(throw 'game-end nil) end\n")
  )

(defun more-mission ()
  (cond
   ((/= *base-total* 0)
    (insert-GB (format "\n\nTHE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER\n"))
    (insert-GB (format "FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,\n"))
    (insert-GB (format "LET HIM STEP FORWARD AND ENTER 'AYE'"))
    ;;(eql 'aye (read))

    (setq KEY-AYE (read-string "LET HIM STEP FORWARD AND ENTER 'AYE'"))
    ;;(insert-params KEY-AYE)
    ;;(insert-GB (format "%s\n" KEY-AYE))

    (string-equal "AYE" KEY-AYE)
    ;; (string-equal
    ;;  "AYE"
    ;;  (read-string "LET HIM STEP FORWARD AND ENTER 'AYE'")))
    )
   (t nil)
   )
  )

;;; success
                                        ;6370
(defun success ()
  (let ((x (/ *klingon-org* (- *time* *time0*))))
    (insert-GB (format "CONGRATULATIONS, CAPTAIN!  THE LAST KLINGON BATTLE CRUISER\n"))
    (insert-GB (format "MENACING THE FEDERATION HAS BEEN DESTROYED.\n\n"))
    (insert-GB (format "YOUR EFFICIENCY RATING IS %s"
                       (* x x 1000))))
  )

;;; 
;;6430
(cl-defun dockedp ()
  (let (x y)
    (dotimes (i 3)
      (dotimes (j 3)
        (setq x (+ *ex* i -1))
        (setq y (+ *ey* j -1))
        ;;(when (and (>= x 0)(<= x 7)(>= y 0)(<= y 7)(eql 'b (aref *sec* x y)))
        (when (and (>= x 0)(<= x 7)(>= y 0)(<= y 7)(eql 'b (aref (aref *sec* x) y)))
          (setq *condi* "DOCKED")
          (setq *docked* t)
          (setq *energy* *full-energy*)
          (setq *torpedo* *full-torpedo*)
          (setq *shield* 0)
          (insert-GB (format "SHIELDS DROPPED FOR DOCKING PURPOSES.\n"))
          (cl-return-from dockedp t))))
    (setq *docked* nil)
    nil))

(defun set-condition ()
  (cond ((not (dockedp))
         (cond
          ((> *c-klingons* 0) (setq *condi* "*RED*"))
          ((< *energy* (/ *full-energy* 10)) (setq *condi* "YELLOW"))
          (t  (setq *condi* "GREEN"))))))

;;; short range sensor
;; f1980
(defmacro srs () (short-range-sensor))
(cl-defun short-range-sensor ()
  (interactive)
  (let (fff)
    (set-condition)

    (when (< (aref *ddd* 2) 0)
      (insert-GB "*** SHORT RANGE SENSORS ARE OUT ***\n")
      (cl-return-from short-range-sensor nil))


    ;;
    (insert-GB "\n   +0-1-2-3-4-5-6-7-+")
    (setq fff *disp-info-funcs*)

    (dotimes (i 8)
      (insert-GB (format "\n  %s|" i))

      (dotimes (j 8)
        (insert-GB (format "%s "
                           ;;(case  (aref *sec* i j)
                           (cl-case  (aref (aref *sec* i) j)
                             ;; ((s) "*")
                             (s "*")
                             ;; ((k) "K")
                             (k "K")
                             ;; ((b) "B")
                             (b "B")
                             ;; ((e) "E")
                             (e "E")
                             (t   ".")
                             )
                           ))
        )

      (insert-GB "|")
      ;;      (disp-info i)
      (apply (pop fff) nil)
      )
    (insert-GB "\n")
    )
  )

(atom (aref (aref *sec* 1) 2))
(aref (aref *sec* 0) 0)

(equal 's 's)

(cl-case  (aref (aref *sec* 0) 0)
  ;; ((s) "*")
  (s "*")
  ;; ((k) "K")
  (k "K")
  )

(defconst
  *disp-info-funcs*
  (list
   (lambda()
     (insert-GB (format "        STARDATE           %05.2f" (/ (floor(* *time* 10)) 10))))
   (lambda()
     (insert-GB (format "        CONDITION          %s" *condi*)))
   (lambda()
     (insert-GB (format "        QUADRANT           %s %s" *qx* *qy*)))
   (lambda()
     (insert-GB (format "        SECTOR             %s %s" *ex* *ey*)))
   (lambda()
     (insert-GB (format "        PHOTON TORPEDOES   %s" *torpedo*)))
   (lambda()
     (insert-GB (format "        TOTAL ENERGY       %05.2f" (+ *energy* *shield*))))
   (lambda()
     (insert-GB (format "        SHIELDS            %05.2f" *shield*)))
   (lambda()
     (insert-GB (format "        KLINGONS REMAINING %s" *klingon-total*))))
  )

(defun comp-help ()
  (insert-GB (format "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:\n"))
  (insert-GB (format "-----------------------------------------\n"))
  (insert-GB (format "   G = CUMULATIVE GALTIC RECORD\n"))
  (insert-GB (format "   S = STATUS REPORT\n"))
  (insert-GB (format "   T = PHOTON TORPEDO DATA\n"))
  (insert-GB (format "   B = STARBASE NAV DATA\n"))
  (insert-GB (format "   N = DIRECTION/DISTANCE CALCULATOR\n"))
  (insert-GB (format "   Z = GALAXY 'REGION NAME' MAP\n")))

(cl-defun computer ()
  (let (a)
    (when (<(aref *ddd* 8) 0)
      (insert-GB (format "COMPUTER DISABLED.\n"))
      (cl-return-from computer nil))
    (insert-GB (format "COMPUTER ACTIVE AND AWAITING COMMAND\n"))

    ;;(setq a (read))
    (setq a (read-char "COMPUTER ACTIVE AND AWAITING COMMAND ?"))

    ;;    (when (or (not (numberp a)) (< a 0) (> a 5))
    ;;      (comp-help)
    ;;      (return-from computer nil))

    (cl-case a
      (?g
       (comp-galaxy-rec))
      (?s
       (comp-stat-repo))
      (?T ;;(t)
       (comp-torpedo))
      (?b
       (base-nav))
      (?n
       (comp-calc))
      (?z
       (comp-galaxy-name-map))
      (?t
       (comp-help))
      )
    ))

(defun comp-galaxy-name-map ()
  (insert-GB (format "                        THE GALAXY\n"))
  (insert-GB (format "       0     1     2     3     4     5     6     7\n"))
  (insert-GB (format "    +-----+-----+-----+-----+-----+-----+-----+-----+\n"))
  (dotimes (i 8)
    (insert-GB "\n")
    (dotimes (j 2)
      (insert-GB (format " %s\n"
                         (quad-name (* j 4) i 1)))
      )
    )
  )

(insert-params "\n")
(dotimes (i 8)
  (dotimes (j 2)
    (insert-params (format "i=%s, j=%s (* j 4)=%s" i j (* j 4)))
    )
  )

(defun comp-galaxy-rec ()
  (let (x qqq)
    (insert-GB (format "       COMPUTER RECORD OF GALAXY FOR QUADRANT %s , %s\n"
                       *qx* *qy*))
    (insert-GB (format "       0     1     2     3     4     5     6     7\n"))
    (insert-GB (format "    +-----+-----+-----+-----+-----+-----+-----+-----+"))

    (dotimes (i 8)
      (insert-GB (format "\n  %s |" i))

      (dotimes (j 8)
        (setq qqq (aref (aref *ggg* i) j))

        (cond 
         ((quad-visit qqq)
          (insert-GB (format " %s %s %s "
                             (quad-klingon qqq)
                             (quad-base qqq)
                             (quad-star qqq))))
         (t (insert-GB " ***  "))
         )
        )
      )
    ))

;; galax record
;; status
(defun comp-stat-repo ()
  (insert-GB (format "   STATUS REPORT:\n   -------------\n"))
  (insert-GB (format " %s KLINGON%s LEFT.\n" *klingon-total* (if (> *klingon-total* 1) "S" "")))
  (insert-GB (format " MISSION MUST BE COMPLETED IN %5.2f STARDATES.\n"
                     (* (/(floor (+ *time0* *t-period* (- *time*))) 10) 10)))
  (cond
   ((> *base-total* 0)
    (insert-GB (format t
                       " THE FEDERATION IS MAINTAINING %s STARBASE%s IN THE GALAXY.\n\n"
                       *base-total*  (if (> *base-total* 1) "S" ""))))
   (t
    (insert-GB "YOUR STUPIDITY HAS LEFT YOU ON YOUR OWN IN\n")
    (insert-GB "  THE GALAXY -- YOU HAVE NO STARBASES LEFT!\n\n")
    )
   )
  (damage-report))

;; torpedo cource
(defun comp-torpedo ()
  (cond
   ((<= *c-klingons* 0) (noememy))
   (t
    (insert-GB
     (format "FROM ENTERPRISE TO KLINGON BATTLE CRUSER%s\n"
             (if (> *c-klingons* 1) "S" "")))
    (dotimes (i 4)
      (cond
       ((> (klingon-energy (aref *kkk* i)) 0)
        (comp-torpedo1 i)))))))

(defun comp-torpedo1 (i)
  (let* (
         (k (aref *kkk* i))
         (kx (klingon-x k))
         (ky (klingon-y k)))
    (insert-GB
     (format "KLINGON at (%s , %s): DIRECTION = %3.2f\n"
             kx ky (calc-p2p *ex* *ey* kx ky))
     )
    ))

;; calculator
(defun comp-calc ()
  (let (x0 y0 x1 y1)
    (insert-GB (format "DIRECTION/DISTANCE CALCULATOR: \n"))
    (insert-GB (format "YOU ARE AT QUADRANT %s,%s " *qx* *qy*))
    (insert-GB (format " SECTOR %s,%s.\n" *ex* *ey*))

    (insert-GB "PLEASE ENTER INITIAL COORDINATES X?\n")
    ;;(setq x0 (read))
    (setq x0 (read-string "PLEASE ENTER INITIAL COORDINATES X? "))

    (insert-GB (format "Y?\n"))
    (setq y0 (read-string "Y? "))
    ;;(setq y0 (read))

    (insert-GB (format "FINAL COORDINATES X?\n"))
    (setq x1 (read-string "FINAL COORDINATES X? "))
    ;;(setq x1 (read))

    (insert-GB (format "Y?\n"))
    (setq y1 (read-string "Y? "))
    ;;(setq y1 (read))

    (disp-direct-dist x0 y0 x1 y1)
    ))

(defun disp-direct-dist (x0 y0 x1 y1)
  (insert-GB (format "DIRECTION = %3.2f\n"
                     (calc-p2p x0 y0 x1 y1)))
  (pr-distance (- x0 x1)(- y0 y1)))

;; starbase nav data
(defun base-nav ()
  (cond 
   ((= *c-bases* 0)
    (insert-GB "MR. SPOCK:  'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'\n"))
   (t
    (disp-direct-dist *ex* *ey* *bx* *by*)
    )
   ))

(defun pr-distance (dx dy)
  (format "DISTANCE = %5.3f\n" (distance-p2p dx dy)))

(defun distance-p2p (dx dy)
  (sqrt (+ (* dx dx) (* dy dy))))

(defun calc-p2p (x0 y0 x1 y1)
  (let (dx dy)
    (setq dx (- x1 x0))
    (setq dy (- y1 y0))
    (cond
     ((and (= dx 0)(= dy 0))
      0)
     ((and (< dx 0)(>= dy 0))
      (if (> (abs dx) (abs dy))
          (calc0 0 dx dy)
        (calc1 2 dx dy)))
     ((and (>= dx 0)(>= dy 0))
      (if (< (abs dx)(abs dy))
          (calc1 2 dx dy)
        (calc2 4 dx dy)))
     ((and (>= dx 0)(< dy 0))
      (if (> (abs dx)(abs dy))
          (calc2 4 dx dy)
        (calc3 6 dx dy)))
     ((and (< dx 0)(< dy 0))
      (if (< (abs dx)(abs dy))
          (calc3 6 dx dy)
        (calc0 8 dx dy))))))

(defun calc0 (n dx dy)
  (- n (/ dy dx)))

(defun calc1 (n dx dy)
  (+ n (/ dx dy)))

(defun calc2 (n dx dy)
  (calc0 n dx dy))

(defun calc3 (n dx dy)
  (calc1 n dx dy))

;;; random move
(cl-defun klingon-rand-move (i)
  (let (x y
          newx newy xxx
          (k  (aref *kkk* i)))
    ;;(format "%s\n" "i=~a\n" i)
    (setq x (klingon-x k))
    (setq y (klingon-y k))
    ;;(format "%s\n" "x=~a y=~a\n" x y)

    (cl-loop
     (setq newx (randmove-vec x))
     (setq newy (randmove-vec y))
     ;;(format "%s\n" "newx=~a newy=~a\n" newx newy)
     (setq xxx (aref (aref *sec* newx) newy))
     ;;(format "%s\n" "xxx=~a\n" xxx )
     (when (or (null xxx) (eql 'k xxx))
       (cl-return)))
    (cond
     ((null xxx)
      ;;(format "%s\n" "aset2-axyv x=~a y=~a\n" x y )
      (aset2-axyv *sec* x y nil)
      (aset2-axyv *sec* newx newy 'k)
      (setf (klingon-x k) newx)
      (setf (klingon-y k) newy)
      (insert-GB (format "Klingon at %s, %s moves to %s, %s\n" x y newx newy))
      )
     )
    ))

(defun randmove-vec (x)
  (let (new
        (d (1- (random 3))))
    (setq new (+ x d))
    (if (< new 0) (setq new 0))
    (if (> new 7) (setq new 7))
    new))

;;; device name
(defconst *device-name*
  ["" "WARP ENGINES" "SHORT RANGE SENSORS" "LONG RANGE SENSORS"
   "PHASER CONTROL" "PHOTON TUBES" "DAMAGE CONTROL" "SHIELD CONTROL"
   "LIBRARY-COMPUTER"])

;;f8790
;;(defmacro device-name (x) 
;;  `(aref  *device-name* ,x))
(defun device-name (x) 
  (aref *device-name* x))

;;; quad name
(defconst *quad-name1*
  ;; (make-vector 8
  ;;              '("ANTARES" "RIGEL" "PROCYON" "VEGA"
  ;;                "CANOPUS" "ALTAIR" "SAGITTARIUS" "POLLUX")))
  ["ANTARES" "RIGEL" "PROCYON" "VEGA"
   "CANOPUS" "ALTAIR" "SAGITTARIUS" "POLLUX"])
(defconst *quad-name2*
  ;; (make-vector 8
  ;;              '("SIRIUS" "DENEB" "CAPELLA" "BETELGEUSE"
  ;;                "ALDEBARAN" "REGULUS" "ARCTURUS" "SPICA")))
  ["SIRIUS" "DENEB" "CAPELLA" "BETELGEUSE"
   "ALDEBARAN" "REGULUS" "ARCTURUS" "SPICA"])
(defconst *quad-sub*
  ;; (make-vector 4
  ;;              '(" I" " II" " III" " IV")))
  [" I" " II" " III" " IV"])

;;9030
(defun quad-name (z5 z4 g5)
  (concatenate 'string 
               (quad-name1 z5 z4)
               (quad-name-sub g5 z5)))

(defun quad-name1 (z5 z4)
  (cond ((< z5 4)
         (aref *quad-name1* z4))
        (t (aref *quad-name2* z4))))

(defun quad-name-sub (g5 z5)
  (if (eql g5 1)
      ""
    (aref *quad-sub* (mod z5 4))))
