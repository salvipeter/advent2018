(defvar *immune-system*)
(defvar *infection*)

(defparameter *boost* 0) ; for part 2

(defstruct group type units hit-points weaknesses immunities attack initiative)

(defun end-score (army1 army2)
  (or (and (null army1)
           (cons (group-type (car army2)) (reduce #'+ (mapcar #'group-units army2))))
      (and (null army2)
           (cons (group-type (car army1)) (reduce #'+ (mapcar #'group-units army1))))))

(defun effective-power (group)
  (* (group-units group)
     (+ (cdr (group-attack group))
        (if (eq (group-type group) 'immune) *boost* 0)))) ; part 2

(defun damage (attacker defender)
  (* (effective-power attacker)
     (cond ((member (car (group-attack attacker)) (group-weaknesses defender)) 2)
           ((member (car (group-attack attacker)) (group-immunities defender)) 0)
           (t 1))))

(defun power-sort (g1 g2)
  (or (> (effective-power g1) (effective-power g2))
      (and (= (effective-power g1) (effective-power g2))
           (> (group-initiative g1) (group-initiative g2)))))

(defun attack-sort (attacker)
  (lambda (g1 g2)
    (let ((d1 (damage attacker g1))
          (d2 (damage attacker g2)))
      (or (> d1 d2)
          (and (= d1 d2)
               (power-sort g1 g2))))))

(defun assign-targets (attackers defenders)
  "Returns (ATTACKER . DEFENDER) pairs."
  (let ((sorted (sort (copy-list attackers) #'power-sort))
        (result '()))
    (dolist (attacker sorted)
      (let ((sorted-defenders (sort (copy-list defenders) (attack-sort attacker))))
        (when (and sorted-defenders
                   (> (damage attacker (car sorted-defenders)) 0))
          (push (cons attacker (car sorted-defenders)) result)
          (setf defenders (cdr sorted-defenders)))))
    result))

(defun attack (pairs)
  "Return whether any units were killed."
  (let (changed)
    (dolist (pair pairs)
      (let ((attacker (car pair))
            (defender (cdr pair)))
        (when (and (> (group-units attacker) 0)
                   (> (floor (damage attacker defender)
                             (group-hit-points defender))
                      0))
          (setf changed t
                (group-units defender)
                (max 0 (- (group-units defender)
                          (floor (damage attacker defender)
                                 (group-hit-points defender))))))))
    changed))

(defun fight (army1 army2)
  (or (end-score army1 army2)
      (let ((targets1 (assign-targets army1 army2))
            (targets2 (assign-targets army2 army1)))
        (if (attack (sort (append targets1 targets2) #'>
                          :key (lambda (pair) (group-initiative (car pair)))))
            (fight (remove-if (lambda (g) (zerop (group-units g))) army1)
                   (remove-if (lambda (g) (zerop (group-units g))) army2))
            '(stalemate)))))

(defun adv24 ()
  (fight (mapcar #'copy-structure *immune-system*)
         (mapcar #'copy-structure *infection*)))

;;; Note that
;;;   (let ((*boost* 44)) (adv24))
;;; gives (STALEMATE)
(defun adv24b ()
  (loop for *boost* upfrom 1
        while (not (eq (car (adv24)) 'immune))
        finally (return (cdr (adv24)))))

;;; Data

(defparameter *immune-system*
  (list (make-group :type 'immune :units 2991 :hit-points 8084 :weaknesses '(fire) :attack '(radiation . 19) :initiative 11)
        (make-group :type 'immune :units 4513 :hit-points 3901 :weaknesses '(slashing) :immunities '(bludgeoning radiation) :attack '(bludgeoning . 7) :initiative 12)
        (make-group :type 'immune :units 5007 :hit-points 9502 :weaknesses '(fire) :immunities '(bludgeoning) :attack '(fire . 16) :initiative 2)
        (make-group :type 'immune :units 2007 :hit-points 5188 :weaknesses '(radiation) :attack '(cold . 23) :initiative 9)
        (make-group :type 'immune :units 1680 :hit-points 1873 :weaknesses '(radiation) :immunities '(bludgeoning) :attack '(bludgeoning . 10) :initiative 10)
        (make-group :type 'immune :units 1344 :hit-points 9093 :weaknesses '(radiation) :immunities '(bludgeoning cold) :attack '(cold . 63) :initiative 16)
        (make-group :type 'immune :units 498 :hit-points 2425 :immunities '(fire bludgeoning cold) :attack '(slashing . 44) :initiative 3)
        (make-group :type 'immune :units 1166 :hit-points 7295 :attack '(bludgeoning . 56) :initiative 8)
        (make-group :type 'immune :units 613 :hit-points 13254 :immunities '(radiation cold fire) :attack '(radiation . 162) :initiative 15)
        (make-group :type 'immune :units 1431 :hit-points 2848 :weaknesses '(radiation) :attack '(cold . 19) :initiative 1)))

(defparameter *infection*
  (list (make-group :type 'infection :units 700 :hit-points 47055 :weaknesses '(fire) :immunities '(slashing) :attack '(fire . 116) :initiative 14)
        (make-group :type 'infection :units 2654 :hit-points 13093 :weaknesses '(radiation) :attack '(radiation . 8) :initiative 19)
        (make-group :type 'infection :units 5513 :hit-points 18026 :weaknesses '(slashing) :immunities '(radiation) :attack '(slashing . 6) :initiative 20)
        (make-group :type 'infection :units 89 :hit-points 48412 :weaknesses '(cold) :attack '(radiation . 815) :initiative 17)
        (make-group :type 'infection :units 2995 :hit-points 51205 :weaknesses '(cold) :attack '(slashing . 28) :initiative 7)
        (make-group :type 'infection :units 495 :hit-points 21912 :attack '(cold . 82) :initiative 13)
        (make-group :type 'infection :units 2911 :hit-points 13547 :attack '(slashing . 7) :initiative 18)
        (make-group :type 'infection :units 1017 :hit-points 28427 :immunities '(fire) :attack '(fire . 52) :initiative 4)
        (make-group :type 'infection :units 2048 :hit-points 29191 :weaknesses '(bludgeoning) :attack '(bludgeoning . 22) :initiative 6)
        (make-group :type 'infection :units 1718 :hit-points 15725 :immunities '(cold) :attack '(slashing . 18) :initiative 5)))
