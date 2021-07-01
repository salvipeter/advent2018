(defvar *data*)

(defun manhattan (p q)
  (+ (abs (- (car p) (car q)))
     (abs (- (cdr p) (cdr q)))))

(defun closest-point (points q)
  (let (best index)
    (loop for p in points and i upfrom 0
          for d = (manhattan p q) do
            (cond ((or (not best) (> best d))
                   (setf best d
                         index (list i)))
                  ((and best (= best d))
                   (pushnew i index))))
    (and (null (cdr index))
         (car index))))

(defun adv06 ()
  (let ((size 400)
        (infinite '())
        (points (make-array (length *data*) :initial-element 0)))
    (dotimes (x size)
      (dotimes (y size)
        (let ((closest (closest-point *data* (cons x y))))
          (when closest
            (incf (aref points closest))
            (when (or (= x 0) (= y 0) (= x (1- size)) (= y (1- size)))
              (pushnew closest infinite))))))
    (loop for i from 0 below (length *data*)
          unless (member i infinite)
            maximize (aref points i))))

(defun distance-sum (points q)
  (let ((result 0))
    (dolist (p points)
      (incf result (manhattan p q)))
    result))

(defun adv06b ()
  (let ((size 400))
    (loop for x from (- size) to size
          sum (loop for y from (- size) to size
                    count (< (distance-sum *data* (cons x y)) 10000)))))

;;; Data

(defparameter *data*
  '((341 . 330) (85 . 214) (162 . 234) (218 . 246) (130 . 67) (340 . 41) (206 . 342) (232 . 295) (45 . 118) (93 . 132) (258 . 355) (187 . 302) (181 . 261) (324 . 246) (150 . 203) (121 . 351) (336 . 195) (44 . 265) (51 . 160) (63 . 133) (58 . 117) (109 . 276) (292 . 241) (81 . 56) (281 . 284) (226 . 104) (98 . 121) (178 . 234) (319 . 332) (279 . 234) (143 . 163) (109 . 333) (80 . 188) (106 . 242) (65 . 59) (253 . 137) (287 . 317) (185 . 50) (193 . 132) (96 . 319) (193 . 169) (100 . 155) (113 . 161) (182 . 82) (157 . 148) (132 . 67) (339 . 296) (243 . 208) (196 . 234) (87 . 335)))
