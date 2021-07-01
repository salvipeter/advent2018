(defparameter *serial-no* 7347)

(defun power-level (x y)
  (let ((rack-id (+ x 10)))
    (- (mod (floor (* (+ (* rack-id y) *serial-no*)
                      rack-id)
                   100)
            10)
       5)))

;;; Memoization to the rescue
(let ((cache (make-array '(300 300 300) :element-type 'fixnum :initial-element 0)))
  (defun power-sum (x y size)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (if (= (aref cache x y (1- size)) 0)
        (setf (aref cache x y (1- size))
              (if (= size 1)
                  (power-level (1+ x) (1+ y))
                  (+ (loop for i from 1 to size
                           sum (power-level (+ x i) (+ y size)))
                     (loop for i from 1 to (1- size)
                           sum (power-level (+ x size) (+ y i)))
                     (power-sum x y (1- size)))))
        (aref cache x y (1- size)))))

;;; Trivial (slow) version
;; (defun power-sum (x y size)
;;   (loop for i from 1 to size
;;         sum (loop for j from 1 to size
;;                   sum (power-level (+ x i) (+ y j)))))

(defun adv11 ()
  (let ((max 0)
        best)
    (dotimes (x 298)
      (dotimes (y 298)
        (let ((level (power-sum x y 3)))
          (when (> level max)
            (setf max level
                  best (list (1+ x) (1+ y)))))))
    best))

;;; ~ 1 min
(defun adv11b ()
  (let ((max 0)
        best)
    (dotimes (size 300)
      (dotimes (x (- 300 size))
        (dotimes (y (- 300 size))
          (let ((level (power-sum x y (1+ size))))
            (when (> level max)
              (setf max level
                    best (list (1+ x) (1+ y) (1+ size)))
              (print best))))))
    best))
