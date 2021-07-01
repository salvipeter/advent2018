(defparameter *players* 479)
(defparameter *last* 71035)

(defun adv09 ()
  (let ((marbles (make-array (1+ *last*))) ; vector of (left-neighbor . right-neighbor) pairs
        (players (make-array *players* :initial-element 0))
        (current 0))
    (setf (aref marbles 0) (cons 0 0))
    (flet ((left (k) (car (aref marbles k)))
           (right (k) (cdr (aref marbles k))))
      (dotimes (i *last*)
        (let ((next (1+ i)))
          (if (zerop (mod next 23))
              (let ((to-remove (left (left (left (left (left (left (left current)))))))))
                (setf (cdr (aref marbles (left to-remove))) (right to-remove)
                      (car (aref marbles (right to-remove))) (left to-remove)
                      current (right to-remove))
                (incf (aref players (mod next *players*))
                      (+ next to-remove)))
              (setf (aref marbles next) (cons (right current) (right (right current)))
                    (car (aref marbles (right (right current)))) next
                    (cdr (aref marbles (right current))) next
                    current next)))))
    (reduce #'max players)))

(defun adv09b ()
  (let ((*last* (* 100 *last*)))
    (adv09)))
