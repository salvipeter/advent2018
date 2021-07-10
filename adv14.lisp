(defparameter *steps* 77201)

(defun adv14 ()
  (let ((recipes (make-array (* *steps* 2) :element-type '(integer 0 9)))
        (n 2)
        (a 0)
        (b 1))
    (setf (aref recipes 0) 3
          (aref recipes 1) 7)
    (dotimes (i (+ *steps* 10))
      (let ((sum (+ (aref recipes a) (aref recipes b))))
        (if (< sum 10)
            (setf (aref recipes n) sum
                  n (1+ n))
            (setf (aref recipes n) (floor sum 10)
                  (aref recipes (1+ n)) (mod sum 10)
                  n (+ n 2)))
        (setf a (mod (+ a (aref recipes a) 1) n)
              b (mod (+ b (aref recipes b) 1) n))))
    (format nil "~{~a~}"
            (loop for i from 0 below 10 collect (aref recipes (+ *steps* i))))))

(defparameter *target* #(0 7 7 2 0 1))

(defun find-target (recipes n target)
  (let ((k (length target)))
    (and (> n k)
         (or (equalp target (subseq recipes (- n k 1) (1- n)))
             (equalp target (subseq recipes (- n k) n))))))

(defun adv14b ()
  (let ((recipes (make-array 100000000 :element-type '(integer 0 9)))
        (n 2))
    (setf (aref recipes 0) 3
          (aref recipes 1) 7)
    (do ((i 0 (1+ i))
         (a 0 (mod (+ a (aref recipes a) 1) n))
         (b 1 (mod (+ b (aref recipes b) 1) n)))
        ((find-target recipes n *target*)
         (let ((k (length *target*)))
           (if (equalp *target* (subseq recipes (- n k) n))
               (- n k)
               (- n k 1))))
      (let ((sum (+ (aref recipes a) (aref recipes b))))
        (if (< sum 10)
            (setf (aref recipes n) sum
                  n (1+ n))
            (setf (aref recipes n) (floor sum 10)
                  (aref recipes (1+ n)) (mod sum 10)
                  n (+ n 2)))))))
