(defvar *data*)
(defparameter *ip* 1)

;;; As in Day 19, except for RUN

(defparameter *instructions* '())

(defmacro instr (name expr)
  (let ((real-name (intern (format nil "INSTR-~a" name))))
    `(progn
       (defun ,real-name (regs a b c)
         (declare (ignorable b))
         (let ((result (copy-list regs)))
           (flet ((r (i) (elt regs i)))
             (declare (ignorable #'r))
             (setf (elt result c) ,expr))
           result))
       (push (cons ',name #',real-name) *instructions*))))

(instr addr (+ (r a) (r b)))
(instr addi (+ (r a) b))
(instr mulr (* (r a) (r b)))
(instr muli (* (r a) b))
(instr banr (logand (r a) (r b)))
(instr bani (logand (r a) b))
(instr borr (logior (r a) (r b)))
(instr bori (logior (r a) b))
(instr setr (r a))
(instr seti a)
(instr gtir (if (> a (r b)) 1 0))
(instr gtri (if (> (r a) b) 1 0))
(instr gtrr (if (> (r a) (r b)) 1 0))
(instr eqir (if (= a (r b)) 1 0))
(instr eqri (if (= (r a) b) 1 0))
(instr eqrr (if (= (r a) (r b)) 1 0))

;;; Runs until END-INSTR
(defun run (program regs end-instr)
  (let ((n (length program)))
    (labels ((rec (regs ip count)
               (cond ((= ip end-instr)
                      regs)
                     ((or (< ip 0) (>= ip n))
                      (error "halted before reaching line ~a" end-instr))
                     (t (let* ((current (elt program ip))
                               (f (cdr (assoc (car current) *instructions*))))
                          (setf (elt regs *ip*) ip)
                          (let ((next (apply f regs (cdr current))))
                            (rec next (1+ (elt next *ip*)) (1+ count))))))))
      (rec regs 0 0))))

(defun adv21 ()
  (elt (run *data* (list 0 0 0 0 0 0) 28) 5))

;;; There are #x1000000 possible values for r0,
;;; so the last one before cycling is the solution
(defun adv21b ()
  (let ((seen (make-array #x1000000 :element-type 'boolean :initial-element nil)))
    (labels ((f (r3 r5 last)
               (let ((next (logand (* (logand (+ r5 (logand r3 #xff))
                                              #xffffff)
                                      #x1016b)
                                   #xffffff)))
                 (cond ((>= r3 #x100) (f (ash r3 -8) next last))
                       ((aref seen next) last)
                       (t (setf (aref seen next) t)
                          (f (logior next #x10000) #x7f493 next))))))
      (f #x10000 #x7f493 nil))))

;;; Data
(defparameter *data*
  '((seti 123 0 5)
    (bani 5 456 5)
    (eqri 5 72 5)
    (addr 5 1 1)
    (seti 0 0 1)
    (seti 0 4 5)
    (bori 5 65536 3)
    (seti 521363 7 5)
    (bani 3 255 4)
    (addr 5 4 5)
    (bani 5 16777215 5)
    (muli 5 65899 5)
    (bani 5 16777215 5)
    (gtir 256 3 4)
    (addr 4 1 1)
    (addi 1 1 1)
    (seti 27 1 1)
    (seti 0 2 4)
    (addi 4 1 2)
    (muli 2 256 2)
    (gtrr 2 3 2)
    (addr 2 1 1)
    (addi 1 1 1)
    (seti 25 2 1)
    (addi 4 1 4)
    (seti 17 3 1)
    (setr 4 2 3)
    (seti 7 1 1)
    (eqrr 5 0 4)
    (addr 4 1 1)
    (seti 5 8 1)))
