(defvar *data*)
(defparameter *size* 32)

(defun scan (x y)
  (ecase (elt (elt *data* y) x)
    (#\# 'wall)
    (#\. 'cavern)
    (#\G 'goblin)
    (#\E 'elf)))

(defun get-type (npc) (caar npc))
(defun get-id (npc) (cadar npc))
(defun get-hits (npc) (caddar npc))
(defun get-pos (npc) (cdr npc))

(defun npcs ()
  (let ((result '())
        (counter 0))
    (dotimes (j *size*)
      (dotimes (i *size*)
        (case (scan i j)
          (goblin (push `((goblin ,(incf counter) 200) . (,i . ,j)) result))
          (elf (push `((elf ,(incf counter) 200) . (,i . ,j)) result)))))
    (nreverse result)))

(defun sort-npcs (list)
  (sort list (lambda (x y)
               (or (< (cddr x) (cddr y))
                   (and (= (cddr x) (cddr y))
                        (< (cadr x) (cadr y)))))))

(defun identify-targets (npcs type)
  (remove-if (lambda (npc) (eq (get-type npc) type)) npcs))

(defun empty (npcs pos)
  (and (not (eq (scan (car pos) (cdr pos)) 'wall))
       (notany (lambda (npc) (equal (get-pos npc) pos)) npcs)))

(defun open-squares (npcs pos)
  (loop with (x0 . y0) = pos
        for (dx . dy) in '((0 . -1) (-1 . 0) (1 . 0) (0 . 1))
        for x = (+ x0 dx) and y = (+ y0 dy)
        when (and (>= x 0) (>= y 0) (< x *size*) (< y *size*)
                  (empty npcs (cons x y)))
          collect (cons x y)))

(defun compute-distances (npcs pos)
  (let ((dists (make-array (list *size* *size*) :element-type 'integer
                                                :initial-element (* *size* *size*))))
    (setf (aref dists (cdr pos) (car pos)) 0)
    (loop with changed = t
          while changed do
            (setf changed nil)
            (dotimes (y0 *size*)
              (dotimes (x0 *size*)
              (when (empty npcs (cons x0 y0))
                (let ((best (aref dists y0 x0)))
                  (dolist (d '((0 . -1) (-1 . 0) (1 . 0) (0 . 1)))
                    (let ((x (+ x0 (car d)))
                          (y (+ y0 (cdr d))))
                      (when (and (>= x 0) (>= y 0) (< x *size*) (< y *size*)
                                 (< (1+ (aref dists y x)) best))
                        (setf changed t
                              best (1+ (aref dists y x))
                              (aref dists y0 x0) best)))))))))
    dists))

(defun find-closest (npcs ranges pos)
  "Finds the first step towards the closest range."
  (let ((steps (open-squares npcs pos)))
    (when steps
      (flet ((first-step (from)
               (let* ((dists (compute-distances npcs from))
                      (d (mapcar (lambda (p) (aref dists (cdr p) (car p))) steps))
                      (min-d (reduce #'min d))
                      (best (position min-d d)))
                 (unless (= min-d (* *size* *size*))
                   (elt steps best)))))
        (let ((dists (compute-distances npcs pos))
              (best (car ranges)))
          (dolist (r (cdr ranges))
            (when (< (aref dists (cdr r) (car r))
                     (aref dists (cdr best) (car best)))
              (setf best r)))
          (first-step best))))))

(defun distance (p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))))

(defparameter *elf-power* 3)            ; for the second part

(defun attack (npcs in-range)
  (let* ((hits (mapcar #'get-hits in-range))
         (min-hits (position (reduce #'min hits) hits))
         (enemy (elt in-range min-hits))
         (index (position enemy npcs :test #'equal)))
    (append (subseq npcs 0 index)
            (let ((pow (if (eq (get-type enemy) 'elf) 3 *elf-power*)))
              (if (<= (get-hits enemy) pow)
                  nil
                  (list (cons (list (get-type enemy) (get-id enemy) (- (get-hits enemy) pow))
                              (get-pos enemy)))))
            (subseq npcs (1+ index)))))

(defun move (npcs)
  (let* ((self (car npcs))
         (targets (identify-targets npcs (get-type self)))
         (in-range (remove-if-not (lambda (x)
                                    (= (distance (get-pos x) (get-pos self)) 1))
                                  targets)))
    (if in-range
        (attack npcs in-range)
        (let ((ranges (mapcan (lambda (x) (open-squares npcs (get-pos x)))
                              targets)))
          (if ranges
              (let ((new-pos (find-closest npcs ranges (get-pos self))))
                (if new-pos
                    (let* ((next-npcs (cons (cons (car self) new-pos) (cdr npcs)))
                           (in-range (remove-if-not (lambda (x)
                                                      (= (distance (get-pos x) new-pos) 1))
                                                    targets)))
                      (if in-range
                          (attack next-npcs in-range)
                          next-npcs))
                    npcs))
              npcs)))))

(defun put-to-front (id npcs)
  (let ((i (position id npcs :key #'get-id)))
    (when i
      (cons (elt npcs i)
           (append (subseq npcs 0 i)
                   (subseq npcs (1+ i)))))))

(defun one-round (npcs)
  (let ((ids (mapcar #'get-id (sort-npcs npcs))))
    (dolist (id ids)
      (let ((next (put-to-front id (sort-npcs npcs))))
        (when next
          (setf npcs (move next))))))
  npcs)

(defun only-one-type (npcs)
  (let ((types (mapcar #'get-type npcs)))
    (not (and (member 'elf types)
              (member 'goblin types)))))

(defun print-state (npcs)
  (dotimes (j *size*)
    (dotimes (i *size*)
      (if (eq (scan i j) 'wall)
          (format t "#")
          (let ((npc (find (cons i j) npcs :key #'get-pos :test #'equal)))
            (if npc
                (format t (if (eq (get-type npc) 'goblin) "G" "E"))
                (format t ".")))))
    (terpri))
  (terpri))

(defun adv15 ()
  (do ((i 0 (1+ i))
       (npcs (npcs) (one-round npcs)))
      ((only-one-type npcs)
       (* (1- i) (reduce #'+ (mapcar #'get-hits npcs))))
    #+nil(print-state npcs)))

(defun less-elves-or-no-goblins (elves npcs)
  (or (< (count 'elf npcs :key #'get-type) elves)
      (not (find 'goblin npcs :key #'get-type))))

(defun adv15b ()
  (let ((elves (count 'elf (npcs) :key #'get-type))
        (low 4)
        (high 200)
        best)
    (loop for *elf-power* = (floor (+ low high) 2)
          for x = (do ((i 0 (1+ i))
                       (npcs (npcs) (one-round npcs)))
                      ((less-elves-or-no-goblins elves npcs)
                       (if (< (count 'elf npcs :key #'get-type) elves)
                           0
                           (* (1- i) (reduce #'+ (mapcar #'get-hits npcs))))))
          while (> high (1+ low))
          do (format t "Trying ~a...~%" *elf-power*)
             (if (= x 0)
                 (setf low *elf-power*)
                 (setf high *elf-power*
                       best x))
          finally (return best))))

;;; Data

(defparameter *data*
  #("################################"
    "##############.#################"
    "##########G##....###############"
    "#########.....G.################"
    "#########...........############"
    "#########...........############"
    "##########.....G...#############"
    "###########.........############"
    "########.#.#..#..G....##########"
    "#######..........G......########"
    "##..GG..................###.####"
    "##G..........................###"
    "####G.G.....G.#####...E.#.G..###"
    "#....##......#######........####"
    "#.GG.#####.G#########.......####"
    "###..####...#########..E...#####"
    "#...####....#########........###"
    "#.G.###.....#########....E....##"
    "#..####...G.#########E.....E..##"
    "#..###G......#######E.........##"
    "#..##.........#####..........###"
    "#......................#..E....#"
    "##...G........G.......#...E...##"
    "##............#..........#..####"
    "###.....#...#.##..#......#######"
    "#####.###...#######...#..#######"
    "#########...E######....#########"
    "###########...######.###########"
    "############..#####..###########"
    "#############.E..##.############"
    "################.#..############"
    "################################"))
