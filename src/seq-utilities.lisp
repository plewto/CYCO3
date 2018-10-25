;;;; CYCO3 src/seq-utilities
;;;;

(defmethod ->list ((obj t)) (list obj))
(defmethod ->list ((lst list)) lst)
(defmethod ->list ((v vector)) (coerce v 'list))
(defmethod ->vector ((obj t)) (vector obj))
(defmethod ->vector ((lst list)) (coerce lst 'vector))
(defmethod ->vector ((v vector)) v)

(defmethod final ((obj t))
  (let ((frmt "FINAL not defined for ~A ~A"))
    (error (format nil frmt obj (type-of obj)))))

(defmethod final ((lst list))(car (reverse lst)))

(defmethod final ((v vector))(final (->list v)))

(defmethod butfinal ((obj t))
  (let ((frmt "BUTFINAL not defined for ~A ~A"))
    (error (format nil frmt obj (type-of obj)))))

(defmethod butfinal ((lst list))
  (reverse (cdr (reverse lst))))

(defmethod butfinal ((v vector))
  (->vector (butfinal (->list v))))

(defmethod cnth ((n integer)(lst list))
  (nth (rem (abs n)(length lst)) lst))

(defmethod cnth ((n integer)(v vector))
  (let ((i (rem (abs n)(length v))))
    (aref v i)))

(defmethod slice ((seq sequence)(start t) &optional end)
  (if start
      (cond ((not end)
	     (subseq seq start))
	    ((plusp end)
	     (subseq seq start end))
	    ((or (minusp end)(zerop end))
	     (setf end (max end (- (length seq))))
	     (subseq seq start (+ (length seq) end)))
	    (t nil))
    (slice seq 0 end)))

(defun flatten (obj)
  "Flatten nested list."
  (do* ((rs (list obj))
	(node rs))
      ((null node)(delete nil rs))
    (cond ((consp (car node))
	   (when (cdar node)(push (cdar node)(cdr node)))
	   (setf (car node)(caar node)))
	  (t (setf node (cdr node))))))

(defun flatten1 (obj)
  "Flatten nested list by 1 level."
  (let ((acc '()))
    (dolist (e obj)
      (cond ((atom e)
	     (setf acc (append acc (list e))))
	    (t (setf acc (append acc e)))))
    acc))

(flet ((+rotate (lst)
		(append (cdr lst)(list (car lst))))
       (-rotate (lst)
		(cons (car (last lst))
		      (butlast lst))))
  (defun rotate (lst &optional (n 1))
    "Rotates list n positions.
If n < 0, rotate left.
If n > 0, rotate right."
    (cond ((plusp n)
	   (dotimes (i n)
	     (setf lst (+rotate lst))))
	  ((minusp n)
	   (dotimes (i (abs n))
	     (setf lst (-rotate lst))))
	  (t nil))
    lst))


(labels ((zip1 (a b)
	       (if a (append (list (car a)(car b))
			     (zip1 (cdr a)(cdr b)))))
	 (zip2 (a b)
	       (if b (append (list (car a)(car b))
			     (zip2 (cdr a)(cdr b))))))
  (defun zip (a b)
    (if (>= (length a)(length b))
	(zip1 a b)
      (zip2 a b))))
		 

(defun ->alist (lst)
  "Converts list to association list.
(->alist '(a 1 b 2 c 3)) --> ((A . 1)(B . 2)(C . 3))"
  (let* ((acc '())
	 (bcc (->vector lst))
	 (i 0)
	 (limit (length bcc)))
    (if (not (evenp (length bcc)))
	(let ((msg "Expected even number of elements for ->alist, encountered ~A"))
	  (error (sformat msg lst)))
      (while (< i limit)
	(push (cons (aref bcc i)(aref bcc (1+ i))) acc)
	(setf i (+ 2 i))))
    (reverse acc)))

(defun alist->hash-table (alst &optional size)
  "Create hash-table from association list."
  (if (alist-p alst)
      (let ((htab (make-hash-table :size (or size (length alst)))))
	(dolist (a alst)
	  (setf (gethash (car a) htab) (cdr a)))
	htab)
    (let ((msg "Argument to ALIST->HASH-TABLE is not an association list: ~A"))
      (error (sformat msg alst)))))

(defmethod palindrome ((lst list) &key (elide nil))
  (let ((rev (reverse lst)))
    (setf rev (cond ((eq elide :last)(butlast lst))
		    ((eq elide :first)(cdr lst))
		    ((eq elide :both)(cdr (butlast lst)))
		    (t rev)))
    (append lst rev)))

(defmethod palindrome ((v vector) &key (elide nil))
  (->vector (palindrome (->list v) :elide elide)))

(defun copies (n &optional (item nil)(clones nil))
  "Returns list of n copies of item.
If clones is true (default is nil) and if item supports cloning, 
each resulting item is a unique object."
  (if (plusp n)
      (cons (if clones (clone item) item)
	    (copies (1- n) item clones))))


(defmethod pick ((v vector))
  (aref v (pick (length v))))

(defmethod pick ((lst list))
  (pick (->vector lst)))

(defmethod permute ((lst list))
  (let ((acc '())
	(pile lst))
    (flet ((rot ()(setf pile (rotate pile (pick (length pile))))))
	  (while pile
	    (rot)
	    (push (car pile) acc)
	    (setf pile (cdr pile))))
    acc))
	    
(defmethod permute ((v vector))
  (->vector (permute (->list v))))

(flet ((-range (start end delta)
	       (let ((acc '())
		     (value start))
		 (while (> value end)
		   (push value acc)
		   (setf value (+ value delta)))
		 (reverse acc)))
       (+range (start end delta)
	       (let ((acc '())
		     (value start))
		 (while (< value end)
		   (push value acc)
		   (setf value (+ value delta)))
		 (reverse acc))) )
  (defun range (start end &key (by 1))
    (if (> start end)
	(-range start end (- (abs by)))
      (+range start end (abs by)))))

(defun fill-list (lst template)
  (cons (or (car lst)(car template))
	(if (or lst template)
	    (fill-list (cdr lst)(cdr template)))))


  
;; Split list into head and tail sections at first position after start
;; that that predicate is true.
;; By default splits list on presence of keywords
;;
;; (split-list '(:ape 1 2 :bat 3 4 :cat 5 6)) --> (:ape 1 2) (:bat 3 4 :cat 5 6)
;; (split-list '(:ape 1 2 3 4))               --> (:ape 1 2 3 4) nil
;; (split-list '(1 2 3 4)                     --> (1 2 3 4) nil
;;
(defun split-list (lst &key (test #'keywordp)(start 1))
  (let ((pos (position nil (nthcdr start lst)
		       :test #'(lambda (a b)
				 (dismiss a)
				 (funcall test b)))))
    (if pos
	(let* ((index (+ pos start))
	       (head (subseq lst 0 index))
	       (tail (subseq lst index)))
	  (values head tail))
      (values lst nil))))

;; Partition list by elements matching test predicate.
;; Default is to split list at keyword boundaries.
;;
;; (partition-list '(:ape 1 2 :bat 3 4 :cat 5 6)) --> ((:ape 1 2)(:bat 3 4)(:cat 5 6))
;; (partition-list '(:ape 1 2)                    --> ((:ape 1 2))
;; (partition-list nil                            --> (nil)
;;
(defun partition-list (lst &key (test #'keywordp))
  (multiple-value-bind (head tail)(split-list lst :test test)
    (cons head
	  (if tail
	      (partition-list tail :test test)
	    nil))))
  
(defun sort-midi-events (events)
  (sort (clone events)
	#'(lambda (a b)
	    (let ((time-a (car a))
		  (time-b (car b)))
	      (if (= time-a time-b)
		  (let* ((pri-a (priority (cdr a)))
			 (pri-b (priority (cdr b))))
		    (> pri-b pri-a))
		(< time-a time-b))))))
	      
	    


