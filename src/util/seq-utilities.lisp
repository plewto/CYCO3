;;;; CYCO util seq-utilities.lisp
;;;; 
;;;; Utility functions on sequences.
;;;;

(in-package :cyco)

(defmethod ->list ((obj t)) (list obj))
(defmethod ->list ((lst list)) lst)
(defmethod ->list ((v vector)) (coerce v 'list))
(defmethod ->vector ((obj t)) (vector obj))
(defmethod ->vector ((lst list)) (coerce lst 'vector))
(defmethod ->vector ((v vector)) v)

(defmethod final ((obj t))
  (cyco-type-error 'final '? obj
		   (sformat "FINALE not defined for ~A" (type-of obj))))

(defmethod final ((lst list))(car (reverse lst)))

(defmethod final ((v vector))(final (->list v)))

(defmethod butfinal ((obj t))
  (cyco-type-error 'butfinal '? obj
		   (sformat "BUTFINAL not defined for ~A" (type-of obj))))

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
	  (cyco-error (sformat msg lst)))
      (while (< i limit)
	(push (cons (aref bcc i)(aref bcc (1+ i))) acc)
	(setf i (+ 2 i))))
    (reverse acc)))


(defun alist->hash-table (alst &optional size)
  "Create hash-table from association list."
  (let ((htab (make-hash-table :size (or size (length alst)))))
    (dolist (a alst)
      (setf (gethash (car a) htab) (cdr a)))
    htab))


(defun elide (list method &optional (count 1))
  "Removes count values from either end of list and returns.
method may be one of
   nil    - return list without modification
   :start - removes first count items.
   :end   - removes final count items.
   :both  - removes count items from both ends."
  (cond ((or (null method)(eq method :NONE))
	 list)
	((eq method :start)
	 (nthcdr count list))
	((eq method :end)
	 (reverse (nthcdr count (reverse list))))
	((eq method :both)
	 (nthcdr count (reverse (nthcdr count (reverse list)))))
	(t list)))

(defmethod palindrome ((lst list) &key (elide nil))
  (let ((rev (reverse lst)))
    (append lst (elide rev elide))))

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
    "Creates list of numbers between star (inclusive) and end (exclusive)
:by argument sets increment size."
    (if (> start end)
	(-range start end (- (abs by)))
      (+range start end (abs by)))))


(labels ((+irange (start end steps)
	       (let* ((diff (float (- end start)))
		      (increment (/ diff (1- steps)))
		      (value start)
		      (acc '()))
		 (while (< value end)
		   (push (round value) acc)
		   (setf value (+ value increment)))
		 (while (< (length acc) steps)
		   (push end acc))
		 (reverse acc)))

       (-irange (start end steps)
		(reverse (+irange end start steps))))

  (defun irange (start end steps)
    "Generates list of integers between start and end

start - Integer, the starting value.
end   - Integer, the ending value.
steps - Integer, length of result. 0 <= steps.

Always returns monotonic list of length steps such that the car is 
start and the final element is end.   The resulting step sizes are 
not guaranteed to be consistent and the list may contain duplicate values."
    (cond ((zerop steps)
	   '())
	  ((= steps 1)
	   (list start))
	  ((< start end)
	   (+irange start end steps))
	  ((< end start)
	   (-irange start end steps))
	  (t (copies steps start)))))

(defun fill-list (source-list template)
    "Creates new list by merging source-list and template list.
The resulting list contains the non-null elements of source-list.
All nil and missing elements of source-list are replaced by corresponding 
values from template list."
  (if (or source-list template)
      (cons (or (car source-list)(car template))
	    (fill-list (cdr source-list)(cdr template)))))

  

(defun split-list (lst &key (test #'keywordp)(start 1))
  "Split list into head and tail sections at first position after start
for which predicate is true.
By default splits list on presence of keywords

(split-list '(:ape 1 2 :bat 3 4 :cat 5 6)) --> (:ape 1 2) (:bat 3 4 :cat 5 6)
(split-list '(:ape 1 2 3 4))               --> (:ape 1 2 3 4) nil
(split-list '(1 2 3 4)                     --> (1 2 3 4) nil"
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

(defun partition-list (lst &key (test #'keywordp))
  "Partition list by elements matching test predicate.
Default is to split list at keyword boundaries.

(partition-list '(:ape 1 2 :bat 3 4 :cat 5 6)) --> ((:ape 1 2)(:bat 3 4)(:cat 5 6))
(partition-list '(:ape 1 2)                    --> ((:ape 1 2))
(partition-list nil                            --> (nil)"
  (multiple-value-bind (head tail)(split-list lst :test test)
    (cons head
	  (if tail
	      (partition-list tail :test test)
	    nil))))
  
(defun sort-midi-events (events)
  "Sorts MIDI event list first by event-time and then by message priority."
  (sort (clone events)
	#'(lambda (a b)
	    (let ((time-a (car a))
		  (time-b (car b)))
	      (if (= time-a time-b)
		  (let* ((pri-a (priority (cdr a)))
			 (pri-b (priority (cdr b))))
		    (> pri-b pri-a))
		(< time-a time-b))))))

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

(defmethod permute ((s string))
    (let ((acc "")
	  (indexes (permute (range 0 (length s)))))
      (dolist (i indexes)
	(setf acc (str+ acc (subseq s i (1+ i)))))
      acc))
