;;;; CYCO3 src/string-utilities
;;;;

(defmethod clone ((str string) &key new-name new-parent)
  (dismiss new-name new-parent)
  str)

(defmethod final ((s string))
  (if (plusp (length s))
      (subseq (reverse s) 0 1)
    ""))

(defmethod butfinal ((s string))
  (if (plusp (length s))
      (subseq s 0 (1- (length s)))
    ""))

(defmethod cnth ((n integer)(s string))
  (char s (rem n (length s))))

(defun str+ (&rest args)
  "Concatenate all arguments to string."
  (apply #'concatenate (cons 'string (->string args))))

(defun spaces (&optional (n 1))
  "Returns string of n spaces."
  (let ((frmt (format nil "~~~DA" n)))
    (format nil frmt #\space)))

(defun scopies (n &optional (char #\-))
  "Returns string of n characters."
  (let ((scc ""))
    (dotimes (i (truncate n))
      (setf scc (str+ scc char)))
    scc))

(defun parse-word (str &optional (delim #\space))
  "Parse first word from string.
Returns two values: 
     1) The delineated word
     2) The remaining string.
     The second value is nil when argument string contains a single word."
  (let* ((d (->string delim))
	 (s (string-left-trim d str))
	 (pos (position d s :test #'string=))
	 (head (subseq s 0 pos))
	 (tail (if pos (subseq s pos) nil)))
    (values head tail)))

(defun split-string (str &optional (delim #\space))
  "Splits string into list of words."
  (let ((acc '())
	(more t))
    (while more
      (multiple-value-bind (head tail)(parse-word str delim)
	(push head acc)
	(setf str tail)
	(setf more tail)))
    (reverse acc)))

(defun starts-with-p (s target)
  "Returns true if initial characters of s are string= to target."
  (let ((tlen (length target)))
    (and (>= (length s) tlen)
	 (string= (subseq s 0 tlen) target))))

(defun sformat (frmt &rest args)
  "Formats string.
Convenience function, same as calling (format nil frmt args...)"
  (apply #'format (append (list nil frmt) args)))

(defmethod palindrome ((s string) &key (elide nil))
  (let ((r (reverse s)))
    (setf r (cond ((eq elide :last)
		   (subseq r 1))
		  ((eq elide :first)
		   (subseq r 0 (1- (length r))))
		  ((eq elide :both)
		   (subseq r 1 (1- (length r))))
		  (t r)))
    (concatenate 'string s r)))

;; string-replace curtsy of: stack-overflow
;; https://stackoverflow.com/questions/4366668/str-replace-in-lisp
;;
(defun string-replace (search replace string &optional count)
  (loop for start = (search search (or result string)
                            :start2 (if start (1+ start) 0))
        while (and start
                   (or (null count) (> count 0)))
        for result = (concatenate 'string
                                  (subseq (or result string) 0 start)
                                  replace
                                  (subseq (or result string)
                                          (+ start (length search))))
        do (when count (decf count))
        finally (return-from string-replace (or result string))))

(defmethod pick ((s string))
  (char s (pick (length s))))

;; inefficient
;;
(defmethod permute ((s string))
  (let ((lst (permute (->list s)))
	(acc ""))
    (dolist (c lst)
      (setf acc (str+ acc (->string c))))
    acc))
    
(defun center-string (str width &optional (shift 0))
  (let* ((lens (length str))
	 (diff (- width lens)))
    (if (minusp diff)
	str
      (str+ (scopies (+ (/ diff 2) shift) #\space) str))))

