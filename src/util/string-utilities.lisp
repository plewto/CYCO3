;;;; CYCO util string-utilities.lisp
;;;;

(in-package :cyco)

(defmethod clone ((mother string) &key &allow-other-keys)
  mother)

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

(defun format-binary (n &key (bits 16) group)
  "Format binary value
n      - The number 
:bits  - Number of bits, default 16
:group - Insert space every group bits, defaults to bits.
Returns string."
  (let ((acc "")
	(brk (or group bits))
	(probe 1))
    (dotimes (bit bits)
      (setf acc (str+ (if (zerop (logand probe n)) "0" "1") acc))
      (setf brk (1- brk))
      (setf probe (ash probe 1))
      (if (zerop brk)
	  (setf acc (str+ " " acc)
		brk (or group bits))))
    (string-left-trim '(#\space) acc)))

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

(defmethod pick ((s string))
  (char s (pick (length s))))

(defun center-string (text width &optional (shift 0))
  "Returns new string of length width with argument text
centered.
str - String
width - positive integer
shift - optional number of spaces to shift text right."
  (if (plusp shift)
      (setf text (str+ (scopies shift #\space) text)))
  (let* ((lens (length text))
	 (diff (- width lens))
	 (pad (scopies (/ diff 2) #\space)))
    (if (minusp diff)
	text
      (str+ pad text pad))))

(defun string-pad-left (str width &optional (c #\space))
  (let ((diff (- width (length str))))
    (if (plusp diff)
	(str+ (scopies diff c) str)
      str)))

(defun string-pad-right (str width &optional (c #\space))
  (let ((diff (- width (length str))))
    (if (plusp diff)
	(str+ str (scopies diff c))
      str)))


(flet ((+rot (str)
		(str+ (subseq str 1)(char str 0)))
       (-rot (str)
	     (let ((tail (char str (1- (length str))))
		   (head (subseq str 0 (1- (length str)))))
	       (str+ tail head))))

      (defmethod rotate ((str string) &optional (n 1))
	(cond ((plusp n)
	       (dotimes (i n)
		 (setf str (+rot str))))
	      ((minusp n)
	       (dotimes (i (abs n))
		 (setf str (-rot str))))
	      (t nil))
	str))
	      
(defun string-compress (s &optional (bag '(#\space #\tab #\newline)))
  "Removes all characters in string which are elements of bag.
By default removes all white space."
  (remove-if #'(lambda (c)(member c bag :test #'char=)) s))

(defun string-white-trim (s &key (bag '(#\space #\tab #\newline)) (end :both))
  "Trims string of white space from either end.
:bag - List of characters to remove, defaults to white space.
:end - end of string to trim. May be :left :right or :both, defaults to :both."
  (if (or (null end)(eq end :left)(eq end :both))
      (setf s (string-left-trim bag s)))
  (if (or (null end)(eq end :right)(eq end :both))
      (string-right-trim bag s)
    s))
