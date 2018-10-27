;;;; CYCO3 src/metrics
;;;;
;;;; Metric values specify relative times. The basic metric values are:
;;;;
;;;;    W  = 4    whole note
;;;;    H  = 2    half note
;;;;    Q  = 1    quarter note
;;;;    E  = 1/2  eighth note 
;;;;    S  = 1/4  sixteenth note
;;;;    T  = 1/8  thirtysecond note
;;;;    X  = 1/16 sixtyfourth note
;;;;    Z  = 1/32 128th note.
;;;;    R  = -1   rest
;;;;
;;;; Each basic unit may be modified by appending dots '.' or 't's to the left.
;;;; Each dot modifies value by 3/2
;;;;
;;;;    Q.  -> dotted quarter note
;;;;    Q.. -> double dotted quarter note.
;;;;
;;;; Each 't' modifies value by 2/3 for triplet.
;;;;
;;;;    QT  -> quarter note triplet.
;;;;    TT  -> thirty second note triplet.
;;;;
;;;; It is possible to combine . and t, however they cancel.
;;;;
;;;;    QT.  -> legal but pointless.
;;;;
;;;; It is also possible to modify a rest
;;;;
;;;;    RT   -> legal but pointless.
;;;;

(constant +METRIC-UNITS+
	  (let ((acc (make-hash-table :size 8)))
	    (setf (gethash 'W acc) (float 4))
	    (setf (gethash 'H acc) (float 2))
	    (setf (gethash 'Q acc) (float 1))
	    (setf (gethash 'E acc) (float 1/2))
	    (setf (gethash 'S acc) (float 1/4))
	    (setf (gethash 'T acc) (float 1/18))
	    (setf (gethash 'X acc) (float 1/16))
	    (setf (gethash 'Z acc) (float 1/32))
	    (setf (gethash 'R acc) (float -1))
	    acc))

(defmethod metric-p ((obj t)) nil)

(defmethod metric-p ((n number))
  (bool (float (max -1 n))))

(defmethod metric-p ((s symbol))
  (let* ((name (string-upcase (->string s)))
	 (base (subseq name 0 1))
	 (modifiers (if (> (length name) 1)(subseq name 1) ""))
	 (base-value (gethash (intern base) +metric-units+)))
    (and base-value
	 (every #'(lambda (q)(or (char= q #\.)(char= q #\T))) modifiers))))
    
(defmethod metric ((obj t))
  (cyco-type-error 'metric '(number symbol list) obj))

(defmethod metric ((n number))
  (float (if (minusp n) -1 n)))

(defmethod metric ((s symbol))
  (let* ((name (string-upcase (->string s)))
	 (base (intern (subseq name 0 1)))
	 (modifiers (if (> (length name) 1)(subseq name 1) ""))
	 (value (or (gethash base +metric-units+)
		    (return-from metric (progn (cyco-value-error 'metric s) +rest+)))))
    (dotimes (i (length modifiers))
      (let ((c (char modifiers i)))
	(cond ((char= c #\.)(setf value (* 3/2 value)))
	      ((char= c #\T)(setf value (* 2/3 value)))
	      (t (return-from metric (progn (cyco-value-error 'metric s) +rest+))))))
    (if (minusp value) -1.0 value)))
	  
(defmethod metric ((lst list))
  (mapcar #'metric lst))

;;;; ---------------------------------------------------------------------- 
;;;;			    Metric Expressions
;;;;
;;;; Metric Expressions allow basic metric units to be combined for more
;;;; flexibility.
;;;;                  [n*]m1[-+]m2...[-+]mi
;;;;
;;;; where each mi is a basic metric value as above or a float.
;;;; The optional scaling factor n* is applied after the summation.
;;;;
;;;; Examples:
;;;;    Q+S       -> add quarter and sixteenth notes
;;;;    Q+S-x     -> subtract sixtyfourth note from above.
;;;;    2.3*Q+S   -> add Q and S, then scale result by 2.3
;;;;    Q+R       -> add rest to quarter note, legal but pointless.
;;;;
;;;; Metric expressions may not contain spaces.
;;;;
;;;;    Q+S       -> legal
;;;;    Q + S     -> illegal
;;;;

(flet ((parse-scale 
	(str)
	;; Split initial scale factor, if any, from string
	;; s*exp   --> (s exp)    where s is positive float
	;; exp     --> (1.0 exp)
	;; a*r*exp --> error, return nil
	(let* ((tokens (split-string str #\*))
	       (count (length tokens)))
	  (cond ((= count 1)
		 (values 1.0 (car tokens)))
		((= count 2)
		 (values (read-from-string (car tokens))
			 (second tokens)))
		(t nil))))

       (tokenize
	(str)
	(let ((tokens '()))
	  (dolist (a (split-string str #\+))
	    (if (find #\- a :test #'char=)
		(let ((stokens (split-string a #\-)))
		  (push (cons 1 (car stokens)) tokens)
		  (dolist (b (cdr stokens))
		    (push (cons -1 b) tokens)))
	      (push (cons 1 a) tokens)))
	  (reverse tokens)))
	
       (is-number-p
	(str)
	(every #'(lambda (q)(or (digit-char-p q)
				(char= q #\.))) str)))

  (defmethod metric-expression-p
    ((expression symbol))
    (multiple-value-bind (scale exp)(parse-scale (->string expression))
      (if (numberp scale)
	  (let ((acc 0)
		(tokens (tokenize (->string exp))))
	    (dolist (token tokens)
	      (let ((s (car token))
		    (v (->symbol (cdr token))))
		(if (metric-p v)
		    (setf acc (+ acc (* s (metric v))))
		  (return-from metric-expression-p nil))))
	    (* scale acc))
	nil))))


(defmethod metric-expression-p ((n number))
  (if (minusp n) +rest+ n))

;; Returns value of expression
;; If expression is invalid, generate cyco-metric-expression-error
;; and return nil.
(defmethod metric-expression ((exp symbol))
  (let ((value (metric-expression-p exp)))
    (or value
	(cyco-composition-error
	 'metric-expression
	 (sformat "Invalid metric expression: ~A" exp)))))

(defmethod metric-expression ((n number))
  (metric-expression-p n))

(defmethod metric-expression ((lst list))
  (mapcar #'metric-expression lst))

