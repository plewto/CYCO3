;;;; CYCO
;;;;

(constant +part-properties+
	  (append +time-signature-properties+
		  '(:chord-model
		    :instruments
		    :cue-function
		    :muted
		    :transposable
		    :reversible
		    :group)))
		    
(defclass part (time-signature) nil
  (:documentation
   "Parts form the lowest element of a composition.  They combine a set of 
instruments together with instructions for what the instruments are to
play.  As such parts form the bulk of a compositons definition.  

The parent of a Part is always a Section and a Part may have child nodes
called 'sub-parts'.   Parts inherit time-signature and chord-model
properties from their parent Section but may override these. 

The Part class is the base class for several specific Part types and is not
used directly."))

  
(defmethod part-p ((object part)) t)

(defmethod mute ((part part) &optional state)
  (cond ((eq state :mute)
	 (put part :muted :mute))
	((eq state :unmute)
	 (put part :muted nil))
	((eq state :solo)
	 (let ((section (parent part)))
	   (dolist (group (property section :groups))
	     (mute group :mute))
	   (dolist (other-part (children part))
	     (mute other-part :mute))
	   (put part :muted nil)))
	(t nil)))

(defmethod unmute ((part part))
  (mute part :unmute))

(defmethod solo ((part part))
  (mute part :solo))

(defmethod print-tree ((part part) &optional (depth 0))
  (let ((tab (scopies (* 4 depth) #\space)))
    (format t "~A~16A mute: ~6A  group: ~A~%"
	    tab (name part)
	    (property part :muted)
	    (property part :group))
    (dolist (sub-part (children part))
      (print-tree sub-part (1+ depth)))))

(defmethod muted-p ((part part))
  (property part :muted))

(defmethod connect ((parent part)(child cyco-node))
  (call-next-method))

;; Prepare instruments for use with a Part.
;; Argument may be:
;;    1) A pattern of instruments
;;    2) A list of instruments
;;    3) A single instrument object.
;;    Patterns are returned directly
;;    List and single instruments are converted to Instrument-Layer objects.
;;
(flet ((validate (instrument-list)
		 (if (every #'instrument-p instrument-list)
		     t
		   (progn
		     (cyco-type-error 'init-part-instruments 'instrument instrument-list)
		     nil))))

  (defun init-part-instruments (instruments)
    (let ((instrument-pattern (cond ((pattern-p instruments)
		      instruments)
		     ((listp instruments)
		      (instrument-layer :of instruments))
		     (t (init-part-instruments (->list instruments))))))
      (and (validate (elements instrument-pattern)) instrument-pattern))))

(defun part-banner (parent-name part-name)
  (banner3 (sformat "Section: ~A  Part: ~A" parent-name part-name)))


(defmethod dump-events ((part part) &key (range (cons 0 1e9))(filter #'false)(render nil))
  (dump-events (render-once part) :range range :filter filter :render render))


