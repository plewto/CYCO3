;;;; CYCO parts part.lisp
;;;;
;;;; Defines the PART class.
;;;;
;;;; CYCO-NODE
;;;;  |
;;;;  +-- TIME-SIGNATURE
;;;;       |
;;;;       +-- PART
;;;;            |
;;;;            +-- BENDER
;;;;            +-- CONTROLLERS
;;;;            +-- MIXER
;;;;            +-- PART
;;;;            +-- PROGRAMS
;;;;            +-- QBALL
;;;;            |    |
;;;;            |    +-- METRONOME (psudo part)
;;;;            |
;;;;            +-- RAW-PART
;;;;            +-- STRUMMER
;;;;            +-- SYSEX
;;;;            +-- TEXT-PART
;;;;            +-- TRANSFORMER


(in-package :cyco-part)


(constant +part-specific-properties+ 
	  '(:chord-model
	    :cue-function
	    :cuelist
	    :group
	    :instruments
	    :muted
	    :render-once
	    :reversible
	    :shift
	    :shuffle-function
	    :transposable))
	  
(constant +part-properties+
	  (append +time-signature-properties+
		  +part-specific-properties+))

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
  (cond ((or (eq state :mute)(eq state :m))
	 (put part :muted :mute))
	((or (eq state :unmute)(eq state :u))
	 (put part :muted nil))
	((or (eq state :solo)(eq state :s))
	 (let ((section (parent part)))
	   (dolist (group (property section :groups))
	     (mute group :mute))
	   (dolist (other-part (children section))
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

(defgeneric copy-part-properties (source destination))

(defmethod copy-part-properties ((source part)(destination part))
  (dolist (p +part-specific-properties+)
    (put destination p (clone (property source p)))))
  
(defmethod render-n ((part part)(n integer) &key (offset 0.0) &allow-other-keys)
  (reset part)
  (let ((midi-events '())
	(period (phrase-duration part))
	(template (render-once part)))
    (dotimes (i (if (property part :render-once) 1 n))
      (let ((time-shift (+ offset (* i period))))
	(dolist (event template)
	  (let ((relative-time (car event))
		(message (cdr event)))
	    (push (cons (+ time-shift relative-time)(clone message))
		  midi-events)))))
    (sort-midi-events midi-events)))
    

(defmethod validate-render ((prt part))
  (reset prt)
  (render-once prt))
  

(defmethod duck ((cuelist list)(prt part) &key (invert nil) timesig (use-subbeats t))
  (duck cuelist (property prt :cuelist) :invert invert :timesig timesig :use-subbeats use-subbeats))


(defmethod cuelist ((prt part) &key (form :list) timesig (use-subbeats t))
  (cuelist (property prt :cuelist)
	   :form form
	   :timesig (or timesig prt)
	   :use-subbeats use-subbeats))


(defmethod pprint-cuelist ((prt part) &key header form timesig use-subbeats)
  (declare (ignore timesig use-subbeats))
  (pprint-cuelist (property prt :cuelist) :form form :header header))


(defmethod keylist ((prt part) &key (blur 0.0))
  (keylist (render-once prt) :blur blur))


(defmethod reset ((p part))
  (reset (property p :instruments))
  p)
