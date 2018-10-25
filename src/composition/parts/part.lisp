;;;; PigIron CYCO composition parts part


(constant +part-properties+
	  (append +time-signature-properties+
		  '(:chord-model
		    :instruments
		    :cue-function
		    :muted
		    :transposable
		    :reversible
		    :group)))
		    
(defclass part (time-signature) nil)

(defmethod part-p ((obj t)) nil)
(defmethod part-p ((obj part)) t)

(defmethod mute ((prt part) &optional state)
  (cond ((eq state :mute)
	 (put prt :muted :mute))
	((eq state :unmute)
	 (put prt :muted nil))
	((eq state :solo)
	 (let ((section (parent prt)))
	   (dolist (p (children prt))
	     (mute p :mute))
	   (dolist (g (property section :groups))
	     (mute g :mute)))
	 (mute prt :unmute))
	(t nil)))

(defmethod unmute ((prt part))
  (mute prt :unmute))

(defmethod solo ((prt part))
  (mute prt :solo))

(defmethod print-tree ((prt part) &optional (depth 0))
  (let ((ptab (scopies (* 4 depth) #\space)))
    (format t "~A~16A mute: ~6A  group: ~A~%"
	    ptab (name prt)
	    (property prt :muted)
	    (property prt :group))
    (dolist (c (children prt))
      (print-tree c (1+ depth)))))

(defmethod muted-p ((prt part))
  (property prt :muted))

(defmethod connect ((parent part)(child cyco-node))
  (call-next-method))

;; Prepare instruments for use with a Part.
;; Argument may be:
;;    1) A pattern of instruments
;;    2) A list of instruments
;;    3) A single instrument object.
;;    Patterns are returned directly
;;    List and single instruments are convertd to Instrument-Layer objects.
;;
(flet ((validate
	(ilist)
	(if (every #'instrument-p ilist)
	    t
	  (progn
	    (cyco-type-error 'init-part-instruments 'instrument ilist)
	    nil))))

  (defun init-part-instruments (instruments)
    (let ((pat (cond ((pattern-p instruments)
		     instruments)
		    ((listp instruments)
		     (instrument-layer :of instruments))
		    (t (init-part-instruments (->list instruments))))))
      (and (validate (elements pat)) pat))))

(defun part-banner (parent-name part-name)
  (banner3 (sformat "Section: ~A  Part: ~A" parent-name part-name)))


(defmethod dump-events ((prt part) &key
			(range (cons 0 1e9))
			(filter #'false)
			(render nil))
  (dump-events (render-once prt) :range range :filter filter :render render))
