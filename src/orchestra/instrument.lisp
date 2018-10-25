;;;; CYCO3 src/orchestra/instrument
;;;;
;;;; NOTE: Child node of an Insrtument must also be an Instrument.

(defgeneric instrument-p (obj))
(defgeneric program-number! (inst pnumber))
(defgeneric program-bank! (inst pbank))


;; Returns list of events
;;   ((time-1 . event-1)
;;    (time-2 . event-2)
;;     .................)

(defgeneric program-change-events (inst time &key bank program))


(defgeneric keynumber-map! (inst mapfn))
(defgeneric keynumber-map (inst))
(defgeneric dynamic-map! (inst mapfn))
(defgeneric dynamic-map (inst))
(defgeneric articulation-map! (inst mapfn))
(defgeneric articulation-map (inst))
(defgeneric channel! (inst channel))

;; Low level function
;; key, duration, dynamic arguments shoukld be numeric
;; Returns list of events
;;   ((time-1 . note-on)
;;    (time-2 . note-off))
;;
(defgeneric note-events (inst time keynum duration dynamic &key time-scale))


(constant +instrument-properties+
	  '(:program-map :program-number :program-bank
			 :keynumber-map :dynamic-map :articulation-map
			 :channel :channel-index))

(defclass instrument (cyco-node) nil)

(defmethod instrument-p ((obj t)) nil)

(defmethod instrument-p ((inst instrument)) t)

(defmethod program-map! ((inst instrument)(pmap function))
  (put inst :program-map pmap))

(defmethod program-map! ((inst instrument)(pmap null))
  (set-basic-program-map inst))

(defmethod program-number! ((inst instrument)(pnumber integer))
  (put inst :program-number pnumber))

(defmethod program-number! ((inst instrument)(pnumber symbol))
  (put inst :program-number pnumber))

(defmethod program-bank! ((inst instrument)(bank integer))
  (put inst :program-bank bank))

(defmethod program-bank! ((inst instrument)(bank symbol))
  (put inst :program-bank bank))

(defmethod program-map ((inst instrument))
  (property inst :program-map))

(defmethod program-number ((inst instrument))
  (property inst :program-number))

(defmethod program-bank ((inst instrument))
  (property inst :program-bank))

(defmethod program-change-events ((inst instrument)(time number) &key bank program)
  (let ((pmap (program-map inst)))
    (funcall pmap (float time) :bank bank :program program)))

(defmethod keynumber-map! ((inst instrument)(mapfn function))
  (put inst :keynumber-map mapfn))

(defmethod keynumber-map ((inst instrument))
  (property inst :keynumber-map))

(defmethod dynamic-map! ((inst instrument)(mapfn function))
  (put inst :dynamic-map mapfn))

(defmethod dynamic-map ((inst instrument))
  (property inst :dynamic-map))

(defmethod articulation-map! ((inst instrument)(mapfn function))
  (put inst :articulation-map mapfn))

(defmethod articulation-map ((inst instrument))
  (property inst :articulation-map))

(defmethod channel! ((inst instrument)(channel t))
  (put inst :channel channel)
  (if (not (meta-channel-assignment-p channel))
      (progn 
	(cyco-value-error 'instrument.channel! channel)
	(put inst :channel-index 0))
    (put inst :channel-index (1- (meta-channel channel)))))
      
(defmethod channel ((inst instrument) &optional resolve)
  (meta-channel (property inst :channel) resolve))

(defmethod channel-index ((inst instrument))
  (property inst :channel-index))

(constant +root-instrument+
	  (let ((root (make-instance 'instrument
				     :name 'root-instrument
				     :properties +instrument-properties+
				     :transient nil)))
	    (set-basic-program-map root)
	    (program-number! root 0)
	    (program-bank! root 0)
	    (keynumber-map! root +default-keynumber-map+)
	    (dynamic-map! root +default-dynamic-map+)
	    (articulation-map! root +default-articulation-map+)
	    (channel! root 1)
	    root))
	    
		
(defun make-instrument (name &key
			     (parent +root-instrument+)
			     (transient t)
			     channel
			     program
			     bank
			     keynumber-map
			     dynamic-map
			     articulation-map
			     remarks)
  (let ((inst (make-instance 'instrument
			     :name name
			     :properties +instrument-properties+
			     :transient transient
			     :remarks (->string (or remarks "")))))
    (if channel (channel! inst channel))
    (if program (program-number! inst program))
    (if bank (program-bank! inst bank))
    (if keynumber-map (keynumber-map! inst keynumber-map))
    (if dynamic-map (dynamic-map! inst dynamic-map))
    (if articulation-map (articulation-map! inst articulation-map))
    (set-basic-program-map inst)
    (connect parent inst)
    inst))

(defmacro instrument (name &key
			   (parent +root-instrument+)
			   (transient t)
			   channel
			   program
			   bank
			   keynumber-map
			   dynamic-map
			   articulation-map
			   remarks)
  `(let ((inst (make-instrument ',name
				:parent ,parent
				:transient ,transient
				:channel ,channel
				:program ,program
				:bank ,bank
				:keynumber-map ,keynumber-map
				:dynamic-map ,dynamic-map
				:articulation-map ,articulation-map
				:remarks ,remarks)))
     (param ,name inst)
     inst))

(instrument null-instrument :parent +root-instrument+
	    :transient nil
	    :channel 1
	    :program 0
	    :keynumber-map (symbolic-keynumber-map nil :instrument-name 'null-instrument)
	    :dynamic-map (basic-dynamic-map :scale 0.0)
	    :articulation-map (constant-articulation-map 'r)
	    :remarks "Null instrument ~~ Does not produce any events.")
(program-map! null-instrument #'(lambda (&rest _)(dismiss _) nil))

(setf *metronome* (make-instrument 'null-metronome
				   :parent +root-instrument+
				   :transient nil
				   :keynumber-map (metronome-keynumber-map)
				   :dynamic-map (metronome-dynamic-map)))


(defun prune-orchestra (&key (force nil)(root +root-instrument+))
  (prune root force)
  (if force
      (progn 
	(connect +root-instrument+ null-instrument)
	(connect +root-instrument+ *metronome*))))

(defmethod note-events ((inst instrument)
			(time number)
			(keynumber integer)
			(duration number)
			(dynamic float)
			&key (time-scale 1.0))
  (let ((kn (funcall (keynumber-map inst) keynumber))
	(dr (funcall (articulation-map inst) duration :time-scale time-scale))
	(dy (funcall (dynamic-map inst) dynamic)))
    (if (or (rest-p kn)(rest-p dr)(rest-p dy))
	nil
      (let* ((start time)
	     (end (+ start duration))
	     (vel (dynamic->velocity dy))
	     (ci (channel-index inst)))
	(list (cons start (midi-note-on ci kn vel))
	      (cons end (midi-note-off ci kn 64)))))))
	
(defmethod clone ((src instrument) &key new-name new-parent)
  (dismiss new-name new-parent)
  src)

(defmethod connect ((src instrument)(child cyco-node))
  (if (not (instrument-p child))
      (cyco-type-error
       'instrument.connect 'instrument child
       (sformat "Attempt to connect non-instrument ~A to instrument ~A"
		(name child)(name src)))
    (call-next-method)))

(defmethod ? ((n instrument))
  (format t "~A~%" (type-of n))
  (format t "  name       : ~A~%" (name n))
  (format t "  remarks    : ~A~%" (remarks n))
  (format t "  parent     : ")
  (if (root-p n)
      (format t "<root>~%")
    (format t "~A~%" (name (parent n))))
  (format t "  children   : ")
  (if (zerop (length (children n)))
      (format t "None~%")
    (progn
      (format t "~%")
      (dolist (c (children n))
	(format t "      ~12A ~A~%" (type-of c)(name c)))))
  (format t "  properties :~%")
  (dolist (k '(:channel :channel-index :program-number :program-bank))
    (format t "      [~24A] --> ~A~%" k (property n k)))

  ;; Interactive menu
  (format t "Options: ~%")
  (format t "   A   - Articulation map~%")
  (format t "   D   - Dynamic map~%")
  (format t "   K   - Key map~%")
  (format t "   P   - Program map~%")
  (format t "   Q   - Quit~%")
  (let ((option (string-upcase (->string (read-char)))))
    (cond ((string= option "Q")
	   (format t "~%"))
	  ((string= option "A")
	   (funcall (articulation-map n) :doc))
	  ((string= option "D")
	   (funcall (dynamic-map n) :doc))
	  ((string= option "K")
	   (funcall (keynumber-map n) :doc))
	  ((string= option "P")
	   (funcall (program-map n) 0.0 :program :doc))
	  )))
  
	  
  

