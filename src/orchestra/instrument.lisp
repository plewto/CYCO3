;;;; CYCO
;;;;
;;;; NOTE: Child node of an Instrument must also be an Instrument.



(constant +instrument-properties+
	  '(:program-map :program-number :program-bank
			 :keynumber-map :dynamic-map :articulation-map
			 :channel :channel-index))

(defclass instrument (cyco-node) nil
  (:documentation
   "An INSTRUMENT is a type of NODE used to represent an external MIDI 
device.  They are essentially proxies for real-world synthesizers.

Instruments may have child instruments which inherit most of the 
parent instruments properties.   A very common usage is to define 
specific percussion instruments as children of a drum machine 
or synthesizer. 

The Node transient property was implemented specifically for use with 
instruments.   Typically instruments are defined in two stages:

1) Permanent instruments defined by the configuration process.
2) Temporary instruments defined as the 'orchestra' of specific projects.

The type-1 instruments should be non-transient while the type-2 instruments
are transient.


The global *ROOT-INSTRUMENT* serves as the common root node for all 
other instruments."))

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

(global *root-instrument*
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
			     (parent *root-instrument*)
			     (transient t)
			     channel
			     program
			     bank
			     keynumber-map
			     dynamic-map
			     articulation-map
			     remarks)
  "Creates a new instance of INSTRUMENT.
name - Symbol
:parent     - Parent instrument, defaults to *ROOT-INSTRUMENT*
:transient  - bool, If this instrument is being created as part of a 
              project's orchestra, transient should be t.
              If the instrument is created by the configuration process
              transient should be nil.  Default t.
:channel    - MIDI channel may be a numeric channel number between 1 and 16
              inclusive or a meta-channel name.   If channel is not specified
              it is inherited from the parent instrument.
:program    - Default program number.  The program-number format is dependent
              on the instruments program-map.
:bank       - Default program bank, format is dependent on program-map.
:remarks    - Optional remarks text.
:keynumber-map    - Sets keynumber-map, if nil inherits from parent.
:dynamic-map      - Sets dynamic-map, if nil inherits from parent.
:articulation-map - Sets articulation-map, if nil inherits from parent."
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
			   (parent *root-instrument*)
			   (transient t)
			   channel
			   program
			   bank
			   keynumber-map
			   dynamic-map
			   articulation-map
			   remarks)
  "Same as make-instrument except binds the instrument to a symbol named name."
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

(instrument null-instrument :parent *root-instrument*
	    :transient nil
	    :channel 1
	    :program 0
	    :keynumber-map (symbolic-keynumber-map nil :instrument-name 'null-instrument)
	    :dynamic-map (basic-dynamic-map :scale 0.0)
	    :articulation-map (constant-articulation-map 'r)
	    :remarks "Null instrument ~~ Does not produce any events.")
(program-map! null-instrument #'(lambda (&rest _)(dismiss _) nil))

(setf *metronome* (make-instrument 'null-metronome
				   :parent *root-instrument*
				   :transient nil
				   :keynumber-map (metronome-keynumber-map)
				   :dynamic-map (metronome-dynamic-map)))


(defun prune-orchestra (&key (force nil)(root *root-instrument*))
  "Removes all transient instruments from the orchestra (tree rooted at root).
While composing a piece it is usual to reload the project repeatedly.   
Projects typically define several instruments as their 'orchestra'.  
If the orchestra is not pruned then each time the project is loaded it 
creates useless duplicate instruments.   Non-transient instruments are
not effect by prune-orchestra unless the :force argument is true."
  (prune root force)
  (if force
      (progn 
	(connect *root-instrument* null-instrument)
	(connect *root-instrument* *metronome*))))

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
  "Connect child instrument to this instrument.
Child nodes of an instrument must also be instruments."
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
	   (funcall (program-map n) 0.0 :program :doc)))))
  
	  
  



