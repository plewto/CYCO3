;;;; CYCO orchestra instrument.lisp
;;;;
;;;; NOTE: Child node of an Instrument must also be an Instrument.
;;;;

(in-package :cyco)

(constant +instrument-properties+
	  '(:program-map 
	    :program-number 
	    :program-bank
	    :keynumber-map 
	    :dynamic-map 
	    :articulation-map
	    :channel))

(defclass instrument (cyco-node)
  ((channel-index
    :type t
    :accessor instrument-channel-index
    :initform nil))
  (:documentation
   "An INSTRUMENT is a type of NODE used to represent an external MIDI 
device.  They are essentially proxies for real-world synthesizers.

Instruments may have child instruments which inherit most of the 
parent instruments properties.   A very common usage is to define 
specific percussion instruments as children of a drum machine 
or synthesizer. 

The Node transient property was implemented specifically for use with 
instruments.   Typically instruments are defined in two stages:

  1) Permanent instruments defined by configuration/plugin files.
  2) Temporary instruments defined as the 'orchestra' of specific projects.

The type-1 instruments should be non-transient while the type-2 instruments
are transient.

The constant +ROOT-INSTRUMENT+ serves as the common root node for all 
other instruments."))

(defmethod instrument-p ((instrument instrument)) t)

(defmethod set-program-map ((instrument instrument)(pmap function))
  (put instrument :program-map pmap))

(defmethod set-program-map ((instrument instrument)(pmap null))
  (set-basic-program-map instrument))

(defmethod set-program-number ((instrument instrument)(pnumber integer))
  (put instrument :program-number pnumber))

(defmethod set-program-number ((instrument instrument)(pnumber symbol))
  (put instrument :program-number pnumber))

(defmethod set-program-bank ((instrument instrument)(bank integer))
  (put instrument :program-bank bank))

(defmethod set-program-bank ((instrument instrument)(bank symbol))
  (put instrument :program-bank bank))

(defmethod program-map ((instrument instrument))
  (property instrument :program-map))

(defmethod program-number ((instrument instrument))
  (property instrument :program-number))

(defmethod program-bank ((instrument instrument))
  (property instrument :program-bank))

(defmethod program-change-events ((instrument instrument)(time number) &key bank program)
  (let ((pmap (program-map instrument)))
    (funcall pmap (float time) :bank bank :program program)))

(defmethod set-keynumber-map ((instrument instrument)(mapfn function))
  (put instrument :keynumber-map mapfn))

(defmethod keynumber-map ((instrument instrument))
  (property instrument :keynumber-map))

(defmethod set-dynamic-map ((instrument instrument)(mapfn function))
  (put instrument :dynamic-map mapfn))

(defmethod dynamic-map ((instrument instrument))
  (property instrument :dynamic-map))

(defmethod set-articulation-map ((instrument instrument)(mapfn function))
  (put instrument :articulation-map mapfn))

(defmethod articulation-map ((instrument instrument))
  (property instrument :articulation-map))

(labels ((set-channel-index
	  (instrument channel)
	  (setf (instrument-channel-index instrument)
		(if (meta-channel-assignment-p channel)
		    (1- (meta-channel channel :resolve t :default 1))
		  (progn
		    (cyco-value-error
		     'instrument.set-channel
		     (sformat "Undefined channel ~A" channel))
		    0)))))

  (defmethod set-channel ((instrument instrument)(channel t))
    (setf (gethash :channel (property-table instrument)) channel)
    (set-channel-index instrument channel)
    channel)

  (defmethod put ((instrument instrument)(key symbol)(value t))
    (if (eq key :channel)
	(set-channel instrument value)
      (call-next-method))
    value))

(defmethod channel ((instrument instrument) &optional resolve)
  (meta-channel (property instrument :channel) :resolve resolve))

(defmethod channel-index ((instrument instrument))
  (or (instrument-channel-index instrument)
      (1- (channel instrument :resolve))))

(constant +root-instrument+
	(let ((root (make-instance 'instrument
				   :name '+root-instrument+
				   :properties +instrument-properties+
				   :transient nil)))
	  (set-basic-program-map root)
	  (set-program-number root 0)
	  (set-program-bank root 0)
	  (set-keynumber-map root +default-keynumber-map+)
	  (set-dynamic-map root +default-dynamic-map+)
	  (set-articulation-map root +default-articulation-map+)
	  (set-channel root 1)
	  root))

(let ((docstring 
 "Creates a new instance of INSTRUMENT.
name        - Symbol
:parent     - Parent instrument, defaults to +ROOT-INSTRUMENT+
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
:articulation-map - Sets articulation-map, if nil inherits from parent."))
  
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
    docstring
    (let ((instrument (make-instance 'instrument
				     :name name
				     :properties +instrument-properties+
				     :transient transient
				     :remarks (->string (or remarks "")))))
      (if channel (set-channel instrument channel))
      (if program (set-program-number instrument program))
      (if bank (set-program-bank instrument bank))
      (if keynumber-map (set-keynumber-map instrument keynumber-map))
      (if dynamic-map (set-dynamic-map instrument dynamic-map))
      (if articulation-map (set-articulation-map instrument articulation-map))
      (set-basic-program-map instrument)
      (connect parent instrument)
      instrument)))

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
  "Same as make-instrument except binds the new instrument to name."
  `(let ((instrument (make-instrument ',name
				:parent ,parent
				:transient ,transient
				:channel ,channel
				:program ,program
				:bank ,bank
				:keynumber-map ,keynumber-map
				:dynamic-map ,dynamic-map
				:articulation-map ,articulation-map
				:remarks ,remarks)))
     (param ,name instrument)
     instrument))

(instrument null-instrument :parent +root-instrument+
	    :transient nil
	    :channel 1
	    :program 0
	    :keynumber-map #'(lambda (&rest _)(declare (ignore _)) +REST+)
	    :dynamic-map (basic-dynamic-map :scale 0.0)
	    :articulation-map (constant-articulation-map 'r)
	    :remarks "Null instrument ~~ Does not produce any events.")
(set-program-map null-instrument #'(lambda (&rest _)(declare (ignore _)) nil))

(setf *metronome* (make-instrument '*metronome*
				   :parent +root-instrument+
				   :transient nil
				   :channel 16
				   :keynumber-map (metronome-keynumber-map)
				   :articulation-map (metronome-articulation-map)
				   :dynamic-map (metronome-dynamic-map)))


(let ((docstring 
 "Removes all transient instruments from the tree rooted at +root-instrument+.
While composing a piece it is usual to reload the project repeatedly.   
Projects typically define several instruments as their 'orchestra'.  
If the orchestra is not pruned then each time the project is loaded it 
creates useless duplicate instruments.   Non-transient instruments are
not effect by prune-orchestra unless the :force argument is true."))
  
  (defun prune-orchestra (&key (force nil)(root +root-instrument+))
    docstring
    (prune root force)
    (if force
	(progn 
	  (connect +root-instrument+ null-instrument)
	  (connect +root-instrument+ *metronome*)))))

(defmethod note-events ((instrument instrument)
			(time number)
			(keynumber integer)
			(duration number)
			(dynamic float)
			&key (time-scale 1.0))
  (let ((actual-key-number (funcall (keynumber-map instrument) keynumber))
	(actual-duration (funcall (articulation-map instrument) duration :time-scale time-scale))
	(amplitude (funcall (dynamic-map instrument) dynamic)))
    (if (or (rest-p actual-key-number)(rest-p actual-duration)(rest-p amplitude))
	nil
      (let* ((start time)
	     (end (+ start actual-duration))
	     (velocity (dynamic->velocity amplitude))
	     (channel-index (channel-index instrument)))
	(list (cons start (midi-note-on channel-index actual-key-number velocity))
	      (cons end (midi-note-off channel-index actual-key-number 64)))))))


(defmethod clone ((mother instrument) &key &allow-other-keys)
  mother)

(defmethod connect ((parent-instrument instrument)(child-instrument cyco-node))
  "Connects child instrument to this instrument.
Child nodes of an instrument must also be instruments."
  (if (not (instrument-p child-instrument))
      (cyco-type-error
       'instrument.connect 'instrument child-instrument
       (sformat "Attempt to connect non-instrument ~A to instrument ~A"
		(name child-instrument)(name parent-instrument)))
    (call-next-method)))
  
(defmethod print-tree ((instrument instrument) &optional (depth 0))
  (format t "[~2D] ~A" (channel instrument :resolve) (spaces (* 4 depth)))
  (format t "~A~%" (name instrument))
  (dolist (c (children instrument))
    (print-tree c (1+ depth))))

(defmethod key-gamut ((instrument instrument))
  (sort (funcall (keynumber-map instrument) :gamut) #'<))
		      
(defmethod reset ((instrument instrument))
  (funcall (keynumber-map instrument) :reset))
