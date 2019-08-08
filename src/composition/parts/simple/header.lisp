;;;; CYCO
;;;; simple-part loader
;;;;

(defpackage :cyco-simple
  (:use :cl)
  (:import-from :cyco
                :+rest+
		:+part-properties+
		:*project*
		:->list
		:->string
                :->symbol
		:absolute-chords-p
		:articulation-map
		:bend->midi-data
		:channel-index
                :clone
		:children
		:chord-inversion
		:chord-template
		:connect
		:constant
		:copy-time-signature
		:cyco-composition-error
		:cyco-warning
		:defines-chord-p
		:dismiss
		:dynamic
		:dynamic-p
		:dynamic-map
                :invert
		:keynumber-map
		:metric-expression
		:metric-expression-p
		:midi-channel-pressure
		:midi-control-change
		:midi-note-on
		:midi-note-off
		:midi-pitch-bend
		:muted-p
		:name
		:norm->midi-data
                :param
		:parent
                :part
		:partition-list
		:phrase-duration
		:project-p
		:property
		:put
		:render-once
		:render-n
                :reset
		:rest-p
		:retrograde
		:section-p
		:set-cyco-prompt
		:sformat
		:sort-midi-events
		:tempo
                :transpose))

(in-package :cyco-simple)

(defun ->cyco-symbol (sym)
  (->symbol (string-upcase (->string sym)) :cyco))


(constant +simple-part-properties+
	  (append +part-properties+
		  '(:shift
		    :shuffle-function
		    :render-once)))

;; eq symbol test ignoring package.
;;
(defun symbol-eq-p (a b)
  (eq (->cyco-symbol a)
      (->cyco-symbol b)))

(load "src/composition/parts/simple/docs")
(load "src/composition/parts/simple/simple-state")
(load "src/composition/parts/simple/simple-part")
(load "src/composition/parts/simple/midi-render")

(export '(make-simple-part +docs+ +macro-docs+ dump) :cyco-simple)
(import '(cyco-simple:make-simple-part) :cyco)

(in-package :cyco)

(defmacro simple-part (name instruments &key
			    section
			    cuefn
			    shuffle
			    shift
			    tempo unit bars beats subbeats
			    render-once
			    transposable
			    reversible
			    chord-model
			    remarks
			    events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-part (make-simple-part ',name ,instruments
				       :section ,section
				       :cuefn ,cuefn
				       :shuffle ,shuffle
				       :shift ,shift
				       :tempo ,tempo
				       :unit ,unit
				       :bars ,bars
				       :beats ,beats
				       :subbeats ,subbeats
				       :render-once ,render-once
				       :transposable ,transposable
				       :reversible ,reversible
				       :chord-model ,chord-model
				       :remarks ,remarks
				       :events ,events)))
       (defparameter ,name new-part)
       new-part)))
				       
(setf (documentation 'simple-part 'function) cyco-simple:+macro-docs+)
