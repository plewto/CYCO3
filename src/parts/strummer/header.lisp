;;;; CYCO cyco-strummer header.lisp
;;;;

(defpackage :cyco-strummer
  (:use :cl)
  (:import-from :cyco
                :+rest+
		:+part-properties+
		:*project*
		:->list
		:->string
                :->symbol
		:absolute-chords-p
		:approximate
		:articulation-map
		:bend->midi-data
		:channel-index
                :clone
		:children
		:chord-inversion
		:chord-template
		:connect
		:constant
		:copies
		:copy-time-signature
		:cycle
		:cyco-composition-error
		:cyco-warning
		:defines-chord-p
		:dismiss
		:dynamic
		:dynamic-p
		:dynamic-map
                :invert
		:keynumber-map
		:limit
		:line
		:metric-expression
		:metric-expression-p
		:midi-channel-pressure
		:midi-control-change
		:midi-note-on
		:midi-note-off
		:midi-pitch-bend
		:muted-p
		:name
		:next-1
		:norm->midi-data
                :param
		:parent
                :part
		:partition-list
		:permute
		:phrase-duration
		:program-map
		:project-p
		:property
		:put
		:range
		:remarks
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
                :transpose
		:value))

(in-package :cyco-strummer)

(constant +strum-directions+ '("DOWN" "UP" "DICE" "RANDOM"))

(defun ->cyco-symbol (sym)
  (->symbol (string-upcase (->string sym)) :cyco))

(load "src/parts/strummer/docs")
(load "src/parts/strummer/state")
(load "src/parts/strummer/part")
(load "src/parts/strummer/midi-render")


(export '(make-strummer +macro-docs+ ) :cyco-strummer)

(import '(cyco-strummer:make-strummer  ) :cyco)
(in-package :cyco)


(defmacro strummer (name instrument &key
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
     (let ((new-strummer (make-strummer ',name ,instrument
					:section ,section
					:cuefn ,cuefn
					:shuffle ,shuffle
					:shift  ,shift 
					:tempo  ,tempo 
					:unit  ,unit 
					:bars  ,bars 
					:beats  ,beats 
					:subbeats ,subbeats
					:render-once ,render-once
					:transposable ,transposable
					:reversible ,reversible
					:chord-model ,chord-model
					:remarks ,remarks
					:events ,events)))
       (defparameter ,name new-strummer)
       new-strummer)))
					
(setf (documentation 'strummer 'function) cyco-strummer:+macro-docs+)
