;;;; CYCO parts parts-header.lisp
;;;; 
;;;; Creates the :CYCO-PART package and loads related files.
;;;;

(defpackage :cyco-part
  (:use :cl)
  (:import-from :cyco
                :*metronome*
                :*project*
                :+default-keytable+
                :+rest+
                :+time-signature-properties+
                :->cycle
                :->list
                :->pattern
                :->string
                :->symbol
                :absolute-chords-p
                :approximate
                :articulation-map
                :banner3
                :bar
                :bars
                :beat-duration
                :beats
                :bend->midi-data
                :channel
                :channel-index
                :children
                :chord-inversion
                :chord-template
                :clone
		:command
                :connect
                :constant
                :copies
                :copy-time-signature
                :cycle
                :cyco-composition-error
                :cyco-node
                :cyco-type-error
                :data
                :defines-chord-p
                :disconnect
                :dump-events
                :duration
                :dynamic
                :dynamic-map
                :dynamic-p
                :elements
                :elide
                :false
                :final
		:flatten
                :get-controller-number
                :global
                :init-time-signature
                :instrument-layer
                :instrument-p
                :invert
                :pulse
                :sawtooth
                :triangle
                :keynumber
                :keynumber-map
                :limit
                :line
                :metric-expression
                :metric-expression-p
		:midi-channel-message-p
                :midi-channel-pressure
		:midi-clock
                :midi-control-change
                :midi-control-change-p
                :midi-message-p
                :midi-note-off
                :midi-note-off-p
                :midi-note-on
                :midi-note-on-p
                :midi-pitch-bend
                :mute
                :muted-p
                :name
                :next
                :next-1
                :next-n
                :no-shuffle
                :norm->midi-data
                :parent
                :part-p
                :partition-list
                :pattern-p
                :period
                :permute
                :phrase-duration
                :print-tree
                :program-change-events
                :program-map
                :project-p
                :property
                :put
		:ramp
                :range
                :remarks
                :render-n
                :render-once
                :reset
                :rest-p
                :retrograde
                :scopies
                :section-p
                :sformat
                :signed-norm->midi-data
                :solo
                :sort-midi-events
                :tempo
		:thin-bend-events
		:thin-controller-events
                :time-signature
                :transpose
		:true
                :unmute
                :value
                :while
                :wrap)) 

(in-package :cyco-part)

(defun ->cyco-symbol (sym)
  (->symbol (string-upcase (->string sym)) :cyco))

(defmacro push? (item list) 
  `(if ,item (push ,item ,list)))

(defun scale-time-parameter (param time-signature)
  (let ((scale (float (if (numberp param) 1.0 (beat-duration time-signature)))))
    (* scale (metric-expression param))))

(defun src-load (filename)
  (let ((fqn (sformat "src/parts/~A" filename)))
    (format t "    ~A~%" fqn)
    (load fqn)))

(defgeneric pattern-reset (part-or-state))


