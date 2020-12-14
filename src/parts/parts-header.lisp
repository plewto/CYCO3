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
                :dismiss
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
                :ipulse
                :iramp
                :isawtooth
                :itriangle
                :keynumber
                :keynumber-map
                :limit
                :line
                :metric-expression
                :metric-expression-p
                :midi-channel-pressure
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
                :pulse
                :put
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
		:thin-controller-events
                :time-signature
                :transpose
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

(src-load "expect")
(src-load "part")
(src-load "raw-part")
(src-load "qball-docs")
(src-load "qball")
(src-load "qball-midi-render")
(src-load "metronome")
(src-load "programs")
(src-load "simple-docs")
(src-load "simple-state")
(src-load "simple-part")
(src-load "simple-part-midi-render")
(src-load "strummer-docs")
(src-load "strummer-state")
(src-load "strummer")
(src-load "strummer-midi-render")
(src-load "controllers-docs")
(src-load "controllers-state")
(src-load "controllers")
(src-load "controllers-render")

(src-load "pressure")


(export '(*strummer-render-trace*
	  controllers
	  controllers-p
	  make-controllers
	  make-metronome
	  make-programs
	  make-qball
	  make-raw-part
	  make-simple-part
	  make-strummer
	  metronome
	  programs
	  qball
	  qball-p
	  raw-part
	  raw-part-p
	  simple-part
	  simple-part-p
	  strummer
	  strummer-p) :cyco-part)

(import '(cyco-part:*strummer-render-trace*
          cyco-part:controllers
          cyco-part:controllers-p
          cyco-part:make-controllers
          cyco-part:make-metronome
          cyco-part:make-programs
          cyco-part:make-qball
          cyco-part:make-raw-part
          cyco-part:make-simple-part
          cyco-part:make-strummer
          cyco-part:metronome
          cyco-part:programs
          cyco-part:qball
          cyco-part:qball-p
          cyco-part:raw-part
          cyco-part:raw-part-p
          cyco-part:simple-part
          cyco-part:simple-part-p
          cyco-part:strummer
          cyco-part:strummer-p) :cyco)

(in-package :cyco)

