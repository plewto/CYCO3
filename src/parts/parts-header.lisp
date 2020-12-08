;;;; CYCO parts parts-header.lisp
;;;; 
;;;; Creates the :CYCO-PART package and loads related files.
;;;;

(defpackage :cyco-part
  (:use :cl)
  (:import-from :cyco
		:*project*
		:+default-keytable+
		:+rest+
		:+time-signature-properties+
		:->cycle
		:->list
		:->pattern
		:absolute-chords-p
		:approximate
		:articulation-map
		:beat-duration
		:bend->midi-data
		:channel
		:channel-index
		:chord-inversion
		:chord-template
		:copies
		:cycle
		:cyco-composition-error
		:data
		:defines-chord-p
		:disconnect
		:dismiss
		:duration
		:dynamic
		:dynamic-map
		:dynamic-p
		:elide
		:final
		:get-controller-number
		:global
		:init-time-signature
		:invert
		:keynumber
		:keynumber-map
		:limit
		:line
		:metric-expression
		:metric-expression-p
		:midi-channel-pressure
		:midi-control-change
		:midi-control-change-p
		:midi-note-off
		:midi-note-off-p
		:midi-note-on
		:midi-note-on-p
		:midi-pitch-bend
		:name
		:next
		:next-1
		:next-n
		:norm->midi-data
		:partition-list
		:period
		:permute
		:phrase-duration
		:program-change-events
		:program-map
		:pulse
		:ramp
		:ramp
		:range
		:reset
		:rest-p
		:retrograde
		:sawtooth
		:signed-norm->midi-data
		:tempo
		:transpose
		:triangle
		:value
		:while
	:*metronome*
  		:->string
  		:->symbol
  		:banner3
  		:bars
  		:beats
  		:children
  		:clone
  		:connect
  		:constant
  		:copy-time-signature
  		:cyco-node
  		:cyco-type-error
  		:dump-events
  		:elements
  		:false
  		:instrument-layer
  		:instrument-p
  		:midi-message-p
  		:mute
  		:muted-p
  		:parent
  		:part-p
  		:pattern-p
  		:print-tree
  		:project-p
  		:property
  		:put
  		:remarks
  		:render-n
  		:render-once
  		:scopies
  		:section-p
  		:sformat
  		:solo
  		:sort-midi-events
  		:time-signature
  		:unmute
		:wrap))

(in-package :cyco-part)

(defun ->cyco-symbol (sym)
  (->symbol (string-upcase (->string sym)) :cyco))

(defmacro push? (item list) 
  `(if ,item (push ,item ,list)))

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
(src-load "control-ball-docs")
(src-load "control-ball")
(src-load "control-ball-midi-render")

(export '(*strummer-render-trace*
	  control-ball
	  control-ball-p
	  make-control-ball
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
          cyco-part:control-ball
          cyco-part:control-ball-p
          cyco-part:make-control-ball
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

