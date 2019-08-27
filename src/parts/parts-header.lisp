;;;; CYCO
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
		:->pattern
		:->list
  		:->string
  		:->symbol
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
		:dismiss
		:duration
		:dynamic
		:dynamic-map
		:dynamic-p
		:get-controller-number
		:global
		:invert
		:keynumber
		:keynumber-map
		:limit
		:line
		:metric-expression
		:metric-expression-p
		:midi-channel-pressure
		:midi-control-change
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
		:permute
		:phrase-duration
		:program-change-events
		:program-map
		:range
		:reset
		:rest-p
		:retrograde
		:signed-norm->midi-data
		:tempo
		:transpose
		:value
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
		:final
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
  		:unmute))

(in-package :cyco-part)

(defun ->cyco-symbol (sym)
  (->symbol (string-upcase (->string sym)) :cyco))

(defmacro push? (item list) 
  `(if ,item (push ,item ,list)))

(defun src-load (filename)
  (let ((fqn (sformat "src/parts/~A" filename)))
    (format t "    ~A~%" fqn)
    (load fqn)))

(src-load "expect")
(src-load "part")
(src-load "raw-part")
(src-load "qball-docs")
(src-load "qball")
(src-load "qball-midi-render")
(src-load "metronome")
(src-load "programs")
(src-load "ghost")
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
(src-load "controllers-midi-render")

(export '(controllers
	  controllers-p
	  ghost
	  ghost-p
	  make-controllers
	  make-ghost
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

(import '(cyco-part:controllers
	  cyco-part:controllers-p
	  cyco-part:ghost
	  cyco-part:ghost-p
	  cyco-part:make-controllers
	  cyco-part:make-ghost
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

