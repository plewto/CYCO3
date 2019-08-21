;;;; CYCO
;;;;

(defpackage :cyco-part
  (:use :cl)
  (:import-from :cyco
		:+rest+
  		:+time-signature-properties+
		:+default-keytable+
		:*metronome*
  		:*project*
		:->cycle
  		:->list
		:->pattern
  		:->string
  		:->symbol
		:absolute-chords-p
		:approximate
		:articulation-map
  		:banner3
  		:bars
  		:beats
		:beat-duration
		:bend->midi-data
		:channel
		:channel-index
  		:children
		:chord-inversion
		:chord-template
  		:connect
  		:constant
		:copies
  		:clone
  		:copy-time-signature
		:cycle
		:cyco-composition-error
  		:cyco-node
  		:cyco-type-error
		:data
		:defines-chord-p
		:dismiss
  		:dump-events
		:duration
		:dynamic
		:dynamic-map
		:dynamic-p
  		:elements
  		:false
		:get-controller-number
		:global
  		:instrument-layer
  		:instrument-p
		:invert
		:keynumber
		:keynumber-map
		:limit
		:line
		:metric-expression
		:metric-expression-p
		:midi-channel-pressure
		:midi-control-change
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
		:norm->midi-data
  		:part-p
  		:parent
		:partition-list
  		:pattern-p
		:permute
		:phrase-duration
  		:print-tree
		:program-change-events
		:program-map
  		:property
  		:project-p
  		:put
		:range
  		:remarks
  		:render-once
  		:render-n
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
  		:time-signature
		:transpose
		:value
  		:unmute
  		))

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



(export '(make-controllers
	  make-ghost
	  make-metronome
	  make-programs
	  make-qball
	  make-simple-part
	  make-raw-part
	  make-strummer
	  controllers
	  controllers-p
	  ghost
	  ghost-p
	  metronome
	  programs
	  qball
	  qball-p
	  raw-part
	  raw-part-p
	  simple-part
	  simple-part-p
	  strummer
	  strummer-p
	  ) :cyco-part)

(import '(cyco-part:make-controllers
	  cyco-part:make-ghost
	  cyco-part:make-metronome
	  cyco-part:make-programs
	  cyco-part:make-qball
	  cyco-part:make-simple-part
	  cyco-part:make-strummer
	  cyco-part:make-raw-part
	  cyco-part:metronome
	  cyco-part:ghost
	  cyco-part:ghost-p
	  cyco-part:controllers
	  cyco-part:controllers-p
	  cyco-part:programs
	  cyco-part:qball
	  cyco-part:qball-p
	  cyco-part:raw-part
	  cyco-part:raw-part-p
	  cyco-part:simple-part
	  cyco-part:simple-part-p
	  cyco-part:strummer
	  cyco-part:strummer-p
	  ) :cyco)


(in-package :cyco)

