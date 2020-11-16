;;;; pig-main
;;;; CYCO interface to Pigiron MIDI utility.
;;;;

(defpackage :pig
  (:use :cl)
  (:import-from :cyco
		:param
		:load-plugin-file
		:sformat
		:str+
		:->list
		))

(in-package :pig)

 (load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket)
  (ql:quickload :uiop))

(load-plugin-file 'util)
(load-plugin-file 'proxy)
(load-plugin-file 'midi-ports)
(load-plugin-file 'channel-operators)
(load-plugin-file 'player)
(load-plugin-file 'documentation)

(export '(pig-proxy
	  *pig-server*
	  ping
	  panic
	  initialize
	  refresh
	  query-midi-transmitters
	  query-midi-receivers
	  query-roots
	  query-operators
	  new-operator
	  delete-operator
	  connect
	  chain
	  disconnect
	  set-midi-device
	  midi-input
	  midi-output
	  query-channel-mode
	  query-midi-channels
	  set-midi-channels
	  clear-midi-channels
	  channel-filter
	  distributor
	  make-midi-player
	  play
	  stop
	  resume
	  is-playing
	  add-media-directory
	  dump-media-list
	  select-media
	  clear-media-list)
	:pig)

(import '(assign-midi-player-id
	  play
	  stop
	  resume
	  is-playing
	  add-media-directory
	  select-media
	  dump-media-list
	  clear-media-list)
	:cyco)

	  
	  
