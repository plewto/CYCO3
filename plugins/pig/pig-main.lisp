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

(export '(add-media-directory
	  chain
	  channel-filter
	  clear-media-list
	  clear-midi-channels
	  connect
	  delete-operator
	  disconnect
	  distributor
	  dump-media-list
	  initialize
	  is-playing
	  set-pig-player-id
	  make-pig-player
	  midi-input
	  midi-output
	  new-operator
	  panic
	  pig-proxy
	  ping
	  play
	  query-channel-mode
	  query-midi-channels
	  query-midi-receivers
	  query-midi-transmitters
	  query-operators
	  query-roots
	  refresh
	  resume
	  select-media
	  set-midi-channels
	  set-midi-device
	  set-pig-server
	  stop)
	:pig)

(import '(add-media-directory
	  clear-media-list
	  dump-media-list
	  is-playing
	  play
	  resume
	  select-media
	  set-pig-server
	  set-pig-player-id
	  stop)
	:cyco)

	  
	  
