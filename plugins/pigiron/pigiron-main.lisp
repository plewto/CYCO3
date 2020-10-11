;;;; pigiron-main
;;;;
;;;; Provides OSC communication to Pigiron MIDI utility.
;;;;


(defpackage :pigiron
  (:use :cl)
  (:import-from :cyco
		:str+
		:param
		:load-plugin-file
		:resolve-user-home
		:while
		:->string
		:->symbol
		:dismiss
		:split-string
		:sformat
		:final
		:identity
		:split-path
		:split-extension)
  (:nicknames :pig))

(in-package :pigiron)

(load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket)
  (ql:quickload :uiop))

(load-plugin-file 'util) 
(load-plugin-file 'proxy)
(load-plugin-file 'io)  
(load-plugin-file 'channels)
(load-plugin-file 'player)
(load-plugin-file 'documentation)


(export '(pigiron-proxy
	  ping
	  panic
	  initialize
	  refresh
	  load-configuration
	  new-operator
	  delete-operator
	  connect
	  disconnect
	  query-roots
	  query-operators
	  query-operator-info
	  query-midi-transmitters
	  query-midi-receivers
	  set-midi-device
	  midi-input
	  midi-output
	  query-channel-mode
	  query-midi-channels
	  enable-midi-channel
	  enable-all-channels
	  disable-all-channels
	  query-midi-channel
	  select-midi-channel
	  player-id
	  add-media
	  can-record
	  is-recording
	  is-playing
	  current-media
	  current-media-url
	  current-media-duration
	  player-position
	  player-relative-position
	  clear-media-list
	  stop
	  play
	  player-continue
	  add-media-directory
	  remove-media
	  select-media
	  media-list
	  seek)
	:pigiron)

(import '(pigiron-proxy
	  player-id
	  clear-media-list
	  add-media-directory
	  add-media
	  remove-media
	  select-media
	  media-list	  
	  is-playing
	  current-media
	  current-media-url
	  current-media-duration
	  player-position
	  player-relative-position
	  stop
	  play
	  player-continue
	  seek)
	:cyco)
