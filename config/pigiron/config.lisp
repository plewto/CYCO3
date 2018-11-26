;;;; CYCO PigIron Profile
;;;;

(defpackage :pigiron
  (:use :cl)
  (:import-from :cyco :global :str+ :->string
		:osc-send :name :load-profile-file))

(in-package :pigiron)

(global *pigiron-host* cyco:*local-host*)
(global *pigiron-port* 65000)
(global *pigiron-id* "/pigiron")
(global *midi-file-player-id* "midi-file-player-1")



(defun bool (arg)
  (if arg "TRUE" "FALSE"))

(defun join-address (&rest args)
  (let ((acc *pigiron-id*))
    (dolist (a args)
      (setf acc (str+ acc "/" (string-downcase (->string a)))))
    acc))

(defmacro defpig (name command &optional (documentation ""))
  `(defun ,name ()
     ,documentation
       (osc-send nil :address (join-address ',command))))

(defmacro defpig-1 (name argument command &optional (documentation ""))
  `(defun ,name (,argument)
     ,documentation
     (osc-send (list ,argument) :address (join-address ',command))))

(defmacro defpig-2 (name arg1 arg2 command &optional (documentation ""))
  `(defun ,name (,arg1 ,arg2)
     ,documentation
     (osc-send (list ,arg1 ,arg2) :address (join-address ',command))))

(load-profile-file "global-messages")
(load-profile-file "channel-selector")
(load-profile-file "midi-file-player")
(load-profile-file "midi-ports")

(export '(*midi-file-player-id*
	  *pigiron-host*
	  *pigiron-id*	
	  *pigiron-port*	
	  ?pig-channel-enabled
	  ?pig-forest
	  ?pig-is-playing
	  ?pig-midi-device
	  ?pig-midi-filename
	  ?pig-operators
	  ?pig-optypes
	  ?pig-receivers
	  ?pig-transmitters
	  pig-clear-forest
	  pig-connect
	  pig-create-operator
	  pig-disconnect
	  pig-enable-all-channels
	  pig-enable-channel
	  pig-forget-operator
	  pig-isolate-operator
	  pig-load-midi-file
	  pig-panic
	  pig-ping
	  pig-play
	  pig-reset
	  pig-set-midi-device
	  pig-stop
	  pig-sync-ui)
	:pigiron)

(import '(pigiron:*midi-file-player-id*
	  pigiron:*pigiron-host*
	  pigiron:*pigiron-id*	
	  pigiron:*pigiron-port*	
	  pigiron:?pig-channel-enabled
	  pigiron:?pig-forest
	  pigiron:?pig-is-playing
	  pigiron:?pig-midi-device
	  pigiron:?pig-midi-filename
	  pigiron:?pig-operators
	  pigiron:?pig-optypes
	  pigiron:?pig-receivers
	  pigiron:?pig-transmitters
	  pigiron:pig-clear-forest
	  pigiron:pig-connect
	  pigiron:pig-create-operator
	  pigiron:pig-disconnect
	  pigiron:pig-enable-all-channels
	  pigiron:pig-enable-channel
	  pigiron:pig-forget-operator
	  pigiron:pig-isolate-operator
	  pigiron:pig-load-midi-file
	  pigiron:pig-panic
	  pigiron:pig-ping
	  pigiron:pig-play
	  pigiron:pig-reset
	  pigiron:pig-set-midi-device
	  pigiron:pig-stop
	  pigiron:pig-sync-ui) :cyco)

(in-package :cyco)
