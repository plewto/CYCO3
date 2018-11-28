;;;; PigIron Profile MIDI ports
;;;;
;;;; query-device
;;;; set-device
;;;;


(flet ((is-input-port (op)
	(search "midi-input" (string-downcase (->string op)))))

  (defun ?pig-midi-device (op)
    "Instructs PigIron to display name of MIDI device serving operator op.
op should be either a midi-input or midi-output"
    (let ((cmd (if (is-input-port op) 'query-receiver 'query-transmitter)))
      (osc-send nil :address (join-address op cmd))))
    
  (defun pig-set-midi-device (op devname)
    "Instructs PigIron to use named MIDI device to service operator.
The operator should be either a midi-input or midi-output.
Use ?pig-translators (inputs) and ?pig-receivers (outputs) to view list 
of available devices.

devname may be a partial device name."
    (let ((cmd (if (is-input-port op) 'set-receiver 'set-transmitter)))
      (osc-send (list devname) :address (join-address op cmd)))))
