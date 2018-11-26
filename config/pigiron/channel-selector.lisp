;;;; PigIron Profile Channel Selectors
;;;;
;;;; Defines OSC messages for Operators which Implement ChannelSelector.
;;;; Currently these are ChannelFilter and ChannelDistributor
;;;;

;;;; query-channel id n --> boolean
;;;; enable-channel id n flag
;;;; enable-all id 
;;;; disable-all id


(defun ?pig-channel-enabled (op channel)
  "Instructs PigIron to display enable state of operator MIDI channel.
op - operator name
channel- MIDI cjhannel"
  (osc-send (list (->string channel))
	    :address (join-address op 'query-channel)))

(defun pig-enable-channel (op channel flag)
  "Instructs PigIron to change enable status oeprator of MIDI channel.
op - opertor name
channel - MIDI channel
flag - false to disable, true to enable."
  (osc-send (list (->string channel)
		  (bool flag))
	    :address (join-address op 'enable-channel)))

(defun pig-enable-all-channels (op flag)
  "Instructs PigIron to enable/disable all channels of operator.
op - operator name
channel - MIDI channel
flag - false to disable all, true enables all."

  (let ((cmd (if flag 'enable-all 'disable-all)))
    (osc-send (list (bool flag))
	      :address (join-address op cmd))))

  
