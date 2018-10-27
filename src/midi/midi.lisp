;;;; CYCO3 src/midi/midi
;;;;
;;;; Terminology:
;;;;
;;;;   1) A MIDI 'message' is a class or instance or byte array representing a
;;;;      single MIDI command without reference to time.
;;;;   2) A MIDI 'event' is a cons of event-time and MIDI message:
;;;;      (time . message)

(defgeneric mnemonic (obj)
  (:documentation
   "Returns mnemonic text for object."))

(defgeneric data-count (obj))

(defgeneric data  (obj index)
  (:documentation
   "Returns data value from object."))

(defgeneric render-midi-message (obj)
  (:documentation
   "Translates object into list of MIDI bytes."))

(defgeneric render-smf-header (obj track-count))

(defgeneric render-smf-track (trk pad))
(defgeneric dump-smf-track-chunk (trk pad))

(defgeneric smf-track-count (obj)
  (:documentation
   "Returns number of SMF tracks."))

(defgeneric smf-track (obj &optional index)
  (:documentation
   "Returns smf-track object
index - the track number (currently 0 is only valid value)."))
  
(defgeneric smf-track! (obj trk &optional index)
  (:documentation
   "Sets track object for SMF MIDI file."))

(defgeneric render-smf (obj &key pad)
  (:documentation
   "Render SMF object to list of MIDI bytes."))

(defgeneric write-smf (obj filename &key pad no-overwrite)
  (:documentation
   "Render SMF object and writes results to file.
obj - SMF object
filename - string
pad - Number of seconds added to final track event.
no-overwrite - if true throw error if filename exists."))

(defgeneric midi-message-p (obj))
(defgeneric midi-channel-message-p (obj))
(defgeneric midi-key-message-p (obj))
(defgeneric midi-note-off-p (obj))
(defgeneric midi-note-on-p (obj))
(defgeneric midi-poly-pressure-p (obj))
(defgeneric midi-control-chnage-p (obj))
(defgeneric midi-channel-pressure-p (obj))
(defgeneric midi-program-change-p (obj))
(defgeneric midi-pitch-bend-p (obj))
(defgeneric midi-system-common-message-p (obj))
(defgeneric midi-system-exclusive-p (obj))
(defgeneric midi-end-system-exclusive-p (obj))
(defgeneric midi-meta-message-p (obj))
(defgeneric midi-meta-text-p (obj))
(defgeneric midi-meta-copyright-p (obj))
(defgeneric midi-meta-track-name-p (obj))
(defgeneric midi-meta-instrument-name-p (obj))
(defgeneric midi-meta-lyric-p (obj))
(defgeneric midi-meta-cue-p (obj))
(defgeneric midi-meta-marker-p (obj))
(defgeneric midi-end-of-track-p (obj))
(defgeneric midi-tempo-message-p (obj))
(defgeneric midi-time-signature-p (obj))
(defgeneric midi-key-signature-p (obj))

;; Lower values have higher priority
;;
