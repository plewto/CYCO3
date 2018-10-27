;;;; CYCO3 src/midi/syscommon
;;;;


(defclass midi-system-common-message (midi-message) nil)

(defmethod midi-system-common-message-p ((obj t)) nil)
(defmethod midi-system-common-message-p ((evn midi-system-common-message)) t)

;;; ---------------------------------------------------------------------- 
;;;                       MIDI-SYSTEM-EXCLUSIVE

(defclass midi-system-exclusive (midi-system-common-message)
  ((command
    :initform +SYSTEM-EXCLUSIVE+)
   (priority
    :initform 5)
   (data
    :type vector
    :initform #()
    :reader data-array
    :initarg :data)))

(defun midi-system-exclusive (arg)
  "Creates instance of MIDI-SYSTEM-EXCLUSIVE message.
arg may either be a integer, list or vector.
If arg is a list or vector it should contain the sysex data bytes.
If arg is a integer an empty (all 0) vector of that length is created."
  (if (numberp arg)
      (setf arg (make-array (truncate arg) 
			    :element-type 'integer :fill-pointer t)))
  (make-instance 'midi-system-exclusive :data (->vector arg)))

(defmethod midi-system-exclusive-p ((obj t)) nil)
(defmethod midi-system-exclusive-p ((evn midi-system-exclusive)) t)

(defmethod data-count ((evn midi-system-exclusive))
  (length (data-array evn)))

(defmethod data ((evn midi-system-exclusive)(index integer))
  (aref (data-array evn) index))

(defmethod ->string ((evn midi-system-exclusive))
  (let ((acc "")
	(line-count 40))
    (dotimes (i (data-count evn))
      (setf acc (str+ acc (sformat  "~2X" (data evn i))))
      (setf line-count (1- line-count))
      (if (zerop line-count)
	  (progn
	    (setf acc (sformat  "~%"))
	    (setf line-count 40))))
    (str+ (sformat  "~A " (gethash +SYSTEM-EXCLUSIVE+ +MNEMONICS+))
	  (sformat  "~A~%" acc))))

(defmethod render-midi-message ((evn midi-system-exclusive))
  (let* ((data (data-array evn))
	 (acc (list (command evn))))
    (dotimes (i (length data))
      (push (aref data i) acc))
    (reverse acc)))


;;; ---------------------------------------------------------------------- 
;;;			 MIDI-END-SYSTEM-EXCLUSIVE (Singleton)

(defclass midi-end-system-exclusive (midi-system-common-message)
  ((command
    :initform +END-EXCLUSIVE+)
   (priority
    :initform 6)))

(defmethod midi-end-system-exclusive-p ((obj t)) nil)
(defmethod midi-end-system-exclusive-p ((evn midi-end-system-exclusive)) t)

(constant-function midi-end-system-exclusive
		   (make-instance 'midi-end-system-exclusive))

(defmethod data-count ((evn midi-end-system-exclusive)) 0)

(defmethod ->string ((evn midi-end-system-exclusive))
  (sformat  "~A " (gethash +END-EXCLUSIVE+ +MNEMONICS+)))

(defmethod render-midi-message ((evn midi-end-system-exclusive))
  (list +END-EXCLUSIVE+))
  

