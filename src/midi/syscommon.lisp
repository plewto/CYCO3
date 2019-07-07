;;;; CYCO
;;;;


(defclass midi-system-common-message (midi-message) nil)

(defmethod midi-system-common-message-p ((message midi-system-common-message)) t)

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

(defgeneric midi-system-exclusive (argument)
  (:documentation
   "Makes new instance of MIDI-SYSTEM-EXCLUSIVE"))

(defmethod midi-system-exclusive ((data vector))
  "Makes new instance of MIDI-SYSTEM-EXCLUSIVE from explicit data array."
  (make-instance 'midi-system-exclusive :data data))

(defmethod midi-system-exclusive ((list list))
  "Makes new instance of MIDI-SYSTEM-EXCLUSIVE from explicit data list."
  (midi-system-exclusive (->vector list)))

(defmethod midi-system-exclusive ((data-length integer))
  "Makes new instance of MIDI-SYSTEM-EXCLUSIVE with initially zeroed data array of given length."
  (midi-system-exclusive (make-array data-length :element-type 'integer :fill-pointer t)))

(defmethod midi-system-exclusive-p ((message midi-system-exclusive)) t)

(defmethod data-count ((message midi-system-exclusive))
  (length (data-array message)))

(defmethod data ((message midi-system-exclusive)(index integer))
  (aref (data-array message) index))

(defmethod ->string ((message midi-system-exclusive))
  (let ((result "")
	(line-count 40))
    (dotimes (i (data-count message))
      (setf result (str+ result (sformat  "~2X" (data message i))))
      (setf line-count (1- line-count))
      (if (zerop line-count)
	  (progn
	    (setf result (sformat  "~%"))
	    (setf line-count 40))))
    (str+ (sformat  "~A " (gethash +SYSTEM-EXCLUSIVE+ +MNEMONICS+))
	  (sformat  "~A~%" result))))

(defmethod render-midi-message ((message midi-system-exclusive))
  (let* ((data (data-array message))
	 (bytes (list (command message))))
    (dotimes (i (length data))
      (push (aref data i) bytes))
    (reverse bytes)))


;;; ---------------------------------------------------------------------- 
;;;			 MIDI-END-SYSTEM-EXCLUSIVE (Singleton)

(defclass midi-end-system-exclusive (midi-system-common-message)
  ((command
    :initform +END-EXCLUSIVE+)
   (priority
    :initform 6)))

(defmethod midi-end-system-exclusive-p ((message midi-end-system-exclusive)) t)

(constant-function midi-end-system-exclusive
		   (make-instance 'midi-end-system-exclusive))

(defmethod data-count ((message midi-end-system-exclusive)) 0)

(defmethod ->string ((message midi-end-system-exclusive))
  (sformat  "~A " (gethash +END-EXCLUSIVE+ +MNEMONICS+)))

(defmethod render-midi-message ((message midi-end-system-exclusive))
  (list +END-EXCLUSIVE+))
  



