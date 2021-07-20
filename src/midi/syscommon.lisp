;;;; CYCO midi syscommon.lisp
;;;;
;;;; Defines MIDI "system common" messages.
;;;;

(in-package :cyco)


(defclass midi-system-common-message (midi-message) nil)

(defmethod midi-system-common-message-p ((message midi-system-common-message)) t)

(defmethod data-count ((message midi-system-common-message)) 0)

(defmethod data ((message midi-system-common-message)(index t))
  (declare (ignore index))
  nil)

(defmethod render-midi-message ((message midi-system-common-message))
  (list (command message)))

(defmethod ->string ((message midi-system-common-message))
  (->string (gethash (command message) +MNEMONICS+)))

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

(let ((instance (make-instance 'midi-system-common-message
			  :command +END-EXCLUSIVE+
			  :priority 06)))

  (constant-function midi-end-system-exclusive instance)

  (defmethod midi-midi-end-system-exclusive-p ((obj t)) nil)
  
  (defmethod midi-end-system-exclusive-p ((message midi-system-common-message))
    (= (command message) +END-EXCLUSIVE+)))


;;; ----------------------------------------------------------------------
;;;                      MIDI-RESET (Singleton)

(let ((instance (make-instance 'midi-system-common-message
				    :command +RESET+
				    :priority 1)))

  (constant-function midi-reset instance)

  (defmethod midi-reset-p ((obj t)) nil)
  
  (defmethod midi-reset-p ((message midi-system-common-message))
    (= (command message) +RESET+)))


;;; ---------------------------------------------------------------------
;;;                 MIDI-START (Singleton)

(let ((instance (make-instance 'midi-system-common-message
				    :command +START+
				    :priority 3)))

  (constant-function midi-start instance)

  (defmethod midi-start-p ((obj t)) nil)
  
  (defmethod midi-start-p ((message midi-system-common-message))
    (= (command message) +START+)))


;;; ---------------------------------------------------------------------
;;;                 MIDI-STOP (Singleton)

(let ((instance (make-instance 'midi-system-common-message
				    :command +STOP+
				    :priority 4)))

  (constant-function midi-stop instance)

  (defmethod midi-stop-p ((obj t)) nil)
  
  (defmethod midi-stop-p ((message midi-system-common-message))
    (= (command message) +STOP+)))


;;; ---------------------------------------------------------------------
;;;                 MIDI-CONTINUE (Singleton)

(let ((instance (make-instance 'midi-system-common-message
				    :command +CONTINUE+
				    :priority 3)))

  (constant-function midi-continue instance)
  
  (defmethod midi-continue-p ((obj t)) nil)
  (defmethod midi-continue-p ((message midi-system-common-message))
    (= (command message) +CONTINUE+)))


;;; ---------------------------------------------------------------------
;;;                 MIDI-CLOCK (Singleton)

(let ((instance (make-instance 'midi-system-common-message
				    :command +CLOCK+
				    :priority 2)))

  (constant-function midi-clock instance)

  (defmethod midi-clock-p ((obj t)) nil)
  
  (defmethod midi-clock-p ((message midi-system-common-message))
    (= (command message) +CLOCK+)))
