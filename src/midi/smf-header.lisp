;;;; CYCO
;;;; 

(defclass smf-header nil
  ((format
    :type integer
    :reader smf-format
    :initform 1
    :initarg :format)
   (division
    :type integer
    :reader smf-division
    :initform *TICKS-PER-BEAT*
    :initarg :division)))

(defmethod render-smf-header ((header smf-header)(track-count integer))
  (flet ((msb (n)(logand (ash n -8) #xFF))
	 (lsb (n)(logand n #xFF)))
    (list 77 84 104 100 ; Chunck ID "MThd"
	  0 0 0 6
	  (msb (smf-format header))
	  (lsb (smf-format header))
	  (msb track-count)
	  (lsb track-count)
	  (msb (smf-division header))
	  (lsb (smf-division header)))))



