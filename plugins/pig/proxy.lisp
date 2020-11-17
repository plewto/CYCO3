;;;; cyco3 plugin pig proxy
;;;;

(in-package :pig)


;;; CYCO side representation for a  Pigiron instance.
;;;
;;;  Usually there is a single instance of pig-proxy
;;;  which is bound to the global symbol *pig-server*
;;;  Most related functions default to *pig-server*
;;;
;;; In general when CYCO transmits an OSC message to Pigiron,
;;; it writes a tempoaray file in response.  The first line
;;; of this file is either :ACK (acknowledgment) or :ERROR.
;;; Each OSC transmitting function checks this file to
;;; both determine of the call was sucessful and to extract
;;; any returned data.   Once the file has been read it
;;; is deleted.  A nil result indicates an error.
;;;
;;; There is an artifical delay of about 1/50 second between
;;; transmitting an OSC message and checking the response file.
;;; This is to ensure Pigiron has had enough time to write
;;; the file.
;;;


(defclass pig-proxy nil
  ((host
    :accessor host
    :initform #(127 0 0 1)
    :initarg :host
    :documentation "IP address for Pigiron application")
   (port
    :accessor port
    :type fixnum
    :initform 7000
    :initarg :port
    :documentation "Port number where Pigiron receives OSC messages.")
   (id
    :accessor id
    :type string
    :initform "pig"
    :initarg :id
    :documentation "OSC address prefix for all messages to Pigiron.")
   (response-file-name
    :accessor response-filename
    :type string
    :initform "/tmp/pigiron-response"
    :initarg :response-file
    :documentation "Location of temporary file where Pigiron stores
Responses to received OSC messages.")
   (delete-after-read
    :accessor delete-after-read
    :type t
    :initform nil
    :initarg :delete-after-read
    :documentation "Flag indicates if response file should be deleted
after reading.")
   (response-read-delay
    :accessor response-read-delay
    :type float
    :initform 0.2
    :initarg :read-delay
    :documentation "Time delay in seconds for reading the OSC response file."))
  (:documentation
   "Defines OSC interface to Pigiron application"))
    
(param *pig-server* nil)

(defun set-pig-server (&key (host #(127 0 0 1))
			    (port 7000)
			    (id "pig")
			    (response-filename "/tmp/pigiron-response")
			    (read-delay 0.2))
  (setf *pig-server* (make-instance 'pig-proxy 
				    :host host 
				    :port port
				    :id id
				    :response-file response-filename
				    :read-delay read-delay))
  *pig-server*)

(set-pig-server)


(defmethod pig-warning ((message string) &optional extra)
  (format t "~%PIGIRON-error ******************************~%")
  (format t "  ~A~%" message)
  (dotimes (index (length extra))
    (format t "  [line ~3d] ~S~%" index (nth index extra)))
  (format t "~%"))


(defun format-osc-address (command &optional (server *pig-server*))
  (str+ "/" (id server) "/" command))


(defun osc-send (address &key (host "127.0.0.1")(port 7000)(data nil))
  (let* ((socket (usocket:socket-connect host port
					 :protocol :datagram
					 :element-type '(unsigned-byte 8)))
	 (arguments (if (listp data) data (list data)))
	 (message (apply #'osc:encode-message (cons address arguments))))
    (unwind-protect
	(usocket:socket-send socket message (length message))
      (when socket (usocket:socket-close socket)))))

(defun send (command &key data (server *pig-server*))
  (let ((address (format-osc-address command server)))
    (osc-send (format-osc-address command server)
	      :host (host server)
	      :port (port server)
	      :data data)
    address))


(labels ((file-exists (filename)
	    (if (probe-file filename)
		t
	      (progn
		(pig-warning (cyco:sformat "Response file ~s does not exists" filename))
		nil)))
	 
	 (read-file-to-list (filename)
	    (uiop:read-file-lines filename))

	 (verify-acknowledgment (file-contents key)
	    (let ((received (first file-contents)))
	      (if (string= received key)
		  t
		(let* ((msg1 (sformat "Did not receive expected OSC response.~%"))
		       (msg2 (sformat "  Expected : ~S~%" key))
		       (msg3 (sformat "  Received : ~S" received)))
		  (pig-warning (str+ msg1 msg2 msg3) file-contents)
		  nil))))
	 
	 (verify-signature (file-contents osc-address)
	    (let ((signature (cyco:split-string (second file-contents))))
	      (if (string= (second signature) osc-address)
		  t
		(let ((msg1 (sformat "Did not receive expected OSC signature.~%"))
		      (msg2 (sformat "  Expected : ~S~%" osc-address))
		      (msg3 (sformat "  Received : ~S~%" (second signature))))
		  (pig-warning (str+ msg1 msg2 msg3))
		  nil))))
	 
	 (find-data-count (file-content)
	    (let ((index 0)
		  (count 0)
		  (more t))
	      (cyco:while (and more (< index (length file-content)))
			  (let* ((line (nth index file-content))
				 (fields (cyco:split-string line)))
			    (if (string= (car fields) ":DATA-COUNT")
				(setf count (parse-integer (second fields))
				      index (1+ index)
				      more nil)
			      (setf index (1+ index)))))
	      (values count index)))
	 
	 (extract-data (file-contents)
	    (multiple-value-bind (count index)
		(find-data-count file-contents)
	      (if (plusp count)
		  (nthcdr index file-contents)
		nil))) )

  (defmethod read-response ((server pig-proxy)(osc-address string)
			    &key (acknoledgement-key ":ACK")(delay nil))
    (sleep (or delay (response-read-delay server)))
    (let ((filename (cyco:resolve-user-home (response-filename server))))
      (if (file-exists filename)
	  (let ((contents (read-file-to-list filename)))
	    (if (and (verify-acknowledgment contents acknoledgement-key)
		     (verify-signature contents osc-address))
		(prog1
		    (or (extract-data contents) t)
		  (if (delete-after-read server)
		      (delete-file (pathname filename))))))))) )


(defmacro simple-osc-command (name command)
  `(defun ,name (&key (server *pig-server*))
     (read-response server (send ,command :server server))))

(simple-osc-command ping "ping")
(simple-osc-command panic "panic")
(simple-osc-command initialize "initialize")
(simple-osc-command refresh "refresh")
(simple-osc-command query-midi-transmitters "q-midi-transmitters")
(simple-osc-command query-midi-receivers "q-midi-receivers")
(simple-osc-command query-roots "q-roots")
(simple-osc-command query-operators "q-operators")

;; Returns actual operator id
(defun new-operator (operator-type id &key (server *pig-server*))
  (read-response server (send "new-operator" :server server :data (list operator-type id))))


(defun delete-operator (id &key (server *pig-server*))
  (read-response server (send "delete-operator" :server server :data (list id))))


(defun connect (parent child &key (server *pig-server*))
  (read-response server (send "connect" :server server :data (list parent child))))

;; call connect on list of operators in sequence.
(defun chain (chain &key (server *pig-server*))
  (if (listp chain)
      (let ((index 1))
	(cyco:while (< index (length chain))
	  (let* ((parent (nth (1- index) chain))
		 (child (nth index chain))
		 (result (connect parent child :server server)))
	    (if (not result)
		(error (sformat "pig:chain function, invalid operator id: ~s" chain)))
	    (setf index (1+ index))))
	t)
    (error (sformat "pig:chain function expected list argument: encountered ~a" chain))))
    
		  
(defun disconnect (parent child &key (server *pig-server*))
  (read-response server (send "disconnect" :server server :data (list parent child))))




