;;;; pigiron proxy.lisp
;;;;


(in-package :pigiron)

(defclass pigiron-proxy nil
  ((host
    :accessor host
    :initform #(127 0 0 1)
    :initarg :host
    :documentation "IP address for Pigiron application")
   (port
    :accessor port
    :type fixnum
    :initform 6060
    :initarg :port
    :documentation "Port number where Pigiron receives OSC messages.")
   (id
    :accessor id
    :type string
    :initform "/pig"
    :initarg :id
    :documentation "OSC address prefix for all messages to Pigiron.")
   (response-file-name
    :accessor response-filename
    :type string
    :initform "/tmp/pigiron-osc-response"
    :initarg :response-file
    :documentation "Location of temporary file where Pigiron stores
Responses to received OSC messages.")
   (response-read-delay
    :accessor response-read-delay
    :type float
    :initform 0.1
    :initarg :read-delay
    :documentation "Time delay in seconds for reading the OSC response file.")
   (verbose
    :type boolean
    :accessor verbose-flag
    :initform t
    :initarg :verbose
    :documentation "If true print all transmitted OSC messages to terminal."))
  (:documentation
   "Defines OSC interface to Pigiron application"))


(defmethod pigiron-warning ((message string))
  (format t "PIGIRON-WARNING ******************************~%")
  (format t "  ~A~%~%" message))

(defmethod pigiron-warning ((message-list list))
  (format t "PIGIRON-WARNING ******************************~%")
  (dolist (item message-list)
    (format t "  ~A~%" item))
  (format t "~%"))


(defmethod format-osc-address ((proxy pigiron-proxy)(command string))
  (str+ "/" (id proxy) "/" command))


(labels ((print-verbose(proxy osc-address data)
	  (if (verbose-flag proxy)
	      (progn
		(format t "OSC-SEND ~A  Port ~A: ~S "
			(host proxy)
			(port proxy)
			osc-address)
		(if data
		    (format t "DATA: ~A~%" data)
		  (format t "~%"))))))
  
  (defmethod raw-send ((proxy pigiron-proxy)(osc-address string) &optional data)
    (let* ((socket (usocket:socket-connect (host proxy)(port proxy)
					  :protocol :datagram
					  :element-type '(unsigned-byte 8)))
	   (arguments (if (listp data) data (list data)))
	   (message (apply #'osc:encode-message (cons osc-address arguments))))
      (print-verbose proxy osc-address arguments)
      (unwind-protect
	  (usocket:socket-send socket message (length message))
	(when socket (usocket:socket-close socket))))) )

(defmethod send ((proxy pigiron-proxy)(command string) &optional data)
  (let ((osc-address (format-osc-address proxy command)))
    (raw-send proxy osc-address data)
    osc-address))


(param *default-proxy* nil)


(defun pigiron-proxy (&key (host #(127 0 0 1))(port 6060)(id "pig")(verbose nil))
  (setf *default-proxy*
	(make-instance 'pigiron-proxy :host host :port port :id id :verbose verbose))
  *default-proxy*)


(pigiron-proxy)


(labels ((read-response-file (filename)
	   (setf filename (resolve-user-home filename))
	   (if (probe-file filename)
	       (uiop:read-file-lines filename)
	     (progn
	       (pigiron-warning (sformat "OSC response file ~S does not exists." filename))
	       nil)))

	 (response-file-exists (filename)
	   (probe-file filename))
			       

	 
	 (expected-message-p (raw-contents message)
	    (let ((received-message (->string (final (split-string (second raw-contents))))))
	      (if (string= received-message message)
		  t
		(progn
		  (pigiron-warning (list "Did not receive expected OSC response."
					 (sformat "Expected response to ~S" message)
					 (sformat "Actual response from ~S" received-message)))
		  nil))))

	 (is-not-error (raw-contents)
	    (let ((flag (string-upcase (->string (car raw-contents)))))
	      (if (string= flag ":ERROR")
		  (progn 
		    (pigiron-warning raw-contents)
		    nil)
		t)))

	 (strip-contents-header (raw-contents)
	    (let ((head (string-upcase (->string (car raw-contents)))))
	      (if (string= head ":RESULT")
		  (cdr raw-contents)
		(and raw-contents (strip-contents-header (cdr raw-contents)))))) )


 
  ;; Returns either the "RESULT" contents of the osc response file or
  ;; the keyword :ERROR
  ;;
  ;; The function returns the contents of the response-file as a second value.
  ;;
  ;; If the file does not exists, returns :ERROR
  ;; Deletes file after reading.
  ;;
  (defmethod read-osc-response ((proxy pigiron-proxy)(message string) &key delay)
    (sleep (or delay (response-read-delay proxy)))
    (let ((filename (response-filename proxy)))
      (if (response-file-exists filename)
	  (let* ((raw-contents (read-response-file filename)))
	    (delete-file (pathname filename))
	    (if (and (expected-message-p raw-contents message)
		     (is-not-error raw-contents))
		(values (strip-contents-header raw-contents) raw-contents)
	      (values :ERROR raw-contents)))
	(values :ERROR nil)))) )


 
(defun ping (&key (proxy *default-proxy*) delay)
  (let ((message (send proxy "ping")))
    (multiple-value-bind (junk results)
	(read-osc-response proxy message :delay delay)
      (dismiss junk)
      (and (string= ":ACK" (car results))
	   (string= message (final (split-string (second results))))))))
  
(defun panic (&key (proxy *default-proxy*) delay)
    (let* ((message (send proxy "panic")))
      (read-osc-response proxy message :delay delay)))

(defun initialize (&key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "initialize")))
    (read-osc-response proxy message :delay delay)))

(defun refresh (&key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "refresh")))
    (read-osc-response proxy message :delay delay)))

(defun load-configuration (filename &key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "load-configuration" filename)))
    (read-osc-response proxy message :delay delay)))

;; Returns actual ID
(defun new-operator (operator-type id &key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "new-operator" (list operator-type id))))
    (multiple-value-bind (id contents)
	(read-osc-response proxy message :delay delay)
      (if (eq id :ERROR)
	  (values :ERROR contents)
	(values (nth 5 contents) contents)))))

(defun delete-operator (id &key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "delete-operator" (list id))))
    (multiple-value-bind (id contents)
	(read-osc-response proxy message :delay delay)
      (if (eq id :ERROR)
	  (values :ERROR contents)
	(values (nth 4 contents) contents)))))

(defun connect (parent child &key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "connect" (list parent child))))
    (read-osc-response proxy message :delay delay)))

(defun disconnect (parent child &key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "disconnect" (list parent child))))
    (read-osc-response proxy message :delay delay)))

(defun query-roots (&key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "q-roots")))
    (read-osc-response proxy message :delay delay)))

(defun query-operators (&key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "q-operators")))
    (read-osc-response proxy message :delay delay)))

;; BUGGY
;; (defun query-operator-info (id &key (proxy *default-proxy*) delay)
;;   (let* ((message (send proxy "q-operator-info" id))
;; 	 (results (read-osc-response proxy message :delay delay)))
;;     results))

(defun query-midi-transmitters (&key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "q-midi-transmitters")))
    (read-osc-response proxy message :delay delay)))

(defun query-midi-receivers (&key (proxy *default-proxy*) delay)
  (let* ((message (send proxy "q-midi-receivers")))
    (read-osc-response proxy message :delay delay)))
