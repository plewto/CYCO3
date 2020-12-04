;;;; CYCO parts expect.lisp
;;;;
;;;; Utilities for parsing part event list
;;;;

(in-package :cyco-part)

(defun validate-section (part-name section)
  (or (and (section-p section) section)
      (and (project-p *project*)
	   (property *project* :current-section))
      (cyco-composition-error
       (sformat "No default project or section while creating part ~A" part-name))))

(defun part-id (part)
  (sformat "Section ~A  Part ~A" (name (parent part))(name part)))

(defun clause-id (event clause)
  (sformat "Event  ~A~%Clause  ~A" event clause))


;; clause (command ....)
;; returns command or nil
;;
(defun validate-argument-count (part event clause count-table)
  (let* ((command (car clause))
	 (count (1- (length clause)))
	 (expected-count (gethash command count-table)))
    (if (not expected-count)
	(progn 
	  (cyco-composition-error
	   (part-id part)
	   (clause-id event clause)
	   (sformat "Invalid command ~A" command))
	  (return-from validate-argument-count nil)))
    (if (not (= count expected-count))
	(progn
	  (cyco-composition-error
	   (part-id part)
	   (clause-id event clause)
	   (sformat "Expected ~A argument~A" expected-count (if (= expected-count 1) "" "s"))
	   (sformat "Encountered ~A" count))
	  nil)
      command)))

(let ((false (->cyco-symbol 'no))
      (true (->cyco-symbol 'yes)))
  (defun expect-yes-no (part event clause &key (position 1)(default false))
    (let ((value (nth position clause)))
      (cond ((or (eq value false)(eq value nil))
	     (return-from expect-yes-no nil))
	    ((or (eq value true)(eq value t))
	     (return-from expect-yes-no t))
	    (t
	     (cyco-composition-error
	      (part-id part)
	      (clause-id event clause)
	      "Expected either 'YES or 'NO"
	      (sformat "Encountered  ~A" value)
	      (sformat "Using default ~A" default))
	     (if (eq default true) t nil))))))

(defun expect-integer (part event clause &key (position 1)(min -1e9)(max +1e9)(default 0))
  (let ((value (nth position clause)))
    (or (and (integerp value)
	     (<= min value)
	     (<= value max)
	     value)
	(progn
	  (cyco-composition-error
	   (part-id part)
	   (clause-id event clause)
	   (sformat "Expected integer between ~A and ~A" min max)
	   (sformat "Encountered  ~A" value)
	   (sformat "Using default ~A" default))
	  default))))

(defun expect-float (part event clause &key (position 1)(min -1e9)(max +1e9)(default 0.0))
  (let ((value (nth position clause)))
    (or (and (numberp value)
	     (<= min value)
	     (<= value max)
	     (float value))
	(progn
	  (cyco-composition-error
	   (part-id part)
	   (clause-id event clause)
	   (sformat "Expected float between ~A and ~A" min max)
	   (sformat "Encountered  ~A" value)
	   (sformat "Using default ~A" default))
	  (float default)))))

(defun expect-normalized-float (part event clause &key (position 1)(signed nil))
  (expect-float part event clause :position position :min (if signed -1 0) :max 1 :default 0))

(defun expect-metric-expression (part event clause &key (position 1)(default 'q))
  (let ((expression (nth position clause)))
    (if (not (metric-expression-p expression))
	(progn
	  (cyco-composition-error
	   (part-id part)
	   (clause-id event clause)
	   "Expected metric-expression"
	   (sformat "Encounterd ~A" expression)
	   (sformat "Using default ~A" default))
	  (metric-expression default))
      expression)))
	   
(defun expect-dynamic (part event clause &key (position 1)(default 0.5))
  (let ((values (->list (nth position clause))))
    (if (not (every #'dynamic-p values))
	(progn
	  (cyco-composition-error
	   (part-id part)
	   (clause-id event clause)
	   "Expected dynamic values"
	   (sformat "Encountered ~A" values)
	   (sformat "Using default ~A" default))
	  (->list default))
      (dynamic values))))
