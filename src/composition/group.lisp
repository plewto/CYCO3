;;;; CYCO3 src/composition/group
;;;;
;;;; syntax within section context
;;;;
;;;;   (group name  [parts....])
;;;;
;;;; Functions on groups/parts
;;;;
;;;;    (mute obj  [:mute :unmute :solo nil])
;;;;    (unmute obj)
;;;;    (solo obj)
;;;;

(defclass group nil
  ((name
    :type symbol
    :accessor name
    :initform 'no-name
    :initarg :name)
   (state
    :type symbol
    :accessor mute-state
    :initform nil)
   (parent-section
    :type section
    :accessor parent
    :initform nil
    :initarg :parent)
   (member
    :type list ;; list of parts
    :accessor group-members
    :initform '()
    :initarg :members)))

(defmethod group-p ((obj null)) nil)
(defmethod group-p ((grp group)) t)

(defmethod mute ((grp group) &optional state)
  (cond ((eq state :mute)
	 (setf (mute-state grp) :mute)
	 (dolist (prt (group-members grp))
	   (mute prt :mute)))
	((eq state :unmute)
	 (setf (mute-state grp) :unmute)
	 (dolist (prt (group-members grp))
	   (unmute prt)))
	
	((eq state :solo)
	 (setf (mute-state grp) :solo)
	 (dolist (g (property (parent grp) :groups))
	   (if (not (eq g grp))
	       (mute g :mute)))
	 (dolist (prt (group-members grp))
	   (unmute prt)))
	
	(t nil)))

(defmethod unmute ((grp group))
  (mute grp :unmute))

(defmethod solo ((grp group))
  (mute grp :solo))

(defmethod muted-p ((grp group))
  (let ((s (mute-state grp)))
    (eq s :mute)))

(flet ((find-parts (function-name group-name section part-name-list)
		   (let ((acc '()))
		     (dolist (part-name part-name-list)
		       (let ((prt (find-child section part-name)))
			 (if prt
			     (progn
			       (push prt acc)
			       (put prt :group group-name))
			   (cyco-composition-error function-name
						   (sformat "Section Name : ~A" (name section))
						   (sformat "Part ~A does not exists" part-name)))))
		     acc)) )

  (defun make-group (name &key member-names section)
    (let ((sec (or section (and (project-p *project*)
				(property *project* :current-section)))))
      (if (not sec)
	  (cyco-composition-error
	   'make-group
	   ;;(name (or section "current-section")))
	   "No default section")
	(let* ((members (find-parts 'make-group name sec
				    (->list member-names)))
	       (grp (make-instance 'group
				  :name name
				  :parent sec
				  :members members)))
	  (add-group sec grp)
	  grp))))

  (defmacro group (name member-names)
   `(let ((grp (make-group ',name :member-names ,member-names)))
      (defparameter ,name grp)
      grp)) )


;;; Group clone is handled by Section clone.
;;;
;; (defmethod clone ((grp group) &key new-name new-parent)
;;   (dismiss grp new-name new-parent)
;;   grp)

