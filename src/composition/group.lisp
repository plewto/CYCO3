;;;; CYCO composition
;;;;
;;;;  Defines group object for muting collections of parts.
;;;;
;;;; Syntax within section context:
;;;;
;;;;   (group name  [parts....])
;;;;
;;;; Functions on groups/parts:
;;;;
;;;;    (mute part-or-group  [:mute :unmute :solo nil])
;;;;    (unmute part-or-group)
;;;;    (solo part-or-group)
;;;;
;;;;

(in-package :cyco)

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
    :type cyco-node
    :accessor parent
    :initform +NULL-NODE+
    :initarg :parent)
   (member
    :type list ;; list of parts
    :accessor group-members
    :initform '()
    :initarg :members))
  (:documentation
   "A Group is an auxiliary object used in the context of a Section to 
combine related parts.  Members of a group may be muted or soloed in
tandem."))


(defmethod group-p ((object null)) nil)
(defmethod group-p ((group group)) t)

(defmethod mute ((group group) &optional state)
  "Change mute state of all group members."
  (cond ((eq state :mute)
	 (setf (mute-state group) :mute)
	 (dolist (part (group-members group))
	   (mute part :mute)))
	((eq state :unmute)
	 (setf (mute-state group) :unmute)
	 (dolist (part (group-members group))
	   (unmute part)))
	((eq state :solo)
	 (setf (mute-state group) :solo)
	 (dolist (g (property (parent group) :groups))
	   (if (not (eq g group))
	       (mute g :mute)))
	 (dolist (part (group-members group))
	   (unmute part)))
	
	(t nil)))

(defmethod unmute ((group group))
  (mute group :unmute))

(defmethod solo ((group group))
  (mute group :solo))

(defmethod muted-p ((group group))
  (let ((s (mute-state group)))
    (eq s :mute)))

(flet ((find-parts (function-name group-name section part-name-list)
		   (let ((parts-list '()))
		     (dolist (part-name part-name-list)
		       (let ((part (find-child section part-name)))
			 (if part
			     (progn
			       (push part parts-list)
			       (put part :group group-name))
			   (cyco-composition-error function-name
						   (sformat "Section Name : ~A" (name section))
						   (sformat "Part ~A does not exists" part-name)))))
		     parts-list)) )

  (defun make-group (name &key member-names section)
    "Creates new instance of Group.  
name - symbol
:member-names - list of symbols.  Each symbol must be the name of a Part 
within in the section.
:section - Parent section, defaults to current section of *project*."
    (let ((sec (or section (and (project-p *project*)
				(property *project* :current-section)))))
      (if (not sec)
	  (cyco-composition-error
	   'make-group
	   ;;(name (or section "current-section")))
	   "No default section")
	(let* ((members (find-parts 'make-group name sec
				    (->list member-names)))
	       (group (make-instance 'group
				  :name name
				  :parent sec
				  :members members)))
	  (add-group sec group)
	  group))))

  (defmacro group (name member-names)
    "Same as make-group except binds the new group to the symbol name."
   `(let ((group (make-group ',name :member-names ,member-names)))
      (defparameter ,name group)
      group)) )


(defmethod clone ((mother group) &key new-name new-parent)
  (dismiss new-name new-parent)
  (cyco-error
   "Cloning of groups not supported."
   (sformat "Can not clone group ~A" (name mother)))
  mother)
