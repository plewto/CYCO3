;;;; CYCO
;;;;
;;;;  cyco-node
;;;;    |
;;;;    +-- time-signature
;;;;    |     |
;;;;    |     +-- project
;;;;    |     |
;;;;    |     +-- section
;;;;    |     |    |
;;;;    |     |    +-- countin
;;;;    |     |    +-- endpad
;;;;    |     |
;;;;    |     +-- part
;;;;    |          |
;;;;    |          +-- control-ball
;;;;    |          +-- control-ghost
;;;;    |          +-- controllers
;;;;    |          +-- key-ghost
;;;;    |          +-- metronome
;;;;    |          +-- programs
;;;;    |          +-- qball
;;;;    |          +-- raw-part
;;;;    |          +-- strummer
;;;;    |     
;;;;    +-- instrument
;;;;

(in-package :cyco)

(defclass cyco-node ()
  ((name
    :type symbol
    :accessor name
    :initform :noname
    :initarg :name)
   (remarks
    :type string
    :accessor remarks
    :initform ""
    :initarg :remarks)
   (parent
    :type t
    :accessor parent
    :initform nil
    :initarg :parent)
   (children
    :type list
    :accessor children
    :initform '())
   (transient
    :type t
    :accessor transientp
    :initform t
    :initarg :transient)
   (property-keys
    :type list
    :accessor property-keys
    :initform '()
    :initarg :properties)
   (property-table
    :type hash-table
    :accessor property-table
    :initform (make-hash-table)))
  (:documentation
   "CYCO-NODE provides a hierarchal linking of objects into trees.
Each node has at most one parent and any number of child nodes.
Each node type defines a prescribed set of properties in the form of
key/value pairs.  Nodes inherit property values from their parent but may 
define values for themselves, this shadowing the parent'a value.

It is an error to assign a value to a non-prescribed property.

The put and property methods assigns and retrieve property values 
respectively.

(put node key value) --> Assigns value to property key.
(property node key)  --> Returns value of property key.

The properties method returns a list of all prescribed property keys.

Nodes are marked as 'transient' or 'non-transient'.  Transient nodes
are removed from a tree by the prune method while non-transient nodes are
not."))

    
(defmethod set-name ((node cyco-node)(new-name symbol))
  (setf (name node) new-name))

(defmethod set-remarks ((node cyco-node)(text string))
  (setf (remarks node) text))

(defmethod root-p ((node cyco-node))
  (not (parent node)))

(defmethod leaf-p ((node cyco-node))
  (null (children node)))

(defmethod child-of-p ((parent cyco-node)(child cyco-node))
  (eq parent (parent child)))

(defmethod path-to-root ((n null)) nil)

(defmethod path-to-root ((node cyco-node))
  (cons node (path-to-root (parent node))))

(defmethod disconnect ((child cyco-node))
  (if (not (root-p child))
      (let ((p (parent child)))
	(setf (children p)(remove child (children p)))
	(setf (parent child) nil))))

(defmethod connect ((parent cyco-node)(child cyco-node))
  (if (not (child-of-p parent child))
      (progn
	(disconnect child)
	(setf (parent child) parent)
	(setf (children parent)
	      (cons child (children parent))))))

(defmethod prune ((node cyco-node) &optional force)
  (dolist (child (children node))
    (if (or force (transientp child))
	(disconnect child))
    (prune child force)))

(defmethod has-property-p ((n null)(key symbol)) nil)

(defmethod has-property-p ((node cyco-node)(key symbol))
  (bool (or (member key (property-keys node))
	    (has-property-p (parent node) key))))

(defmethod put ((node cyco-node)(key t)(_ t))
  (declare (ignore _))
  (cyco-type-error 'put 'symbol key))

(labels ((assert-valid-property (node key)
			      (if (has-property-p node key)
				  t
				(progn 
				  (cyco-error
				   (sformat "CYCO-NODE ~A does not have ~A property" (name node) key))
				  nil)))
	 (find-property-value (node key)
		    (if node
			(or (gethash key (property-table node))
			    (and (not (root-p node))
				 (find-property-value (parent node) key)))
		      nil)))

  (defmethod put ((node cyco-node)(key symbol)(value t))
    (if (assert-valid-property node key)
	(setf (gethash key (property-table node)) value)))
  
  (defmethod property ((node cyco-node)(key symbol))
    (if (assert-valid-property node key)
	(find-property-value node key))))

(defmethod properties ((n null) &optional acc) acc)

(defmethod properties ((node cyco-node) &optional _acc)
  (remove-duplicates (append
		      _acc
		      (property-keys node)
		      (properties (parent node) _acc))))

(defmethod local-properties ((node cyco-node))
  (let ((acc '()))
    (maphash #'(lambda (key value)(push (cons key value) acc))
	     (property-table node))
    acc))

(defmethod print-tree ((node cyco-node) &optional (depth 0))
  (format t "~A" (spaces (* 4 depth)))
  (format t "~A~%" (name node))
  (dolist (c (children node))
    (print-tree c (1+ depth))))
  
(defmethod clone ((mother cyco-node) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (cyco-type-error 'clone '?cyco-node mother))
  
(defmethod ->string ((n cyco-node))
  (sformat "~A name: ~A" (type-of n)(name n)))

(constant +NULL-NODE+ (make-instance 'cyco-node))

(let ((found nil))
  (labels ((-find-child (parent child-name)
			(setf found (or found
					(find child-name (children parent)
					      :test #'(lambda (a b)(eq a (name b))))))
			(if (null found)
			    (dolist (c (children parent))
			      (-find-child c child-name)))))

	  (defmethod find-child ((parent cyco-node)(child-name symbol))
	    (setf found nil)
	    (-find-child parent child-name)
	    found)

	  (defmethod find-child ((parent cyco-node)(child cyco-node))
	    (and (member child (children parent)) child))))
