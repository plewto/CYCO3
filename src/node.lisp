;;;; CYCO3 src/node
;;;;
;;;;  cyco-node
;;;;    |
;;;;    +-- time-signature
;;;;    |     |
;;;;    |     +-- project
;;;;    |     |
;;;;    |     +-- section
;;;;    |     |    |
;;;;    |     |    +-- preroll
;;;;    |     |    +-- final
;;;;    |     |
;;;;    |     +-- part
;;;;    |          |
;;;;    |          +-- programs leaf
;;;;    |          +-- raw-part leaf
;;;;    |          +-- epart 
;;;;    |          +-- cpart
;;;;    |          +-- qball
;;;;    |     
;;;;    +-- instrument
;;;;

(defgeneric root-p (obj))
(defgeneric child-of-p (parent child))
(defgeneric path-to-root (obj))
(defgeneric disconnect (child))
(defgeneric connect (parent child))
(defgeneric prune (node &optional force))
(defgeneric hah-property-p (node key))
(defgeneric put (obj key value))
(defgeneric property* (obj key))  ;; private - no key validity test
(defgeneric property (obj key))
(defgeneric properties (obj &optional acc))
(defgeneric local-properties (obj))
(defgeneric print-tree (node &optional depth))
(defgeneric find-child (parent child))   ;; nil -> child not found

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
    :type hashtable
    :accessor property-table
    :initform (make-hash-table))))
    
(defmethod name! ((node cyco-node)(new-name symbol))
  (setf (name node) new-name))

(defmethod remarks! ((node cyco-node)(text string))
  (setf (remarks node) text))

(defmethod root-p ((node cyco-node))
  (not (parent node)))

(defmethod child-of-p ((parent cyco-node)(child cyco-node))
  (eq parent (parent child)))

(defmethod find-child ((parent cyco-node)(child cyco-node))
  (find child (children parent)))

(defmethod find-child ((parent cyco-node)(child-name symbol))
  (find child-name (children parent) :test #'(lambda (a b)(eq a (name b)))))

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
  (dolist (c (children node))
    (if (or force (transientp c))
	(progn
	  (disconnect c)
	  (prune c)))))

(defmethod hah-property-p ((n null)(key symbol)) nil)

(defmethod hah-property-p ((node cyco-node)(key symbol))
  (bool (or (member key (property-keys node))
	    (hah-property-p (parent node) key))))

(defmethod put ((node cyco-node)(key t)(_ t))
  (dismiss _)
  (cyco-type-error 'put 'symbol key))

(flet ((assert-valid-property (node key)
	(if (hah-property-p node key)
	    t
	  (progn 
	    ;;(cyco-property-error node key)
	    (cyco-error
	     (sformat "CYCO-NODE ~A does not have ~A property" (name node) key))
	    nil))))

    (defmethod put ((node cyco-node)(key symbol)(value t))
      (if (assert-valid-property node key)
	  (setf (gethash key (property-table node)) value)))
   
    (defmethod property* ((n null)(key symbol)) nil)

    (defmethod property* ((node cyco-node)(key symbol))
      (or (gethash key (property-table node))
	  (property* (parent node) key)))

    (defmethod property ((node cyco-node)(key symbol))
      (if (assert-valid-property node key)
	  (property* node key))))

(defmethod properties ((n null) &optional acc) acc)

(defmethod properties ((node cyco-node) &optional acc)
  (append
   acc
   (property-keys node)
   (properties (parent node) acc)))

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
  
(defmethod clone ((node cyco-node) &key new-name new-parent)
  (dismiss new-name new-parent)
  ;;(cyco-not-implemented-error 'clone node))
  (cyco-type-error 'clone '?cyco-node node))
  
(defmethod ->string ((n cyco-node))
  (sformat "~A name: ~A" (type-of n)(name n)))


(defmethod ? ((n cyco-node))
  (format t "~A~%" (type-of n))
  (format t "  name       : ~A~%" (name n))
  (format t "  remarks    : ~A~%" (remarks n))
  (format t "  parent     : ")
  (if (root-p n)
      (format t "<root>~%")
    (format t "~A~%" (name (parent n))))
  (format t "  children   : ")
  (if (zerop (length (children n)))
      (format t "None~%")
    (progn
      (format t "~%")
      (dolist (c (children n))
	(format t "      ~12A ~A~%" (type-of c)(name c)))))
  (format t "  transient  : ~A~%" (transientp n))
  (format t "  properties :~%")
  (dolist (k (property-keys n))
    (format t "      [~24A] --> ~A~%" k (property n k))))
  

