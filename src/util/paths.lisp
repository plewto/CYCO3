;;;; CYCO util paths.lisp
;;;;
;;;; Defines functions for manipulating file system paths.
;;;; ISSUE: This file flaunts the idiomatic Lisp approach to pathnames.
;;;;

(in-package :cyco)



;; Set path parameters for current platform.
;;
;; BUG 0009 PORTABILITY
;; Testing shows SBCL and CLISP versions of (software-type) are
;; not consistent.  SBCL returns a simple one-word 'Linux', CLISP
;; returns much more.  Under posix systems this should not
;; pose any problems.
;;
;; See possible solution see:
;; https://stackoverflow.com/questions/4372568/how-can-i-determine-the-operating-system-and-hostname-using-common-lisp  
;; 
(let ((platform (software-type)))
  (cond ((string= platform "Linux")
	 (setf *os-path-root* "/"
	       *os-path-separator* #\/
	       *os-extension-separator* #\.
	       *os-homedir-alias* #\~))
	(t (let ((msg1 (sformat  "Platform ~A not currently supported." platform))
		 (msg2 "Using default (Linux) path parameters."))
	     (cyco-warning msg1 msg2)))))

(defun absolute-path-p (namestring)
  "Predicate, true if namestring indicates an absolute file name."
  (starts-with-p (->string namestring) *os-path-root*))


(defun user-home ()
  "Returns path to user's home directory."
  (let ((dir (->string (user-homedir-pathname))))
    (subseq dir 0 (1- (length dir)))))

;; (defun resolve-user-home (str)
;;   "(resolve-user-home '~/foo') -> <user-home>/foo"
;;   (string-replace (->string *os-homedir-alias*) (user-home) str))

(defun resolve-user-home (str)
  (if (and (plusp (length str))(string= (subseq str 0 1) *os-homedir-alias*))
      (str+ (user-home) (subseq str 1))
    str))


;; (alias file-exists probe-file)  ;; DEPRECIATED -fails under Armed Bear.

(defun partition-path (fname)
  "Partition path namestring into components
(partition-path '/a/b/c') --> ('a' 'b' 'c')"
  (split-string (namestring fname) *os-path-separator*))



(defun split-path (fname)
  "Split path namestring into parent and file components.
(split-path '/a/b/c') --> ('/a/b' 'c')"
  (cond ((zerop (length fname)) nil)
	((string= fname *os-path-root*)(list *os-path-root*))
	(t (let* ((fn (namestring fname))
		  (pos (position *os-path-separator* fn
				 :test #'string= :from-end t)))
	     (if pos
		 (list (resolve-user-home (subseq fn 0 pos))
		       (resolve-user-home (subseq fn (1+ pos))))
	       (list fn nil))))))
   
(defun path-parent (fname)
  "Returns parent path namestring.
(path-parent 'foo/bar/baz') --> 'foo/bar/'"
  (let* ((fn (string-right-trim (->string *os-path-separator*)
				(->string fname)))
	 (pos (position *os-path-separator* fn :from-end t :test #'char=)))
    (if pos
	(resolve-user-home (sformat  "~A~A" (subseq fn 0 pos) *os-path-separator*))
      (->string *os-path-root*))))

(defun join-path-list (lst &optional as-file)
  "Concatenate list of path components to path namestring.
If as-file is true treat final component as a file and not directory.
(join-path-list (list 'a' 'b' 'c')) --> 'a/b/c/'
(join-path-list (list 'a' 'b' 'c') :as-file) --> 'a/b/c'"
  (let ((delim (->string *os-path-separator*))
	(acc ""))
    (dolist (a lst)
      (setf acc (str+ acc (sformat  "~A~A" (string-trim delim a) delim))))
    (if as-file (setf acc (butfinal acc)))
    (if (absolute-path-p (car lst))
        (sformat  "~A~A" *os-path-root* acc)
      (resolve-user-home acc))))

(defun join-path (&rest args)
  "Same as join-path-list but takes arbitrary path components instead of list.
If final argument is :as-file treat result as regular file and not directory.
(join-path 'a' 'b' 'c') --> 'a/b/c/'
(join-path 'a' 'b' 'c' :as-file) --> 'a/b/c'"
  (if (eq (final args) :as-file)
      (join-path-list (butfinal args) :as-file)
    (join-path-list args)))




(defun append-filename-extension (fname extension)
  "Appends extension to file name.
Do not append if filename already ends with extension.

(append-file-extension 'foo' '.bar')    --> 'foo.bar'
(append-file-extension 'foo.bar' 'bar') --> 'foo.bar'
(append-file-extension 'foo.bar' 'baz') --> 'foo.bar.baz'"
  (let* ((flen (length fname))
	 (extlen (length extension))
	 (tail (if (< flen extlen)
		   ""
		 (reverse (subseq (reverse fname) 0 extlen)))))
    (if (not (string= tail extension))
	(str+ fname extension)
      fname)))

;; Split extension from filename
;; (split-extension "foo.bar") --> ("foo" ".bar")
;; (split-extension "foobar")  --> ("foobar" "")
;;
(defun split-extension (fname)
  (let ((index (search "." fname :from-end t)))
    (if index
	(let ((head (subseq fname 0 index))
	      (tail (subseq fname index)))
	  (list head tail))
      (list fname ""))))


