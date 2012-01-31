;;
;;  Copyright (C) 31-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :expression-scan
  (:use :common-lisp :alexandria :j-general :denest
	:expression-hook)
  (:nicknames :expr-scan)
  (:export
 ;Functions to scan things.
   scan-expr scan
 ;The hooks being scanned with
   scan-expression-hook scan-macro-hook
 ;Defining scanners.
   add-scanner-fun add-scanner *otherwise*
   def-scanner expr *additional-scan* *forms-for*
 ;Accessing information.
   access-result *package-list* list-packages-below-path 
   *scan-result*
 ;Some of the classes used to put scan data in.
   base-track typed-track depend-track
   track-fun track-var
   track-form track-generic track-method track-class
   track-package track-system
 ;Getting data out of that, or attaching extra info.
   track-item track-extra)
  (:documentation "Can create and use expression-hook to obtain\
 information about code. Any s-expression can be tracked.
 (So macros and functions can be tracked.)"))

(in-package :expression-scan)

(defvar *overrider* nil "If a scanner is overriding sub-scanners.")

;;Current file.
(defvar *cur-file* nil
  "Current file being scanned.")
(defvar *cur-path* nil
  "Current path being scanned.")

(defun cur-path ()
  "Attempts to get current file being scanned."
  (or (when (and *cur-path* *cur-file*)
	(concatenate 'string (namestring *cur-path*) 
		     (file-namestring *cur-file*)))
      *cur-path* *cur-file*))

;;Some vars
(defvar *cur-package* nil)

(defvar *reading-myself* nil) ;;TODO unclear.

;;TODO chuck below?
(defgeneric name (obj) (:documentation "Gets name of object."))
(defmethod name ((null null)))

;;Scanners
(defvar *scanned-forms* (list) "Names of the scanned forms.")

(defvar *fun-scan* (make-hash-table)
  "Scanners for the different macros/functions under observation.")

(defvar *otherwise* #'identity "What to do if no scanner matches")

(defparameter *additional-scan* (list)
  "List of functions for scanning besides every expression.\
 (Rather then fun-scan, elements of which only scan specific macros.")

(defun add-scanner-fun (for-name scan-function)
  "Adds a scanner for a macro/function."
  (declare (type function scan-function))
  (push for-name *scanned-forms*)
  (setf (gethash for-name *fun-scan*) scan-function))

(defmacro def-scanner (for-name (&rest args) &body body)
  "Add-scanner-fun, but does the creation of function for you.
Note that the variable expr contains the whole expression."
  `(flet ((scanning-fun (expr)
	    ,@(when (stringp (car body)) (list(car body)))
	    (destructuring-bind (,@args) (cdr expr)
		,@body)))
     (add-scanner-fun ',for-name #'scanning-fun)))
	      
(defvar *scan-result* (make-hash-table)
  "Hash table, first by-function/macro name, then 'instance' name.")

;;Accessing result.
(defun access-result (fun-name &optional name)
  (typecase fun-name
    (null)
    (list (dolist (f-name fun-name)
	    (when-let ((result (access-result f-name name)))
	      (return result))))
    (t    (when-let (hash (gethash fun-name *scan-result*))
	    (if name (gethash name hash) hash)))))

(defun (setf access-result) (to fun-name name)
  "See non-setf version. Be warned, will register with the package too!"
  (when *cur-package*
    (pushnew to (slot-value *cur-package* 'scan-list) :test #'equalp))
  (let ((hash (or (gethash fun-name *scan-result*)
		  (setf (gethash fun-name *scan-result*) 
			(make-hash-table :test 'equalp)))))
    (setf (gethash name hash) to)))

(defun list-packages-below-path (path)
  "Lists all the packages that are in the path or in subdirectories thereof."
  (remove-if-not
   (lambda (track)
     (with-slots (paths) track
       (find-if (compose (curry #'sub-path-of path) #'namestring) paths)))
   (copy-list *package-list*)))

(defun scan-expression-hook (expr)
  "The expression hook of the scanner."
  (mapc (rcurry #'funcall expr) *additional-scan*)
  ;See if any trackers for it.
  (funcall (or (and expr (listp expr) (gethash (car expr) *fun-scan*))
	       *otherwise*)
	   expr)    
  (expand-hook expr)) ;Expand the expression.

(defun scan-macrohook (expander form env)
  "Function for in *macroexpand-hook*, doesn't make a nearly as complete\
 scan, but should work with regular loading."
  (when-let (fn (gethash (car form) *fun-scan*))
    (funcall fn form))
  (funcall expander form env))

(defun check-symbol (symbol)
  "Checks if the symbol needs to be added as dependency of the package."
  (denest
   (when (and symbol (symbolp symbol) *cur-package*))
   (when-let ((pkg (symbol-package symbol))))
   (let ((pkg (intern (package-name pkg) :keyword))))
   (with-slots (uses also-uses) *cur-package*)
   (unless (or (find pkg *ignore-packages*) (find pkg uses)
	       (eql pkg :keyword))
     (pushnew pkg also-uses))))

(defun check-top (top)
  (typecase top
    (symbol (check-symbol top))
    (list   (check-top (car top))
	    (check-top (cdr top)))))

;;TODO need a separate package for scanning files and keeping track of current
;; file, line numbers. Preferably a class here that automatically stores them.

(defun scan-stream (from)
  (let ((expr-hook::*discontinue* nil)
	(*reading-myself* t))
    (do ((read nil (read from nil 'end-of-file)))
	((or (eql read 'end-of-file)
	     expr-hook::*discontinue*) nil)
      (check-top read)
      (expand read))))

(defun scan-file (from &key (load-first t))
  (let*((*cur-file* from)
	(*cur-path* (directory-namestring *cur-file*))
	(*default-pathname-defaults* (pathname *cur-path*)))
    (when load-first ;TODO this causes problems?
      (load from))
    (with-open-file (stream from)
      (scan-stream stream))))

;;Scanning stuff.
(defun scan (from ;This aught to be default, right?
	     &key (*package* (find-package :cl-user))
	     (*expression-hook* #'scan-expression-hook)
	     load-first (judge (constantly t))
	     (depth 0)) ;TODO recursive.(Scan dependencies)
  "Scans a file as source code in order to document it.
'From' can be a string, pathname, stream, keyword(tries find system) or a\
 list of those. If it is a directory, it will read all the '.lisp' files in\
 it."
  (labels ((sf (from depth &key (load-first load-first))
	     (typecase from
	       ((or string pathname);Everything .lisp in directories.
		(let*((from (namestring from))
		      (from
		       (if (char= (aref from 0) #\/) from
			 (concatenate 'string 
			   (namestring *default-pathname-defaults*) from))))
		  (if (cl-fad:directory-pathname-p from) 
		    (dolist (file (cl-fad:list-directory from))
		      (when (funcall judge file)
			(cond
			  ((cl-fad:directory-pathname-p file)
			   (when (> depth 0) 
			     (sf file (- depth 1))))
			  ((case (aref (file-namestring file) 0)
			     ((#\. #\#) nil)
			     (t         (string= (file-extension file)
						 ".lisp")))
			   (sf file depth)))))
		    (scan-file from :load-first load-first))))
	       (stream ;;Make a `scan-stream`?
		(scan-stream from))
	       (keyword 
		(when-let (sys (asdf:find-system from)) ;Take as system.
		  ;Otherwise it doesnt know what it is.
		  (let ((*package* (find-package :asdf))) 
		    (when load-first (asdf:oos 'asdf:load-op from))
		    (sf (slot-value sys 'asdf:source-file) depth
			:load-first nil))))
	       (list
		(dolist (f from) (sf f depth))))))
    (sf from depth))
  (values)) ;TODO anything useful to return?

(defun scan-expr (expr)
  "Scans a single expression."
  (let ((*expression-hook* #'scan-expression-hook)
	(*in-funs* nil))
    (check-top expr)
    (expand expr)))

(defclass pre-track () ((extra :initform nil)))

;;Some aspects of tracking.
(defclass base-track (pre-track) ;TODO first two useful for?
  ((keywords :initarg :keywords :initform nil :type list) 
   (overrider :initform nil :initarg :overrider)
   (path :initform (cur-path) :initarg :part)
   (from-package :initform *cur-package*
     :documentation "Package it originates from, in case that isn't clear\
 from expression.")))

(defgeneric track-item (track item-name)
  (:documentation "Get info out of a tracked macro.")
  (:method ((track base-track) (name (eql :def)))
    'base-track))

(defun track-extra (track extra-name)
  "Get/set extra info of a tracked macro."
  (getf (slot-value track 'extra) extra-name))
(defun (setf track-extra) (to track extra-name)
  "Get/set extra info of a tracked macro."
  (setf (getf (slot-value track 'extra) extra-name) to))

(defmacro slot-value-track-item 
    (class slot &optional (key (intern (symbol-name slot) :keyword)))
  (with-gensyms (track item) 
    `(defmethod track-item ((,track ,class) (,item (eql ,key)))
       (slot-value ,track ',slot))))

(slot-value-track-item base-track keywords)
(slot-value-track-item base-track path)
(slot-value-track-item base-track from-package)

(defclass track-form (base-track)
  ((form :initarg :form :type list)))

(defun track-form (&rest form-names)
  "Tracks the form, for `*otherwise*` or"
  (lambda (expr) 
    (when (find (car expr) form-names) 
      (setf (access-result (car expr) (cadr expr)) expr))))

(defmethod track-item ((track track-form) (item (eql :def)))
  (car(slot-value track 'form)))
(defmethod track-item ((track track-form) (item (eql :name)))
  (cadr(slot-value track 'form)))

(defclass typed-track ()
  ((type :initarg :type :initform nil)))

(slot-value-track-item typed-track type)

(defclass depend-track ()
  ((fun-dep :initarg :fun-dep :initform nil :type list
     :documentation "What functions/macros it depends on.")
;   (flet-dep :initarg :flet-dep :initform nil :type list
;     :documentation "Flet/macrolets it depends on.")
   (var-dep :initarg :var-dep :initform nil :type list
     :documentation "Assoc list with variables it depends on, and origin.")))

(slot-value-track-item depend-track fun-dep)
(slot-value-track-item depend-track var-dep)

(defun form-scanner (expr &key (class 'track-form))
  (setf (access-result (car expr) (cadr expr))
	(make-instance class :form expr))
  (expand-hook expr))

;;Package stuff.
(defvar *package-list* nil
  "List of all found packages.")

(defparameter *ignore-packages* 
  (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel :sb-loop 
	:common-lisp-user :cl-user)
  "Packages to ignore because they're boring or they're part of specific CL\
 implementation.(TODO sbcl specific right now!")

(defvar *couldnt-find-package* nil "List of packages not found.")

(def-scanner in-package (name)
  (let ((name (package-keyword name)))
    (setq *cur-package* (access-result 'defpackage name))
    (let ((package (find-package name)))
      (cond (package
	     (setq *package* package))
	    (t 
	     (warn "Couldn't find package for ~a. It might not have been\
 loaded. (May want to check the order in which you are loading the files)
Discontinued scan." name)
	     (pushnew name *couldnt-find-package*)
	     (setq expr-hook::*discontinue* t))))
;TODO we know the package should exist.
    (when-let (tracker (access-result 'defpackage 
			 (intern (princ-to-string  name) :keyword)))
      (pushnew (cur-path)
	       (slot-value tracker 'paths) :test 'equalp)))
  expr)

(defclass track-package (track-form)
  ((paths :initarg :parbts :initform nil :type list
     :documentation "Paths where '(in-package' was found.")
   (uses :initform nil :type list :initarg :uses)
   (also-uses :initform nil :type list
     :documentation "Packages it uses by specific access.")
   (scan-list :initform nil :type list
     :documentation "List of things scanned inside the package.")))

(slot-value-track-item track-package paths)
(slot-value-track-item track-package uses)
(slot-value-track-item track-package also-uses)
(slot-value-track-item track-package scan-list)

(def-scanner defpackage (name &rest rest)
  (let*((name (package-keyword name))
	(tracker
	 (make-instance 'track-package :form `(defpackage ,name ,@rest)
	   :uses (mapcar (lambda (name) 
			   (intern (package-name (find-package name)) 
				   :keyword))
			 (cdr(assoc :use rest))))))
    (setf (access-result 'defpackage name) tracker)
    (setq *package-list* ;TODO a special var indicating if it is allowed.
	  (remove-if (lambda (p) (eql name (track-item p :name)))
		     *package-list*))
    (push tracker *package-list*))
  expr)

;;Data tracker for functions and macros.
;; Note the convention is to not track data already readily available, 
;; like documentation strings.rc
(defclass track-fun (base-track depend-track)
  ((kind :initarg :kind :type symbol) ;TODO document.
   (name :initarg :name :type symbol)
   (args :initarg :args :type list)
   
   (in-funs :initarg :in-funs :type list
     :documentation "Which functions/macros created it."))
  (:documentation
   "Structure to contain information on functions and macros."))

(slot-value-track-item track-fun kind :def)
(slot-value-track-item track-fun name)
(slot-value-track-item track-fun args)
(slot-value-track-item track-fun in-funs)

(defun fun-like-scanner (kind name args)
  (let*((fun (make-instance 'track-fun :name name :args args
	       :kind kind :in-funs *in-funs* :overrider *overrider*)))
    (values (if *overrider* *in-funs* (cons fun *in-funs*)) fun)))

(flet ((fun (type name args expr)
	 (multiple-value-bind (*in-funs* fun)
	     (fun-like-scanner type name args)
	   (setf (access-result type name) fun)
	   (expand-hook expr))))
  (def-scanner defun (name args &rest ignore)
    (declare (ignore ignore))
    (fun 'defun name args expr))
  (def-scanner defmacro (name args &rest ignore)
    (declare (ignore ignore))
    (fun 'defmacro name args expr)))

(defun add-dep-to-tracker (tracker expr)
  (denest
   (when tracker)
   (with-slots (var-dep fun-dep) tracker)
   (typecase expr
     (symbol ;Register var/parameter useages. 
      (when (and (not (assoc expr *eh-sym-macs*))
		 (or (access-result 'defvar expr)
		     (access-result 'defparameter expr)))
	(pushnew expr var-dep)))
     (list ;Register function/macro useages.
;External flets and lets don't count. TODO They should..
      (let ((name (car expr)))
	(unless (or (find name *eh-funs*) (find name *eh-macs*))
	  (pushnew name fun-dep)))))))

(defun additional-scanner-fun (expr)
  "Scans for used variables/functions to <TODO lost some documentation>."
  (unless (null expr)
    (dolist (fun *in-funs*)
      (add-dep-to-tracker
       (or (unless (or (listp fun) (symbolp fun))
	     fun) ;TODO all those derived from depend-track?
	   (access-result 'defmacro fun)
	   (access-result 'defun fun)
	   (access-result 'defvar fun)
	   (access-result 'defparameter fun)
	   (access-result 'defgeneric fun))
       expr))))
(push #'additional-scanner-fun *additional-scan*)

(defclass track-generic (track-form typed-track depend-track)
  ((methods :initarg :methods :initform nil :type list)
   (args :initarg :args :type list))
  (:documentation "Tracks method generic declarations."))

(slot-value-track-item track-generic methods)
(slot-value-track-item track-generic args)

(def-scanner defgeneric (name &rest rest)
  (declare (ignore rest))
  ;Make it, if methods already existed, incorporate.
  (setf (access-result 'defgeneric name)
	(make-instance 'track-generic :form expr
	  :methods (when-let (prev (access-result 'defgeneric name))
		     (slot-value prev 'methods))
	  :args (car rest)))
  expr)

(defclass track-method (track-fun)
  ((way :initarg :way :initform nil :type symbol)))

(defmethod track-item ((fun track-method) (item (eql :def)))
  'defmethod)
(slot-value-track-item track-method way)

(def-scanner defmethod (name way/args &optional args/dstr
			     dstr/body &body body)
  "Scans a method. Notably not in a new entry, lists on the defgeneric."
  (multiple-value-bind (args dstr body)
      (cond
	((listp way/args)
	 (values way/args args/dstr (cons dstr/body body)))
	((listp args/dstr)
	 (values args/dstr dstr/body body))
	(t                 (error "")))
    (declare (ignore body))
    (let((gen
	  (or ;Automatically makes a generic if not scanned.
	   (access-result 'defgeneric name)
	   (setf (access-result 'defgeneric name)
		 (make-instance 'track-generic
		   :form `(defgeneric ,name (,@(mapcar #'delist args))
			    ,@(when dstr `((:documentation ,dstr)))))))))
     ;Push the method.
      (multiple-value-bind (*in-funs* fun)
	  (fun-like-scanner 'defmethod name args)
	(push (change-class fun 'track-method
			    :way (when (symbolp way/args) way/args))
	      (slot-value gen 'methods))
	(expand-hook expr)))))

;;Data tracker for variables.
(defclass track-var (track-form typed-track)
  ((fun-dep :initform nil :type list)
   (var-dep :initform nil :type list))
  (:documentation "Structure containing information on macros.
fun-dep and var-dep for initform!"))

(slot-value-track-item track-var fun-dep)
(slot-value-track-item track-var var-dep)

(flet ((var-scanner (expr)
	 "Function to scan variable creation by defvar/defparameter."
	 (destructuring-bind (type name &rest rest) expr
	   (declare (ignore rest))
	   (setf (access-result type name)
		 (make-instance 'track-var :form expr)))
	 (expand-hook expr)))
  (add-scanner-fun 'defvar #'var-scanner)
  (add-scanner-fun 'defparameter #'var-scanner))

(defclass track-class (track-form) ())

(defmethod track-item ((track track-class) (item (eql :doc)))
  (getf (nth 4 (slot-value track 'form)) :doc))
(defmethod track-item ((track track-class) (item (eql :derives-from)))
  (nth 2 (slot-value track 'form)))
(defmethod track-item ((track track-class) (item (eql :slots)))
  (nth 3 (slot-value track 'form)))

(add-scanner-fun 'defclass (rcurry #'form-scanner :class 'track-class))
(add-scanner-fun 'defstruct #'form-scanner)

(def-scanner declaim (&rest args)
  (dolist (a args)
    (case (car a)
      (inline
	(dolist (sym (cdr a))
	  (when-let (result
		     (access-result '(defun defgeneric defmacro) sym))
	    (setf (getf (slot-value result 'keywords) :inline) t))))
      (type
       (dolist (sym (cddr a))
	 (when-let (result (access-result '(defvar defparameter) sym))
	   (setf (slot-value result 'type) (cadr a)))))
      (ftype ;Note: a little code repeat here.
       (dolist (sym (cddr a))
	 (when-let (result (access-result '(defun defgeneric defmacro) sym))
	   (setf (slot-value result 'type) (cadr a)))))))	 
  (expand-hook expr))

;;Scanning asdf systems forms; load the asd file.
 (defvar *follow-asdf-systems* t
  "Whether to follow the files of asdf system that is scanned.
nil: Don't follow
true: Follow, assumes the macros in the files have been executed!
:also-load: If found load it with (asdf:oos 'asdf:load-op system-name).")

(defun scan-asdf-components (components)
  (dolist (component components)
    (destructuring-bind (name value &rest rest) component
      (let ((value (typecase value (symbol (string-downcase value)) 
			           (t      value))))
	(case name
	  (:file
	   (scan (concatenate 'string *cur-path* "/" value ".lisp")))
	  (:module
	   (let ((*cur-path* (concatenate 'string *cur-path* value)))
	     (scan-asdf-components (getf rest :components)))))))))

(defclass track-system (track-form) ())

(def-scanner asdf::defsystem (system-name &rest info)
  (setf (access-result 'asdf:defsystem system-name)
	(make-instance 'track-system :form expr))
  ;If we know where we are, and are to folow:
  (let ((*default-pathname-defaults* 
	 (or (unless *reading-myself*
	       *load-pathname*)
	     *default-pathname-defaults*)))
    (typecase (and (asdf:find-system system-name nil)
		   (or (when (functionp *follow-asdf-systems*)
			 (funcall *follow-asdf-systems* system-name))
		       *follow-asdf-systems*))
      ((eql :also-load)
            (asdf:oos 'asdf:load-op system-name))
      (null nil)
      (t    (scan-asdf-components (getf info :components)))))
  expr)
