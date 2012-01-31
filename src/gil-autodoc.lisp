;;
;;  Copyright (C) 28-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :gil-autodoc
  (:use :common-lisp :alexandria :j-string-utils :gil-def :expression-scan)
  (:export link tell-about mention sort-by-package list-by-package
	   doc doc-whole-package 
	   gil-autodoc)
  (:documentation "Autodocumentation of expression-scan results using gil-mac.
TODO 
* some lists need to be better('description list')
* function, macro, variable, class plain lists at start.
* use indentation for better readability.
* use `section`
* formalization examples, todos, notes, references"))

(in-package :gil-autodoc)

(defvar *on-docstrings* #'identity
  "Function applied to documentation strings.")

(defvar *from-dir* "/home/jasper/proj/")

(defgeneric link (thing)
  (:documentation "Return gil link name to the thing.")
  (:method ((list list))
    ;(check-type list (cons symbol (cons symbol null)))
    list)
  (:method ((path string))
    (if (start-str= *from-dir* path)
      (subseq path (length *from-dir*)) path))
  (:method ((path pathname))
    (link (namestring path)))
  (:method ((symbol symbol))
    nil)
  (:method ((package package))
    (link (access-result 'defpackage package)))
  (:method ((track base-track))
    (list (track-item track :def) (track-item track :name))))

(defgeneric tell-about (thing)
  (:documentation "Mention something(not linking)")
  (:method ((path pathname))
    (link path))
  (:method ((string string))
    string)
  (:method ((symbol symbol))
    (string-rev-case (symbol-name symbol)))
  (:method ((package package))
    (string-rev-case (package-name package)))
  (:method ((track base-track)) 
    (string-rev-case (symbol-name (track-item track :name)))))

(defun mention (thing &rest body)
  "Mention a scanned thing, linking to it."
  (style (typecase thing
	   (track-fun :fun-name) (track-var :var-name)
	   (track-package :package-name)
	   (t t))
	 (refer nil (link thing)
	   (if body (lambda () (mapc #'handle* body)) (tell-about thing)))))

(defun title-track (thing &rest body)
  (name (link thing)
	(if body (lambda () (mapc #'handle* body)) (tell-about thing))))

(defun mention-var (var-name &rest body) 
  (apply #'mention (cons (or (access-result 'defvar var-name) 
			     (access-result 'defparameter var-name)
			     var-name)
			 body)))
(defun mention-fun (fun-name &rest body)
  (apply #'mention (cons (or (access-result 'defun fun-name) 
			     (access-result 'defgeneric fun-name)
			     fun-name)
			 body)))

(defun package-name-sym (sym)
  (package-name (or (symbol-package sym)
		    (find-package :common-lisp))))

(defun sort-by-package (list)
  (labels ((p-name (sym)
	     (package-name-sym sym))
	   (sorter (a b &key (pa (p-name a)) (pb (p-name b)))
	     (if (string= pa pb) (string< (symbol-name a) (symbol-name b))
		                 (string< pa pb))))
    (sort list #'sorter)))

(defun list-by-package (list &key sorted)
  "Make a list of lists, each list containing symbols of the same package."
  (when list
    (let*((list (if sorted list (sort-by-package list)))
	  (first-p (package-name-sym (car list))))
      (if-let (p (position-if-not 
		  (lambda (s)
		    (string= first-p (package-name-sym s)))
		  (cdr list)))
	(cons (subseq list 0 (+ p 1)) (list-by-package (subseq list (+ p 1))
						       :sorted t))
	(list list)))))

(defun remove-boring (list &key (boring `(:common-lisp :common-lisp-user
					  ,@expr-scan::*ignore-packages*)))
  (remove-if
   (lambda (el) 
     (find (intern (package-name-sym el) :keyword) boring))
   list))

(defun per-package (list &key mention-sym (between ", ")
		    (mention-package 
		     (lambda (package) (gil(style :package-name
						  (mention package) ": "))))
		    (after-package :newline) between-package)
  "Mention symbols sorted by package. TODO "
  (flet ((pp (symbol-list 
	      &optional (package (symbol-package (car symbol-list))))
	   (funcall mention-package package)
	   (handle (funcall mention-sym (car symbol-list)))
	   (mapc (lambda (s) (gil between (funcall mention-sym s)))
		 (cdr symbol-list))
	   (handle after-package)))
    (let ((list (list-by-package list)))
      (lgil (lambda () (pp(car list)))
	    between-package
	    (lambda () (mapc #'pp (cdr list)))))))

(defun between (between fun list)
  (funcall fun (car list))
  (dolist (el (cdr list)) (handle between) (funcall fun el)))

(defvar *dep-var* t)
(defvar *dep-fun* t)
(defvar *dep-mention* :full)

(defun doc-dep(dep)
  "Document the variables and function something depends on."
  (lgil
    (when-let (dep-var-list (and *dep-var* 
				 (remove-boring (track-item dep :var-dep))))
      (style *dep-var*
	(paragraph (style :item-name "uses variables: ")
	  (case *dep-mention*
	    (:full (per-package dep-var-list :mention-sym #'mention-var))
	    (:nop  (per-package dep-var-list :mention-sym #'mention-var 
				:mention-package (constantly "; ")))))))
    (when-let (dep-fun-list (and *dep-fun* 
				 (remove-boring (track-item dep :fun-dep))))
      (style *dep-fun*
	(paragraph (style :item-name "uses functions: ")
	  (case *dep-mention*
	    (:full (per-package dep-fun-list :mention-sym #'mention-fun))
	    (:nop  (per-package dep-fun-list :mention-sym #'mention-fun 
				:mention-package (constantly "; ")))))))))

(defparameter *path* :path)

(defun doc-path (track)
  "Document the path the object was tracked in."
  (when-let (path-str (and *path* (track-item track :path)))
    (p (style :minor-item-name "found in file: ")
 ;TODO seems to lose the path style..
       (style *path* (mention (pathname path-str))))))

(defun handle-extra-arg (extra-arg)
  (p (mention (car extra-arg)) (cdr extra-arg)))

(defvar *extra-args* t)
(defun doc-extra-args (obj)
  "Document extra specifically added documentation."
  (when-let (extra-args-doc (and *extra-args* (track-extra obj :arg-doc)))
    (style *extra-args* 
	   (lambda () (mapc #'handle-extra-arg extra-args-doc)))))

(defvar *in-funs* :minor)
(defun doc-created-in (obj)
  ;Functions the defun was created in.
    (when-let (in-funs-val (and *in-funs* (track-item obj :in-funs)))
      (p (style :minor-item-name "created from: ")
	 (style *in-funs* (format nil "~{~a ~}" in-funs-val)))))

;TODO slightly annoying that these also provide style..
(defvar *args* :bold)

(defun doc-args (obj)
  (when *args*
    (p (style :item-name "arguments: ")
       (style *args* (string-rev-case
	    (format nil "(~{~a~^ ~})" (track-item obj :args)))))))

(defgeneric doc (track &key &allow-other-keys)
  (:documentation "Documents some tracker.")
  (:method ((track null) &key)
    (error "Cannot document null")))

(defun p (&rest stuff)
  (unless (not (find-if-not #'null stuff))
    (paragraph (lambda () (mapc #'handle* stuff)))))

(defun doc-body (item-name which-title title &rest body)
  (p
   (style :item-name item-name " ")
   (style which-title title)
   (style :item-body (lambda () (mapc #'handle* body)))))

(defmethod doc ((fun track-fun) &key)
 ;TODO remove the arguments to give styles.
  (doc-body (case (track-item fun :def)
	      (defun "function")
	      (defmacro "macro")
	      (t        (error "What is ~a" (track-item fun :def))))
	    :fun-title (title-track fun)
    (p (doc-args fun))
    (or (track-extra fun :doc) ;Prefer especially given documentation.
	(p (documentation (track-item fun :name) 'function)))
    (doc-extra-args fun)
    (doc-dep fun)
    (doc-created-in fun)
    (doc-path fun)
    (track-extra fun :more)))

(defmethod doc ((generic track-generic) &key)
  (doc-body "generic function" :fun-title (title-track generic)
    (p (doc-args generic))
    (or (track-extra generic :doc) 
	(p (documentation (track-item generic :name) 'function)))
    (doc-extra-args generic)
    (doc-path generic)
    ;TODO document the methods.
    (track-extra generic :more)))

(defmethod doc ((var track-var) &key)
  "Documents a variable."
  (doc-body (case (track-item var :def)
	      (defvar       "variable")
	      (defparameter "parameter"))
	    :var-title (title-track var)
    (when-let (one (or (track-extra var :doc)
		       (documentation (track-item var :name) 'variable)))
      (p one))
    (doc-path var)
    (track-extra var :more)))

;TODO also doc-struct
(defvar *slot-way* :full)
(defvar *derives-from* t)
(defmethod doc ((class track-class) &key (slot-way *slot-way*)
		(derives-from *derives-from*))
  "Document about a class."
  (doc-body "class" :class-title (title-track class)
     (or (track-extra class :doc)
	 (track-item class :doc))
     (when-let (derives-list 
		(and derives-from (track-item class :derives-from)))
       (p (style :minor-item-name "derived from ")
	  (lambda ()
	    (between ", " 
	      (lambda (derives) 
		(handle 
		 (if-let (used-class (access-result 'defclass derives))
		   (mention used-class) (tell-about used-class))))
	      derives-list))))
     (case slot-way
       (:full
	(let ((extra (track-extra class :slots)))
	  (lambda ()
	    (mapc (lambda (slot)
		    (if (listp slot)
		      (p "  "(style :slot-title (tell-about (car slot)))
			   " "
			   (or (getf extra (car slot))
			       (getf (cdr slot) :documentation)
			       (style :side-note "(no documentation)")))
		      (p (tell-about slot))))
		  (track-item class :slots)))))
       (:short
	(lambda ()
	  (handle (style :item-name "slots: "))
	  (between ", " (lambda (slot)
			  (handle (tell-about (if (listp slot) (car slot) slot))))
		   (track-item class :slots)))))
     (doc-path class); :path path)
     (track-extra class :more)))

(defvar *exports* t)
(defvar *paths* t)
(defmethod doc ((package track-package)
		&key (exports *exports*) (paths *paths*))
  "Mention package, but does not mention objects within it."
  (doc-body "package" :package-title (title-track package)
    (when-let 
	(one (or (track-extra package :doc)
		 (documentation (find-package (track-item package :name)) t)))
      (p one))
    (when-let (export-list (and exports 
				(apropos-list "" (track-item package :name))))
      (p (style :item-name "exports ")
	 (style exports (lambda ()
			  (between ", "(compose #'handle* #'mention)
				   export-list)))))
    ;TODO external symbols.
    (doc-path package)
    (when paths
      (let ((paths-list (track-item package :paths)))
	(when (> (length paths-list) 1)
	  (p (style :item-name "has code in: ")
	     (style paths
		    (lambda ()
		      (between ", " (compose #'handle* #'mention #'pathname)
			       (sort (remove (track-item package :path) paths-list
					     :test #'string=) #'string>))))))))))

;(defun doc-system (system &key level) ;TODO.

(defun remove-other-types (type list &key (test #'eql))
  (remove-if-not (lambda (el) (if (listp type)
				(find (type-of el) type :test test)
				(funcall test (type-of el) type)))
		 list))

(defun doc-whole-package
    (package &key
     (path-per-obj (> (length (track-item package :paths)) 1))
     order (state :exported))
  (let*((order (or order '(("Classes"    track-class)
			   ("Variables"  track-var)
			   ("Functions " track-fun track-generic))))
	(package (typecase package (symbol (access-result 'defpackage package))
			           (t      package)))
	(doc-nr (setf (track-extra package :doc-nr) ;TODO use doc-nr's
		      (+ (or (track-extra package :doc-nr) 0) 1)))
	(scan-list
	 (when (find-if (lambda (el) (and (listp el) (not(symbolp(car el)))))
			order)
;	   (sort (lambda (a b
;			  &key (na (track-item a :name)) (nb (track-item b :name)))
;		   (declare (type symbol na nb))
;		   (string> (symbol-name na) (symbol-name nb)))
	   (remove-if-not
	    (case state
	      ((:exported :internal)
	       (let ((exported 
		      (apropos-list "" (track-item package :name) t)))
		 (lambda (el &key (external-p
				   (find (track-item el :name) exported)))
		   (or (and external-p (eql state :exported))
		       (and (not external-p) (eql state :internal))))))
	      (t
	       state))
	    (track-item package :scan-list)))))
    (declare (ignore doc-nr))
    ;(page (track-item package :name)
    (lambda ()
      (handle (paragraph 
	       (style :bold "I do not expect this autodoc to fully work yet!")))
      (let ((*path* t)) (handle (doc package)))
      (let ((*path* (when path-per-obj *path*)))
	(dolist (el order)
	  (typecase el
	    ((cons symbol symbol)
	     (handle (doc (access-result (car el) (cdr el)))))
	    (list
	     (when-let (have (remove-other-types (cdr el) scan-list))
	       (p (style :category (car el)))
	       (mapc (compose #'handle* #'doc) have)))
	    (t ;Asked to put in text.
	     (handle el))))))))
  ;;TODO classes, etc.

(defun gil-autodoc (package &key (from :ltk) opts order (state :exported))
  "Documents a package, creating a window/file for you."
  (window-under 
   from opts
   (lambda () (doc-whole-package (access-result 'defpackage package)
				 :order order :state state))))

(setf (associated nil :fun-title) '(:weight :bold))
(setf (associated nil :var-title) '(:weight :bold))
(setf (associated nil :package-title) '(:refer (:underline :bold)))
(setf (associated nil :class-title) '(:weight :bold))

(setf (associated nil :category) '(:refer (:underline :bold)))
(setf (associated nil :minor-item-name) (list :size :small :color "#7F7F7F"))
(setf (associated nil :item-name) (list :size :small :color "#707099"))
(setf (associated nil :item-body) (list :padding-left "5%"))

(setf (associated nil :side-note) (list :refer :small :color "#909099"))

(setf (associated nil :fun-name) (list :color :blue))
(setf (associated nil :var-name) (list :color :purple))
(setf (associated nil :package-name) (list :color "#005500"))

(setf (associated nil :path) (list :color :cyan))

(setf (associated nil :slot-title) (list :color "#225522"))
