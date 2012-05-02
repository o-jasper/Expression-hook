;;
;;  Copyright (C) 02-05-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :lml2-autodoc
  (:use :common-lisp :alexandria :j-string-utils :lml2 :expression-scan)
  (:export )
  (:documentation "LML2 based autodocumentation."))

(in-package :lml2-autodoc)

(defvar *from-dir* nil)

(defvar *to-dir* nil)
(defparameter *accumulating-file* "file.html")

(defparameter *css-ref* ".css")

(defun of-packages (packages)
  (labels ((of (thing)
	     (typecase thing
	       (package    (find (intern (package-name thing) :keyword) 
				 packages))
	       (symbol     (of (symbol-package thing)))
	       (base-track (of (track-item thing :name))))))
    #'of))

(defparameter *boring-package-p* 
  (of-packages `(:common-lisp ,@expr-hook::*ignore-packages*)))

(defun track-of-package (package)
  (access-result 'defpackage (intern (package-name package) :keyword)))

(defgeneric ret-name (thing)
  (:documentation "Returns the name of a thing")
  (:method (thing)             (princ-to-string thing))
  (:method ((symbol symbol))   (string-rev-case (symbol-name symbol)))
  (:method ((package package)) (if-let (track (track-of-package package))
				 (ret-name track)
				 (string-rev-case (package-name package))))
  (:method ((track track-package))
    (string-rev-case (symbol-name (track-item track :name))))
  (:method ((track base-track))
    (string-rev-case (symbol-name (track-item track :name)))))

(defun package-link-file (package)
  (or *accumulating-file*
      (concat *to-dir* (ret-name package) ".html")))

(defgeneric link-to (thing)
  (:documentation "Link to thing.")
  (:method ((path string))
    (if (start-str= *from-dir* path)
	(subseq path (length *from-dir*)) path))
  (:method ((path pathname))
    (link-to (namestring path)))
  (:method ((package package))
    (when-let (track (track-of-package package))
      (link-to track)))
  (:method ((track track-package))
    (if *accumulating-file*
      (concat (package-link-file track) "#" (ret-name track))
      (concat *to-dir* (ret-name track) ".html")))
  (:method ((track base-track))
    (concat (package-link-file (symbol-package (track-item track :name)))
	    "#" (ret-name track))))

(defgeneric style-of (thing)
  (:method ((path pathname))   "path")
  (:method ((string string))   "string")
  (:method ((symbol symbol))   "symbol")
  (:method ((package package)) "package")
  (:method ((package track-package)) "package")
  (:method ((track base-track)) (string-rev-case (symbol-name (track-item track :def))))
  (:method ((track track-fun))  (if (eql (track-item track :def) 'defmacro)
				  "macro" "function"))
  (:method ((track track-var))  "variable"))

(defgeneric unstyled-mention (thing)
  (:documentation "Mention something(not styling it)")
  (:method ((path pathname))       (html ((:a :href (link-to path))
					  (lml-princ path))))
  (:method ((string string))       (lml-princ string))
  (:method ((symbol symbol))       (lml-princ (ret-name symbol)))
  (:method ((package package))     (if-let (track (track-of-package package))
				     (unstyled-mention track)
				     (lml-princ (ret-name package))))
  (:method ((track track-package)) (if-let (link (link-to track))
				     (html ((:a :href link)
					    (lml-princ (ret-name track))))
				     (lml-princ (ret-name track))))
  (:method ((track base-track))    (lml-princ (ret-name track))))

(defun mention (thing) 
  (html ((:a :class (style-of thing)) (unstyled-mention thing))))

(defgeneric style-figured (thing)
  (:method ((symbol symbol))
    (style-of (access-result '(defclass defun defmacro defvar defparameter) 
			     symbol))))

(defun mention-figured (thing)
  "Figure out what style best to give it."
  (html ((:a :class (style-figured thing)) (unstyled-mention thing))))
  
(defgeneric title (thing)
  (:documentation "Give something a title")
  (:method (thing)
    (html ((:h3 :class (concat (style-of thing) "_h"))
	   ((:a :name (ret-name thing)) (lml-princ (ret-name thing)))))))

(defun separated (list fun &key (sep ", "))
  (funcall fun (car list))
  (dolist (el (cdr list)) (lml-princ sep) (funcall fun el)))

(defgeneric doc (style thing &key &allow-other-keys)
  (:documentation "Documents some thing.")
  (:method (style (package package) &key)
    (doc style (track-of-package package)))
  (:method ((list list) thing &key)
    (mapc (rcurry #'doc thing) list) (values)))

(defmacro def-doc (style track &body body)
  (with-gensyms (s)
    `(defmethod doc ((,s (eql ',style)) ,@track ,@(unless (find '&key track) '(&key)))
       ,@body)))

;General stuff.
(def-doc :docstr (track)
  (html (or (when-let (file (track-extra track :doc-html-file))
	      (insert-file file))
	    (when-let (fun (track-extra track :doc-fun))
	      (funcall fun))
	    (when-let (str (or (track-item track :doc-str) 
			       (track-extra track :manual-doc-str)))
	      (lml-princ str)))))

(def-doc :title-n-docstr (track)
  (html (:p :nbsp :nbsp (title track) (doc :docstr track))))

(def-doc :exports ((track track-package))
  (html (:p (if-let (exports-list (apropos-list"" (track-item track :name) t))
	      (html "exports: "
		    ((:a :class  "export-list") 
		     (separated exports-list #'mention-figured)))
	      (html "no exports!")))))

(defun mention-from-file (path paths say-path say-paths)
  (when path
    (html (:p (lml-princ say-path)
	      ((:a :class "package-path") (lml-princ path)))))
  (when paths 
    (when (> (length paths) 1)
      (html ((:p :class "package-path") (lml-princ say-paths)
	     (separated paths #'lml-princ))))))

;Package specific stuff.
(def-doc :from-file ((track track-package))
  (mention-from-file (track-item track :path) (track-item track :paths)
		     "Package file: " "Also in files: "))

(def-doc :deps ((track track-package))
  (if-let (used (remove-if (lambda (pkg)
			     (or (eql pkg (track-item track :name))
				 (funcall *boring-package-p* 
					  (find-package pkg))))
			   (union (track-item track :uses) 
				  (track-item track :also-uses))))
    (html ((:p :class "package-use") "Uses: "
	   (separated (remove-if #'null (mapcar #'find-package used))
		      #'mention)))
    (html ((:p :class "package-use")
	   (lml-princ (if (find :common-lisp (track-item track :uses))
			"Apparently just uses common lisp stuff."
			"Cant figure out what it uses."))))))

;'Flatly' state the documentation parts.
(def-doc :full-flat ((track track-package))
  (doc '(:title-n-docstr :exports :from-file :deps) track))
;Sidepane 'minor' info
(def-doc :sidepane ((track track-package))
  (html (:div ((:div :class "main" :style "width:70%;float:left;")
	       (doc '(:title-n-docstr :exports) track))
	      ((:div :class "side" :style "width:30%;float:right;")
	       (doc '(:from-file :deps) track)))))

;Function specific stuff.
(def-doc :deps-plain ((track track-fun)) 
  (when-let (deps (remove-if *boring-package-p*
			     (append (track-item track :fun-dep)
				     (track-item track :var-dep))))
    (html ((:p :class "package-use") "Depends on: "
	   (separated deps #'mention)))))

;(def-doc :deps-sorted ((track track-fun)) ;TODO sort alphabetically,
; package first.

(def-doc :deps ((track track-fun)) (doc :deps-plain track))

(defun enter-args (args)
  (separated args
	     (lambda (a &key (str (string-rev-case (princ-to-string a))))
	       (if (find a '(&optional &key &form &rest &body))
	         (html ((:a :class "args-n") (lml-princ str)))
		 (lml-princ str)))))

(def-doc :args ((track track-fun))
  (html ((:p :class "args") "(" (enter-args (track-item track :args)) ")")))

(def-doc :title-args ((track track-fun))
  (html ((:p :class (concat (style-of track) "_h")) :nbsp :nbsp
	 ((:a :name (ret-name track)) (lml-princ (ret-name track)))
	 ((:a :class "args") "(" (enter-args (track-item track :args)) ")"))))

(def-doc :short ((track track-fun))
  (doc '(:title-n-docstr :args) track))

(def-doc :full-flat ((track track-fun))
  (doc '(:title-args :docstr ; :long-args TODO
	 :deps) track))

(def-doc :sidepane ((track track-fun))
  (html ((:div :class "main" :style "width:70%;float:left;")
	 (doc '(:title-args :docstr) track)) ; :long-args TODO
	((:div :class "side" :style "width:30%;float:right;")
	 (doc :deps track))))

;TODO track-generic

;Variable specific stuff.
(def-doc :full-flat ((track track-var)) (doc :title-n-docstr track))
(def-doc :sidepane ((track track-var))
  (html ((:div :class "main" :style "width:70%;float:left;")
	 (doc :title-n-docstr track))))

(defun doc-completely
    (package &key (how :sidepane) (list (sort (apropos-list "" package t) 
					      #'string-lessp)))
  (flet ((doc-each-of (kind)
	   (dolist (el list)
	     (when-let (result (access-result kind el))
	       (doc how result)))))
    (format *html-stream* "<!DOCTYPE html><head><link rel=\"stylesheet\"")
    (format *html-stream* " type=\"text/css\" href=~s /></head>" *css-ref*)
    (html (:html (:body (doc how (find-package package))
			((:h2 :style "width:70%;float:left;") "Functions:")
			(doc-each-of 'defun); defgeneric)) ;TODO
			((:h2 :style "width:70%;float:left;") "Macros:")
			(doc-each-of 'defmacro)
			((:h2 :style "width:70%;float:left;") "Variables")
			(doc-each-of '(defvar defparameter)))))))
#|
 (with-open-file (lml2:*html-stream* "/home/jasper/lml.html" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
  (doc-completely :expression-scan))

 (with-open-file (lml2:*html-stream* "/home/jasper/lml.html" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
  (format *html-stream* "<!DOCTYPE html><head><link rel=\"stylesheet\" type=\"text/css\"")
  (format *html-stream* " href=~s /></head>" *css-ref*)
  (html (:html (:body
		(doc :sidepane (find-package :expression-scan))
		;((:div :class "main" :style "width:70%;") "ska")
		(doc :sidepane (access-result 'defun 'add-scanner-fun))))))
;  (mention (car (apropos-list "" :gil-autodoc))))

 (mention (find-package :common-lisp))

;(symbol-name 

 (track-item (access-result 'defun 'add-scanner-fun) :doc-str)

 (maphash (compose #'print #'list) (access-result 'defgeneric))
|#
