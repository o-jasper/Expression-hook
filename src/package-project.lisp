;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :package-project
  (:use :common-lisp :alexandria :expression-scan :j-string-utils)
  (:export auto-update package-info package-info-join)
  (:documentation "Packages a project in a simple way."))

(in-package :package-project)

(defun implementation-package-p (pkg)
  "Returns true if package belongs to implementation."
  (find pkg '(:common-lisp :sb-thread)))

(defvar *package-info* (make-hash-table) 
  "Extra info on packages
:system-name    for overriding system name. (Like :alexandria.0.dev ->\
 :system-name)
:also-depends   lists extra dependencies not seen by scanner.
:license        This package has a different license than the rest
                (asd-license header-file-relative-from doc/info)")

(defun package-info (package &optional what)
  "Get info about package. if` :system-name` specified, the asd system name\
 does not correspond to the package name."
  (if what (getf (gethash package *package-info*) what)
           (gethash package *package-info*)))
(defun (setf package-info) (to package what)
  "Set info about package. Note that `what` is not optional here."
  (setf (getf (gethash package *package-info*) what) to))

(defun package-info-join (package joined)
  (when joined
    (assert (cdr joined) nil ;TODO will this message inform the user well?
	    "Malformed plist! Must have even number.")
    (setf (package-info package (car joined)) (cadr joined))
    (package-info-join package (cddr joined))))

;Some pre-provided ones.(Use init scripts to add yours/inform me)
(setf (package-info :alexandria.0.dev :system-name) :alexandria)

(defun update-header (prepend-file file)
  (let ((header (with-output-to-string (s)
		  (with-open-file (stream prepend-file)
		    (with-line-by-line stream (line) 
		      (values (write-line line s)))))))
    (ensure-directories-exist "/tmp/cl-package-project/") ;TODO
    (let ((from-file (concat "/tmp/cl-package-project/" 
			     (file-namestring file))))
      (copy-file file from-file)
      (with-open-file (out file :direction :output :if-exists :supersede)
	(write-string header out)
	(with-open-file (in from-file)
	  (with-line-by-line in (line)
	    (unless (and (> (length line) 1) (string= line ";;" :end1 2))
	      (values (write-line line out) t)))
	  (with-line-by-line in (line)
	    (assert (stringp line))
	    (values (write-line line out))))))))

(defun delist (x) (if (listp x) (car x) x))
(defun enlist (x) (if (listp x) x (list x)))

(defun about-pkg (package names allow-double)
  "Gets the info about a package."
  (labels ((get-info (names)
	     (when names
	       (when-let (have (package-info package (car names)))
		 (assert (or allow-double 
			     (not(find-if (curry #'package-info package)
					  (cdr names))))
			 nil "Warning something is indicated in the info file\
doubly! I disallow this.")
		 (return-from get-info have))
	       (get-info (cdr names)))))
    (get-info names)))

(defun write-system
    (project-dir info
     &key (sys-name (lambda (pkg) (or (package-info pkg :system-name) pkg)))
     src-dir package 
     (pkg-name (string-downcase (symbol-name package))))
  "Writes .asd file for a project."
  (ensure-directories-exist (concat project-dir "asd/"))
  (with-slots ((uses expr-scan::uses) (also-uses expr-scan::also-uses) 
	       (form expr-scan::form) (paths expr-scan::paths)) info
    (with-open-file (stream (concat project-dir "asd/" pkg-name ".asd")
			    :direction :output :if-does-not-exist :create
			    :if-exists :supersede)
      (format stream "~%(defsystem :~a" pkg-name)
    ;Dependencies.
      (when-let 
	  (list (mapcar
		 (compose #'string-downcase #'symbol-name sys-name) 
		 (union ;TODO removing them explictly like :also-depends
		  (remove-if
		   (lambda (p)
		     "Remove self-references, implementing packages and\
 explicitly removed stuff."
		     (or (eql p package)
			 (implementation-package-p p)
			 (find p (package-info package :remove-depends))))
		   (append uses also-uses))
		  (package-info package :also-depends))))
	(format stream "~%  :depends-on (~{:~a~^ ~})" list))
    ;Documentation string. 
      (format stream "~%  :description \"~a\"~%  :serial t"
	      (or (cadr (assoc :documentation (cddr form))) ;Description.
		  "(autogenerated by `manage-systems`)Package docstring\
 apparently missing."))
    ;Do stuff if provided. (Bunch of stuff not supported)
      (flet ((about (&rest names)
	       "Gets the info about a package."
	       (about-pkg package names nil)))
	  (when-let (version (about :asd-version :version))
	    (format stream "~%  :version \"~a\"" version))
	  (when-let (license (about :license))
	    (format stream "~%  :license ~s" (delist license)))
	  (when-let (authors (about :author :authors))
	    (format stream "~%  :author \"~{~a~^, ~}\"" (enlist authors)))
	  (when-let (maintainers (about :maintainer :maintainers))
	    (format stream "~%  :maintainer \"~{~a~^, ~}\"" 
		    (enlist maintainers))))
    ;List the files.
      (format stream "~%  :components ((:module ~s
                 :components (~{(:file \"~a\")~^ ~}))))~%"
	      (concat "../" src-dir) ;Sources directory.
	      (mapcar (lambda (file &key (f (file-namestring file)))
			(subseq f 0 (search ".lisp" f)))
		      (reverse paths))))))

(defun read-empty (stream)
  "Read from the stream until it is dead."
  (do ((list nil (cons (read stream nil :eof) list)))
      ((unless (null list) (eql (car list) :eof)) (reverse (cdr list)))))

(defun read-key-file (file)
  (if (probe-file file)
    (handler-case (with-open-file (stream file)
		    (remove-if #'null (read-empty stream)))
      (parse-error (a) 
	(error "Consider syntax errors, in doc/info/info
~s" a))
      (error (a)
	(error "Error during reading doc/info/info, check it?\ Not a syntax/stream error though;~%~s" a))
      (stream-error (a)
	(error "Some stream error, non-matching parenthesis in doc/info/info?
 ~s" a))
      (file-error (a)
	(error "File error, doc/info/info doesnt exist? Another error should\
 have caught this.~%~s" a))
      (t (a)
	(error "Something happened parsing doc/info/info~%~s" a)))
    (error "doc/info/info file doesnt exist.")))

(defun incorporate-key-plist (package key-plist)
  (destructuring-bind (&whole keys &key package-info 
			      &allow-other-keys) key-plist
    (package-info-join package keys) ;Add package-info
    (dolist (el package-info) ;Will overwrite previous.
      (package-info-join (car el) (cdr el)))))

;TODO give user access to hook.
(defun auto-update
    (filename &key (subdirs '("doc" "test" "try" "example" "gui" "plumbing")) 
     also (load-it-p t) (expr-hook (constantly nil)))
  "If application under `src/` directory, assume it is a project directory,\
 and update it. `also` allows you to add stuff like autodocumentation. 
package-project-documentation-template autodocs using documentation-template.
`expr-hook` allows you to have a say on the hook."
  (when load-it-p (load filename))
  (let*((fn (tokenize-str filename "" (rcurry #'char= #\/)))
	(p  (position "src" fn :test #'string=)))
    (if-let (module (when p
			(cond ((= p (- (length fn) 2)) "src")
			      ((find (nth (+ p 1) fn) subdirs :test #'string=)
			       (concat "src/" (nth (+ p 1) fn))))))
      (let ((project-dir ;Figure out project directory.
	     (reduce (lambda (have el) (concat have el "/")) (subseq fn 0 p)
		     :initial-value "/"))
	    (seen-packages (list)))
	(scan filename :*expression-hook* ;Rescan the file.
	      (lambda (expr)
		(funcall expr-hook expr)
		(typecase expr ;record seen packages.
		  ((cons (eql defpackage) list)
		   (push (cadr expr) seen-packages)))
		(scan-expression-hook expr)))
	(assert seen-packages nil "No packages in file!")
	(assert (null(cdr seen-packages)) nil 
		"Only one package per file allowed at the moment.")
      ;Read some info.
	(let ((package (car seen-packages)))
	  (incorporate-key-plist 
	   package (read-key-file (concat project-dir "doc/info/info")))
       ;Write systems for the package.
	  (when also ;Do more stuff specified by user.
	    (funcall also package project-dir (package-info package)))
       ;System based on scan.
	  (let ((result (access-result 'defpackage package)))
	    (write-system project-dir result :src-dir "src" :package package))
         ;Add the header, if provided.
	  (flet ((when-info-file
		     (file &key (full
				 (concat project-dir "doc/info/" file)))
		   (when (probe-file full) full)))
	    (let ((prepend-file
		   (if-let (file (cadr(about-pkg package '(:license) nil)))
		     (or (when-info-file file)
			 (error "License header file specifically mentioned\
 but not provided!"))
		     (or (when-info-file "header.txt")
			 (when-info-file "header")))))
	      (when (probe-file prepend-file)
		(update-header prepend-file filename)))))
	(list :updated project-dir filename))
      (list :didnt-update filename))))
