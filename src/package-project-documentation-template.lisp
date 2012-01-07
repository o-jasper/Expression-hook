;;
;;  Copyright (C) 06-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :package-project-documentation-template
  (:use :common-lisp :expression-scan :documentation-template)
  (:export also)
  (:documentation "Provides a function that can be used in the `also` keyword\
 argument of `package-project:auto-update` to add documentation-template output. 

NOTE: that package (at least the version i am looking at) really is meant as\
 _template_, it leaves stuff to fill in, and comments that are not applicable to\
 your project! For instance the -for-you-invalid- link to download! Do not edit\
 the output itself, because running this `also` again will overwrite it."))

(in-package :package-project-documentation-template)

(defun also
    (&key (dir "doc/documentation-template/")
     maybe-skip-methods-p 
     (subtitle "Automatically generated")
     (if-exists :supersede) (if-does-not-exist :create))
  "Makes the function for the `:also` argument of `package-project:auto-update`.
`dir` is relative directory(default 'doc/documentation-template/'. 
Other arguments are fed through to the `create-template`.(Except target)
NOTE see defpackage documentation."
  (lambda (package project-dir extra)
    (let ((target (concatenate 'string project-dir dir
		    (string-downcase (package-name package)) ".html")))
      (ensure-directories-exist target)
      (create-template (find-package package) 
       :target target :maybe-skip-methods-p maybe-skip-methods-p 
       :subtitle (or (getf (getf extra :documentation-template) :subtitle) 
		     subtitle)
       :if-exists if-exists :if-does-not-exist if-does-not-exist))))
