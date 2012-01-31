;;
;;  Copyright (C) 28-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :package-project-gil-autodoc
  (:use :common-lisp :expression-scan :gil-autodoc :j-string-utils
	:gil-html) ;TODO gil-html here good for the default, but otherwise..
  (:export also)
  (:documentation "Provides `also` which can be used as that argument to\
 `package-project:auto-update` to make autodocumentation using gil-autodoc.

Different formats depending on `from`, defaultly :html, you can also provide\
options(`opts`) for it, and pass through other arguments to\
 `gil-autodoc:gil-autodoc`."))

(in-package :package-project-gil-autodoc)

(defun also (&key (dir "doc/gil-autodoc/") (from :html) opts 
	     order (state :exported))
  "TODO"
  (lambda (package project-dir extra)
    (declare (ignore extra))
    (let ((file (concat project-dir dir (string-rev-case (symbol-name package))
			".html")))
      (ensure-directories-exist (print file))
      (gil-autodoc package :from from :opts (append (list :file file) opts)
		   :order order :state state))))
