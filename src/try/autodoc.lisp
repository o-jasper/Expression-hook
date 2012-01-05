
(defpackage :test-gil-autodoc
  (:use :common-lisp :expr-scan :gil-autodoc :ltk :gil-def)
  (:export )
  (:documentation "Mere use-testing.
TODO update using `window-under` (other TODO about that first)"))

(in-package :test-gil-autodoc)

(defun ltk-track (track &optional (which #'doc) &rest args)
  (let ((inlinable-concept:*implementation* :ltk))
    (ltk:with-ltk ()
      (let ((gil-ltk::*text* (make-instance 'ltk:text :font "helvetica")))
	(gil-ltk::init-associated)
	(ltk:pack gil-ltk::*text* :expand 1 :fill :both)
	(gil (apply which (cons track args)))))))

(access-result 'defun 'line-by-line)


(ltk-track (access-result 'defun 'j-string-utils:line-by-line))
(ltk-track (access-result 'defmacro 'j-string-utils:with-line-by-line))
(ltk-track (access-result 'defvar 'gil-def:*associated*))

(let ((*path* t)) (ltk-track (access-result 'defpackage :j-string-utils)))
(ltk-track (access-result 'defclass 'track-fun)) ;TODO fix

(ltk-track (access-result 'defpackage :j-string-utils) #'doc-whole-package)

(let ((inlinable-concept:*implementation* :ltk))
  (ltk:with-ltk ()
    (let ((gil-ltk::*text* (make-instance 'ltk:text :font "helvetica")))
      (gil-ltk::init-associated)
      (ltk:pack gil-ltk::*text* :expand 1 :fill :both)
      (gil (per-package '(+ a b c - ltk:pack) :mention-sym  #'symbol-name)))))

(expr-scan:scan :j-string-utils)

(documentation #'concat t)
(apropos-list "" : t)

(window-under
 :ltk '(:file "ska.html")
 (lambda (); (doc (access-result 'defpackage :j-string-utils))))
   (doc (access-result 'defun 'j-string-utils:line-by-line))))
   (gil-autodoc::title-track (access-result 'defun 'j-string-utils:line-by-line))))

(window-under
 :ltk '(:string-stream-p t)
 (lambda ()
   (doc-whole-package (access-result 'defpackage :j-string-utils))))

(gil-autodoc :j-string-utils)

(gil-autodoc :j-string-utils :from :html :opts '(:file "ska.html"))


(setf (access-result 'defgeneric 'doc) nil)

(window-under :ltk nil
	      (lambda () (section 3 (lgil (style :item-name "ska") 
					  (style :package-title "skoe"))
				  "kitten")))

(titl

(window-under :html '(:file "ska.html")
  (lambda () (style :fun-title "ska")))

(gil-autodoc :j-string-utils )

(associated :package-title);fun-title)