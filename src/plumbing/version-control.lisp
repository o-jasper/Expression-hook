;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :plumbing-version-control
  (:use :common-lisp :destructuring-regex :j-string-utils
	:trivial-shell)
  (:export )
  (:documentation "Stuff that interrogates version control systems to get\
 the current version and origin of stuff.

Currently does git and svn. It is meant for `also`s for package-project, so\
 that for instance, autodocumentation can stamp the version which they\
 documented."))

(in-package :plumbing-version-control)

(defun maybe-to-dir (dir rest)
  (if dir (concat "cd " dir ";" rest) rest))

(defun git-config-vars (&optional dir)
  "Get configuration variables"
  (line-by-line (shell-command (maybe-to-dir dir "git config -l"))
    (lambda (line)
      (destructuring-regex (variable "=" value) line
	(cons (intern (string-rev-case variable) :keyword) value)))))

(defun git-origin-url (&optional dir)
  (cdr (assoc :remote.origin.url (git-config-vars dir))))

(defun git-last-version (&optional dir file)
  "Last version as a string, or :no-git if no git detected"
  (multiple-value-bind (stdout err)
      (shell-command (maybe-to-dir dir (concat "git log -1 " file)))
    (regex-case stdout
      (("commit +" commit-hash "
Author: +" other-stuff);author " +<" email "@" email-domain "> Date: ....
       (declare (ignore other-stuff))
       (values commit-hash err))
      (t
       (cond ((string= err
		       "fatal: Not a git repository (or any of the parent directories): .git
")
	      (values :no-git stdout (string= stdout "")))
	     ((string/= err "")
	      (values :some-error err stdout))
	     (t
	      (values :some-thing stdout)))))))

;TODO would be handier if it could look at files.
(defun svn-info (&optional dir) 
  "Get data from svn info
TODO cant operate on files.."
  (multiple-value-bind (stdout err)
      (shell-command (maybe-to-dir dir "svn info"))
    (cond ((string/= err "")
	   (regex-case err
	     (("svn: +" errorcode ": +'" directory "' +is not a working copy
")
	      (values :no-svn errorcode directory))
	     (t
	      (values :some-error err))))
	  ((string= stdout "")
	   (values :no-output))
	  (t
	   (line-by-line stdout
			 (lambda (line)
			   (destructuring-regex (name ": +" thing) line
			     (cons name thing))))))))

(defun version-control-info (&optional dir file)
  "Get some info from various version control systems.
Currently just svn and git."
  (or (let ((svn-info (svn-info dir))) ;svn
	(unless (keywordp svn-info)
	  (flet ((info (about)
		   (cdr (assoc about svn-info :test #'string=))))
	    (list :type :git
	          :root-path (info "Working Copy Root Path")
		  :repo-origin (info "Repository Root")
		  :version (info "Revision")))))
      (let ((git-last-version (git-last-version dir file))) ;git
	(unless (keywordp git-last-version)
	  (list :type :svn
	        :version git-last-version
		:repo-origin (git-origin-url dir)
		;Figure out root path?
		)))))
