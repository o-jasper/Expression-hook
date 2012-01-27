
(defsystem :package-project-documentation-template
  :depends-on (:documentation-template :expression-scan)
  :description "Provides a function that can be used in the `also` keyword
 argument of `package-project:auto-update` to add documentation-template
 output. 

NOTE: that package (at least the version i am looking at) really is meant as
 _template_, it leaves stuff to fill in, and comments that are not applicable
 to your project! For instance the -for-you-invalid- link to download! Do not
 edit the output itself, because running this `also` again will overwrite
 it."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "package-project-documentation-template")))))
