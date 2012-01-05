
(defsystem :package-project-gil-autodoc
  :depends-on (:gil-html :j-string-utils :gil-autodoc :expression-scan)
  :description "TODO Provides `also` which can be used as that argument to
 `package-project:auto-update` to make autodocumentation using gil-autodoc.

Different formats depending on `from`, defaultly :html, you can also provide
options(`opts`) for it, and pass through other arguments to
 `gil-autodoc:gil-autodoc`."
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "package-project-gil-autodoc")))))
