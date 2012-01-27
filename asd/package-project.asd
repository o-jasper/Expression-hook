
(defsystem :package-project
  :depends-on (:j-string-utils :expression-scan :alexandria)
  :description "Packages a project in a simple way."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "package-project")))))
