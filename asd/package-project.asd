
(defsystem :package-project
  :depends-on (:j-string-utils :expression-scan :alexandria)
  :description "Packages a project in a simple way."
  :serial t
  :license "GPLv3"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "package-project")))))
