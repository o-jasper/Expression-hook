
(defsystem :package-project
  :depends-on (:j-string-utils :expression-scan :alexandria)
  :description "Packages a project in a simple way."
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "package-project")))))
