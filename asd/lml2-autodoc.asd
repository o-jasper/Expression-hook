
(defsystem :lml2-autodoc
  :depends-on (:expression-hook :expression-scan :lisp-markup-language-2 :j-string-utils :alexandria)
  :description "LML2 based autodocumentation."
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "lml2-autodoc")))))
