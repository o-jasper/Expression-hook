
(defsystem :gil-autodoc
  :depends-on (:expression-scan :gil-def :j-string-utils :alexandria)
  :description "Autodocumentation of expression-scan results using gil-mac.
TODO 
* some lists need to be better('description list')
* function, macro, variable, class plain lists at start.
* use indentation for better readability.
* use `section`
* formalization examples, todos, notes, references"
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "gil-autodoc")))))
