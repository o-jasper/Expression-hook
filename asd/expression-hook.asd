
(defsystem :expression-hook
  :depends-on (:denest :alexandria)
  :description "Macroexpands code purely by itself. *expression-hook* continues it,
 and must call further expand-hook.
Use the scanner with the EXPAND function with some hook used.

Used for gathering information on code autodoc via expression-scan."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "expression-hook")))))
