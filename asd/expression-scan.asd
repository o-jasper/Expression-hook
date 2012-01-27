
(defsystem :expression-scan
  :depends-on (:cl-fad :asdf :expression-hook :denest :j-general :alexandria)
  :description "Can create and use expression-hook to obtain
 information about code. Any s-expression can be tracked.
 (So macros and functions can be tracked.)"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "expression-scan")))))
