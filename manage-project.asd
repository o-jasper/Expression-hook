
 (defsystem :manage-project
    :depends-on ( :args-n-command :alexandria.0.dev :j-string-utils :expression-scan)
    :description "Find systems to scan, scan them."
    :license "GPLv3"
    :serial t
    :components ((:module "src/"
                  :components ((:file "manage-project")))))
