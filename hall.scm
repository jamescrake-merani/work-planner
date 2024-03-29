(hall-description
  (name "work-planner")
  (prefix "")
  (version "0.1")
  (author "James Crake-Merani")
  (copyright (2022))
  (synopsis "")
  (description "A tool for managing your work.")
  (home-page
    "https://github.com/jamescrake-merani/work-planner")
  (license gpl3+)
  (dependencies `(("guile-readline" (ice-9 readline) ,guile-readline)
                  ("guile-json" (json) ,guile-json-4)))
  (skip ())
  (files (libraries
           ((scheme-file "f")
            (directory
              "work-planner"
              ((scheme-file "date-json")
               (scheme-file "filters")
               (scheme-file "date-helpers")
               (scheme-file "command-line")
               (scheme-file "colours")))
            (directory
             "f"
             ((scheme-file "ports")
              (scheme-file "re-exports")))))
         (tests ((directory
                   "tests"
                   ((scheme-file "filter-tests")))))
         (programs ((directory
                     "scripts"
                     ((in-file "work-planner")))))
         (documentation
           ((text-file "ChangeLog")
            (text-file "AUTHORS")
            (text-file "NEWS")
            (directory
              "doc"
              ((texi-file "version")
               (info-file "work-planer")
               (texi-file "work-planner")
               (text-file "stamp-vti")
               (text-file ".dirstamp")))
            (text-file "COPYING")
            (symlink "README" "README.org")
            (org-file "README")))
         (infrastructure
           ((scheme-file "hall")
            (text-file ".gitignore")
            (scheme-file "guix")))))
