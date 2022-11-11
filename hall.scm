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
  (license glp3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory
              "work-planner"
              ((scheme-file "date-json")
               (scheme-file "filters")
               (scheme-file "date-helpers")))))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "COPYING")
            (directory
              "doc"
              ((texi-file "version")
               (info-file "work-planer")
               (texi-file "work-planner")
               (text-file "stamp-vti")
               (text-file ".dirstamp")))
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (text-file ".gitignore")
            (scheme-file "hall")))))
