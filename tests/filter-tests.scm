(use-modules (srfi srfi-64)
             (srfi srfi-19)
             (srfi srfi-26)
             (work-planner filters)
             (work-planner date-json))

(define base-date (string->date "15/1/2023" "~d/~m/~Y"))

(define* (after-base-date days #:optional (hours 0) (minutes 0))
  "Add DAYS to the base date."
  (let ((date-just-day (julian-day->date (+ (date->julian-day base-date) days))))
    (if (or (= hours 0) (= minutes 0))
        date-just-day
        (make-date
         0
         0
         minutes
         hours
         (date-day date-just-day)
         (date-month date-just-day)
         (date-year date-just-day)
         0)))
  )

(define (create-test-data items)
  (map (lambda (item iteration)
         (cons (cons "id" iteration) item))
       items (iota (length items) 1)))

(define test-items
  (create-test-data
   (list
    (work-item
     #:text "Designated Today"
     #:designated-completion-dates base-date)
    (work-item
     #:text "Designated Yesterday"
     #:designated-completion-dates (after-base-date -1))
    (work-item
     #:text "Designated 3 days ago"
     #:designated-completion-dates (after-base-date -3))
    (work-item
     #:text "Due in 2 days"
     #:due-date (after-base-date 2))
    (work-item
     #:text "Due in 4 days"
     #:due-date (after-base-date 4))
    (work-item
     #:text "Due in 7 days"
     #:due-date (after-base-date 7))
    (work-item
     #:text "Due in 8 days"
     #:due-date (after-base-date 8))
    (work-item
     #:text "Due in 10 days"
     #:due-date (after-base-date 10))
    (work-item
     #:text "Due in 14 days"
     #:due-date (after-base-date 14))
    (work-item
     #:text "Completed Today"
     #:completed base-date)
    (work-item
     #:text "Completed Yesterday"
     #:completed (after-base-date -1))
    (work-item
     #:text "Completed due tomorrow"
     #:due-date (after-base-date 1)
     #:completed base-date)
    (work-item
     #:text "Completed due yesterday"
     #:due-date (after-base-date -1)
     #:completed base-date))))

(define (take-indexes lst indexes)
  "Create a new list where items are taken from LST with given INDEXES."
  (map (cut list-ref lst <>) indexes))

(test-begin "filter-tests")

(test-equal
    (take-indexes test-items '(0))
  (filter (make-filter-designated-on base-date) test-items))

(test-equal
    (take-indexes test-items '(3 4 5 11))
  (filter (make-filter-work-due-in-n-days 7 base-date) test-items))

(test-equal
    (take-indexes test-items '(3 4 5))
  (filter (make-filter-work-overdue (after-base-date 7)) test-items))

(test-equal
    (take-indexes test-items '(1 2 3 4 5 6 7 8))
  (filter (make-filter-undesignated base-date) test-items))

(test-equal
    (take-indexes test-items '(0 1 2 9 10))
  (filter filter-no-due-date test-items))

(test-equal
    ;; Reminder that this list is what has NOT being purged
    (take-indexes test-items '(0 1 2 3 4 5 6 7 8 9 11 12))
  (filter (make-filter-purgable base-date) test-items))

(test-end "filter-tests")
