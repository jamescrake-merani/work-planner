(use-modules (srfi srfi-64)
             (srfi srfi-19)
             (work-planner filters)
             (work-planner date-json))

(define base-date (string->date "15/1/2023" "~d/~m/~Y"))

(define (after-base-date days)
  "Add DAYS to the base date."
  (julian-day->date (+ (date->julian-day days) days)))

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
     #:text "Completed Today"
     #:completed base-date)
    (work-item
     #:text "Completed Yesterday"
     #:completed (after-base-date -1))
    (work-item
     #:text "Completed due tomorrow"
     #:due-date (after-base-date 1))
    (work-item
     #:text "Completed due yesterday"
     #:due-date (after-base-date -1)))))

(test-begin "filter-tests")

(test-equal
    (list (list-ref test-items 0) (list-ref test-items 2))
  (filter (make-filter-work-item-to-be-done-on-date (make-date 0 0 0 15 20 11 2022 0)) test-items))

(test-equal
    (list (list-ref test-items 0) (list-ref test-items 1))
  (filter (make-filter-work-due-in-n-days 5 (make-date 0 0 0 22 18 11 2022 0)) test-items))

(test-equal
    (list (list-ref test-items 0) (list-ref test-items 1))
  (filter (make-filter-work-overdue (make-date 0 0 0 22 23 11 2022 0)) test-items))

(test-equal ;;TODO: Should probably add another test-item for this
    (list (list-ref test-items 1))
  (filter filter-no-to-be-done-date test-items))

(test-end "filter-tests")
