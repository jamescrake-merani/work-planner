(use-modules (srfi srfi-64)
             (srfi srfi-19)
             (work-planner filters))

(define test-items
  (list
   (list
    (cons "id" 1)
    (cons "text" "Placeholder value 1")
    (cons "due-date" (make-date 0 0 0 10 22 11 2022 0))
    (cons "designated-completion-dates" (make-date 0 0 0 15 20 11 2022 0))
    (cons "done" #f))
   (list
    (cons "id" 2)
    (cons "text" "Placeholder value 2")
    (cons "due-date" (make-date 0 0 0 22 19 11 2022 0))
    ;; No designated completion date
    (cons "done" #f))
   (list
    (cons "id" 3)
    (cons "text" "Placeholder value 3")
    (cons "due-date" (make-date 0 0 0 6 29 11 2022 0))
    (cons "designated-completion-dates" (make-date 0 0 0 13 20 11 2022 0))
    (cons "done" #f))))

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
