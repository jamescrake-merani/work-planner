;; Work Planner
;; Copyright (C) 2022 James Crake-Merani
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (work-planner command-line))
(use-modules (work-planner date-json)
             (work-planner filters)
             (srfi srfi-19)
             (srfi srfi-171)
             (ice-9 format)
             (ice-9 readline))

(define date-template "~d/~m/~Y")

;; This function is going to be extended a lot.
(define* (work-item-string-representation item
                                          #:key show-due-date
                                          (date-format "~d/~m/~y ~T"))
  (let ((date-str
         (if (and show-due-date (work-item-due-date item))
             (string-append
              (date->string (work-item-due-date item) date-format)
              ": ")
             ""))
        (completion-str
         (if (work-item-completed? item)
             "COMPLETED: "
             "")))
    (format #f "~a~a[~d]~a" date-str completion-str (work-item-id item) (work-item-text item))))
(export work-item-string-representation)

(define* (list-item-string-representation item
                                          #:key show-due-date
                                          (date-format "~d/~m/~y ~T"))
  "Represents each work item as a list item (with a '-' before it)"
  (string-append "- " (work-item-string-representation
                       item
                       #:show-due-date show-due-date
                       #:date-format date-format)))
(export list-item-string-representation )

;; The following functions return a list of lines that are expected to be
;; printed onto the screen
(define* (construct-to-be-done-on-date items #:optional (date (current-date)))
  (cons "To be done today:"
        (map list-item-string-representation
             (filter (make-filter-work-item-to-be-done-on-date date) items))))
(export construct-to-be-done-on-date)

(define* (construct-due-in-n-days items #:optional (date (current-date)) (n 7))
  (cons (format #f "Due in the next ~d days" n)
        (map (lambda (i) (list-item-string-representation i #:show-due-date #t))
             (filter (make-filter-work-due-in-n-days n date) items))))
(export construct-due-in-n-days)

(define* (construct-all-items items) ;;TODO: Show due if available.
  (cons "All work items: "
        (map list-item-string-representation
             (sort items (lambda (y x) (< (work-item-id y) (work-item-id x)))))))
(export construct-all-items)

(define-public (show-all-items items)
  (let ((lines (construct-all-items items)))
    (string-append (string-join lines "\n") "\n")))

(define* (summary-screen items #:optional (date (current-date)))
  (let ((lines
         (list (construct-to-be-done-on-date items date)
               (construct-due-in-n-days items date))))
    (string-append (string-join (list-transduce tflatten rcons lines) "\n") "\n")))
(export summary-screen)

(define (prompt-text)
  (let ((proposed-text (readline "Please enter the text for the work item: ")))
    (if (= (string-length proposed-text) 0)
        (begin
          (display "You cannot enter an empty string.")
          (newline)
          (prompt-text))
        proposed-text)))

(define (prompt-due-date) ;;TODO: Inform the user of what the right format is.
  (let ((user-input (readline "Please enter a date in the appropriate format: ")))
    (catch #t
      (lambda ()
        (string->date user-input date-template))
      (lambda (e . args) ;;TODO: Perhaps be more specific about what the error is.
        (display "Incorrect date format")
        (newline)
        (prompt-due-date)))))

(define-public (interactive-create-work-item)
  (work-item
   #:text (prompt-text)
   #:due-date (prompt-due-date)))
