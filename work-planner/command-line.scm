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
             (work-planner date-helpers)
             (work-planner filters)
             (work-planner colours)
             (srfi srfi-19)
             (srfi srfi-171)
             (ice-9 format)
             (ice-9 readline))

(define date-template "~d/~m/~Y")

(define (default-date-format to-format)
  "Formats TO-FORMAT according to the default format where dates are in the UK
format, and time is shown only when TO-FORMAT is not midnight"
  (let ((base-format "~d/~m/~y"))
    (if (is-midnight? to-format)
        base-format
        (string-append base-format " ~T"))))

;; This function is going to be extended a lot.
(define* (work-item-string-representation item
                                          #:key show-due-date
                                          (date-format 'default))
  "Creates the string representation of ITEM. When SHOW-DUE-DATE is true, the due
date is shown at the beginning of the representation. DATE-FORMAT is the format
used to format the due dates. By default, it uses the default-date-format
function."
  (let ((date-str
         (if (and show-due-date (work-item-due-date item))
             (string-append
              (date->string (work-item-due-date item)
                            (if (eq? date-format 'default) (default-date-format (work-item-due-date item)) date-format))
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
                                          (date-format 'default)
                                          (colours no-colour-scheme))
  "Represents each work item as a list item (with a '-' before it) and colours"
  (string-append (colour-scheme-list-item colours) "- " (work-item-string-representation
                       item
                       #:show-due-date show-due-date
                       #:date-format date-format)
                 reset-colours))
(export list-item-string-representation )

(define (get-representation beginning-text representation-func items-filter items colours)
  "Gets the representation of a collection of work items.
BEGINNING-TEXT is the header of the collection.
ITEMS-FILTER is used to filter the work items.
ITEMS are the work items that are being represented.
COLOURS is the colour the scheme to use."
  (let ((filtered (filter items-filter items)))
    (if (null? filtered)
        '()
        (cons (string-append (colour-scheme-header colours) beginning-text reset-colours)
              (map representation-func filtered)))))


;; The following functions return a list of lines that are expected to be
;; printed onto the screen
(define* (construct-to-be-done-on-date items #:optional (date (current-date)) (colours no-colour-scheme))
  (get-representation "To be done today:"
                      (lambda (i) (list-item-string-representation i #:show-due-date #t #:colours colours))
                      (make-filter-designated-on date)
                      items
                      colours))
(export construct-to-be-done-on-date)

(define* (construct-due-in-n-days items #:optional (date (current-date)) (colours no-colour-scheme) (n 7))
  (get-representation (format #f "Due in the next ~d days:" n)
                      (lambda (i) (list-item-string-representation i #:show-due-date #t #:colours colours))
                      (make-filter-work-due-in-n-days n date)
                      items
                      colours))
(export construct-due-in-n-days)

(define* (construct-all-items items #:optional colours)
  (get-representation "All work items: "
                      (lambda (i) (list-item-string-representation i #:show-due-date #t #:colours colours))
                      identity
                      (sort items (lambda (y x) (< (work-item-id y) (work-item-id x))))
                      colours))
(export construct-all-items)

(define* (construct-overdue items #:optional (date (current-date)) (colours no-colour-scheme))
  (get-representation  "OVERDUE!!:"
                       (lambda (i) (list-item-string-representation i #:show-due-date #t #:colours colours))
                       (make-filter-work-overdue date)
                       items
                       colours))
(export construct-overdue)

(define* (construct-undesignated items #:optional (date (current-date)) (colours no-colour-scheme))
  (get-representation "The following items have not been designated:"
                      (lambda (i) (list-item-string-representation i #:show-due-date #t #:colours colours))
                      (make-filter-undesignated date)
                      items
                      colours))

(define-public (show-all-items items)
  "Creates a representation with all work items."
  (let ((lines (construct-all-items items default-colour-scheme)))
    (string-append (string-join lines "\n") "\n")))

(define (item-lines->string lines)
  "Evaluate to a single string containing all the work item lines."
  (string-append (string-join (list-transduce tflatten rcons lines) "\n") "\n"))

(define* (summary-screen items #:optional (date (current-date)))
  "This is the screen which contains multiple different collections as defined
below. These are deemed to be the most relevent to the user. In the future, it
will be possible to customise this. "
  (let* ((colour-scheme default-colour-scheme)
         (lines
         (filter ;; Get rid of any empty ones.
          (lambda (l) (not (null? l)))
          (list (construct-to-be-done-on-date items date colour-scheme)
               (construct-due-in-n-days items date colour-scheme)
               (construct-overdue items date colour-scheme)
               (construct-undesignated items date colour-scheme))
          )))
    (if (null? lines)
        "There is nothing to report on the summary screen. Enjoy!
P.S: If you want to see all the work items, just do work-planner --all\n"
        (string-append (item-lines->string lines) "\n"))))
(export summary-screen)

(define-public (view-day items day)
  "View all the ITEMS designated to be completed on DAY."
  (let* ((date-str (date->string day (default-date-format day)))
         (representation
         (get-representation
          (format #f "The items designated for ~a are:" date-str)
          (lambda (i) (list-item-string-representation i #:show-due-date #t #:colours default-colour-scheme))
          (make-filter-designated-on day)
          items
          default-colour-scheme)))
    (if (null? representation)
        (format #f "There are no items designated for ~a\n" date-str)
        (item-lines->string representation))))

(define (prompt-text)
  "Prompt for text for a work item, following the normal validation rules."
  (let ((proposed-text (readline "Please enter the text for the work item: ")))
    (if (= (string-length proposed-text) 0)
        (begin
          (display "You cannot enter an empty string.")
          (newline)
          (prompt-text))
        proposed-text)))

(define (prompt-due-date) ;;TODO: Inform the user of what the right format is.
  "Prompt for a due date which must be in the right format."
  (let ((user-input (readline "Please enter a date in the appropriate format (empty for no due date): ")))
    (catch #t
      (lambda ()
        (if (string=? user-input "")
            #f
            (string->date user-input date-template)))
      (lambda (e . args) ;;TODO: Perhaps be more specific about what the error is.
        (display "Incorrect date format")
        (newline)
        (prompt-due-date)))))

(define-public (interactive-create-work-item)
  "Create a work item by prompting the user for the different values the new work
item will have."
  (work-item
   #:text (prompt-text)
   #:due-date (prompt-due-date)))

(define-public (purge-work-items items)
  "Evaluate to a new list where purgable items in ITEMS have been filtered out"
  (filter (make-filter-purgable) items))
