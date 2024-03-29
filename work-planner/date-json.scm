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

(define-module (work-planner date-json))
(use-modules (srfi srfi-19)
             (srfi srfi-1)
             (json))
;; The purpose of this module is to be able to serialise, and
;; deserialise dates from the SRFI-19 module to, and from JSON.

(define-public (a-list->date a-lst)
  "Converts the association list A-LST to an equivalent SRFI-19 date"
  (if a-lst
      (make-date
       (assoc-ref a-lst "nsecs")
       (assoc-ref a-lst "seconds")
       (assoc-ref a-lst "minutes")
       (assoc-ref a-lst "hours")
       (assoc-ref a-lst "day")
       (assoc-ref a-lst "month")
       (assoc-ref a-lst "year")
       (assoc-ref a-lst "zone-offset"))
      #f))

(define-public (date->a-list date)
  "Converts the SRFI-19 DATE to an equivalent assocation list."
  (if date
      (list
       (cons "nsecs" (date-nanosecond date))
       (cons "seconds" (date-second date))
       (cons "minutes" (date-minute date))
       (cons "hours" (date-hour date))
       (cons "day" (date-day date)) ;; I don't know why these have different names.
       (cons "month" (date-month date))
       (cons "year" (date-year date))
       (cons "zone-offset" (date-zone-offset date)))
      #f))

(define* (work-item #:key id text due-date designated-completion-dates completed)
  "Creates a new work item. All parameters are optional:
ID: The unique identifier for the item
DUE-DATE: The date at which the new work item will be due.
DESIGNATED-COMPLETION-DATES: The dates that the work item has been designated
for completion "
  (list
   (cons "id" id)
   (cons "text" text)
   (cons "due-date" due-date)
   (cons "designated-completion-dates" designated-completion-dates)
   (cons "completed" completed)))
(export work-item)

(define* (add-work-item-to-lst item items-lst #:optional (id 1))
  "Fills in the id to the first one available"
  (if (any (lambda (item) (= (work-item-id item) id)) items-lst)
      (add-work-item-to-lst item items-lst (1+ id))
      (let ((to-add (cons (cons "id" id) (alist-delete "id" item))))
        (cons to-add items-lst))))
(export add-work-item-to-lst)
;; TODO: Right now designated completion dates is being used like a single
;; value but I think its meant to be a list
(define-public (work-item-fix-date item)
  "After items are loaded from JSON, dates will appear as association lists. This
function converts the dates in ITEM to srfi-19 dates"
  (append (list (cons "due-date" (a-list->date (assoc-ref item "due-date")))
                (cons "designated-completion-dates" (a-list->date (assoc-ref item "designated-completion-dates")))
                (cons "completed" (if (assoc-ref item "completed") (a-list->date (assoc-ref item "completed")) #f)))
          (alist-delete "due-date" (alist-delete "designated-completion-dates" item))))
;; TODO: These two functions are so similar that they can probably be combined
;; into one.
(define-public (work-item-dates-assoc item)
  "Converts the dates in ITEM from srfi-19 dates to association lists."
  (append (list (cons "due-date" (date->a-list (assoc-ref item "due-date")))
                (cons "designated-completion-dates" (date->a-list (assoc-ref item "designated-completion-dates")))
                (cons "completed" (date->a-list (assoc-ref item "completed"))))
          (alist-delete "completed" (alist-delete "due-date" (alist-delete "designated-completion-dates" item)))))

(define-public (json-string->work-item str)
  "Converts STR with JSON to a work item association list."
  (work-item-fix-date (vector->list (json-string->scm str))))

(define-public (work-item->json-string item)
  "Converts ITEM into a string with JSON."
  (scm->json-string (work-item-dates-assoc item)))

(define-public (json-string->work-items str)
  "Converts STR containing JSON of a collection of work items into a list of work
items all represented as association lists"
  (map work-item-fix-date (vector->list (json-string->scm str))))

(define-public (work-items->json-string items)
  "Converts ITEMS, which is a list of work items all represented as association
lists into JSON of a collection of the same of work items."
  (scm->json-string (list->vector (map work-item-dates-assoc items))))

;; A work item does not have to have all these fields - the caller
;; can expect a #f value if the field doesn't exist. However, callers
;; may assume certain fields exist, and will of course error when they
;; don't.
;;
;; See documentation of the work-item function for descriptions of all the field

(define-public (work-item-id item)
  (assoc-ref item "id"))

(define-public (work-item-text item)
  (assoc-ref item "text"))

(define-public (work-item-due-date item)
  (assoc-ref item "due-date"))

;; This is a list of dates, not just one single date. The user might
;; want to designated dates to work at a certain task without necessarily
;; finishing it in one day.
(define-public (work-item-designated-completion-dates item) ;;TODO: Might rename this. Not decided yet.
  (assoc-ref item "designated-completion-dates"))

(define-public (work-item-completed? item)
  (assoc-ref item "completed"))

(define-public (work-item-complete item)
  (let ((new-value (if (assoc-ref item "completed") #f (current-date))))
    (cons (cons "completed" new-value) (alist-delete "completed" item))))

(define-public (work-item-replace old-item new-item items)
  "Replace OLD-ITEM in ITEMS with NEW-ITEM. The comparison is done via the id
value of the association list therefore each work item in ITEMS must have a
unique id."
  (cons new-item
        (filter (lambda (i)
                  (not (= (work-item-id i) (work-item-id old-item)))) items)))

(define-public (designate-work-item-date item date)
  "Designate ITEM to be completed on DATE."
  (cons (cons "designated-completion-dates" date) (alist-delete "designated-completion-dates" item)))

(define-public (get-work-item items id)
  "Get a work item in ITEMS based on ID."
  (find (lambda (item) (= (work-item-id item) id)) items))
