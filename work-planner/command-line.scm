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
             (srfi srfi-171))

;; This function is going to be extended a lot.
(define* (work-item-string-representation item
                                          #:key show-due-date
                                          (date-format "~d/~m/~y ~T"))
  (let ((date-str
         (if show-due-date
             (string-append
              (date->string (work-item-due-date item) date-format)
              ": ")
             "")))
    (string-append date-str (work-item-text item))))
(export work-item-string-representation)

;; The following functions return a list of lines that are expected to be
;; printed onto the screen
(define* (construct-to-be-done-on-date items #:optional (date (current-date)))
  (cons "To be done today:"
        (map (filter (make-filter-work-item-to-be-done-on-date date) items)
             work-item-string-representation)))
(export construct-to-be-done-on-date)

(define* (summary-screen items #:optional (date (current-date)))
  (let ((lines
         (list (construct-to-be-done-on-date items date))))
    (string-join (list-transduce tflatten rcons lines) "\n")))
(export summary-screen)
