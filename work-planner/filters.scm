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

(define-module (work-planner filters))
(use-modules (work-planner date-helpers)
             (work-planner date-json)
             (srfi srfi-19))

;; These functions are designed to be called with the filter
;; function.

;; FIXME: Very long winded title. Can this be reduced?
(define-public (make-filter-work-item-to-be-done-on-date date)
  (lambda (item)
    (same-day? (work-item-date item) date)))

(define-public (make-filter-work-due-in-n-days n)
  (lambda (item)
    (> (days-between-dates (work-item-due-date item) (current-date)))))

(define-public (filter-work-overdue item)
  (>= (days-between-dates (current-date) (work-item-due-date item)) 0))