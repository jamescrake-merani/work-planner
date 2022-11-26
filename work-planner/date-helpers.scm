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

(define-module (work-planner date-helpers))
(use-modules (srfi srfi-19))

(define-public (same-day? date1 date2)
  (and ;; These first arguments are to make sure both dates aren't #f.
   date1
   date2
   (= (date-day date1) (date-day date2))
   (= (date-month date1) (date-month date2))
   (= (date-year date1) (date-year date2))))

(define-public (days-between-dates date1 date2)
  (apply -
         (map date->julian-day (list date1 date2))))

(define-public (is-midnight? d)
  (and
   (= (date-second d) 0)
   (= (date-minute d) 0)
   (= (date-hour d) 0)))
