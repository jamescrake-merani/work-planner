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
(use-modules (srfi srfi-19))
;; The purpose of this module is to be able to serialise, and
;; deserialise dates from the SRFI-19 module to, and from JSON.

(define (a-list->date a-lst)
  (make-date
   (assoc-ref a-lst "nsecs")
   (assoc-ref a-lst "seconds")
   (assoc-ref a-lst "minutes")
   (assoc-ref a-lst "hours")
   (assoc-ref a-lst "day")
   (assoc-ref a-lst "month")
   (assoc-ref a-lst "year")
   (assoc-ref a-lst "zone-offset")))

(define (date->a-list date)
  (list
   (cons "nsecs" (date-nanosecond date))
   (cons "seconds" (date-second date))
   (cons "minutes" (date-minute date))
   (cons "hours" (date-hour date))
   (cons "day" (date-day date)) ;; I don't know why these have different names.
   (cons "month" (date-month date))
   (cons "year" (date-year date))
   (cons "zone-offeset" (date-zone-offset date))))
