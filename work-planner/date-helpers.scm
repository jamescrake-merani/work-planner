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

(define weekdays-alist
  '(("SUNDAY" . 0)
    ("MONDAY" . 1)
    ("TUESDAY" . 2)
    ("WEDNESDAY" . 3)
    ("THURSDAY" . 4)
    ("FRIDAY" . 5)
    ("SATURDAY" . 6)))

(define-public (same-day? date1 date2)
  "Works out if DATE1, and DATE2 are on the same day regardless of what exact time
they represent."
  (and ;; These first arguments are to make sure both dates aren't #f.
   date1
   date2
   (= (date-day date1) (date-day date2))
   (= (date-month date1) (date-month date2))
   (= (date-year date1) (date-year date2))))

(define (date-remove-time d)
  "Removes the time component of D. This is acheived by creating a new date with
the time component of D set to midnight"
  (make-date 0 0 0 0 (date-day d) (date-month d) (date-year d) 0))

(define* (days-between-dates date1 date2 #:optional truncate?)
  "Calculates the amount of days between DATE1, and DATE2. The optional TRUNCATE?
parameter performs the calculation on truncated froms of DATE1, and DATE2"
  (let* ((dates-lst (list date1 date2))
         (times-removed (map date-remove-time dates-lst))
         (times (map date->time-utc times-removed))
         (seconds (time-second (apply time-difference times))))
    (/ seconds (* 24 3600))))
(export days-between-dates)

(define-public (is-midnight? d)
  "Is the time of D exactly midnight?"
  (and
   (= (date-second d) 0)
   (= (date-minute d) 0)
   (= (date-hour d) 0)))

(define* (past-date? date1 date2 #:optional (inclusive #t))
  "Calculates whether DATE2 is prior to DATE1. When INCLUSIVE is false, the
function will evaluate to false if the two dates fall on the same day."
  (let ((comparer (if inclusive >= >)))
    (comparer (days-between-dates date1 date2 #t) 0)))
(export past-date?)

(define* (days-until-weekday until-weekday #:optional (d (current-date)))
  "Calculate the amount of days until the next occurance of UNTIL-WEEKDAY after
the date D. UNTIL-WEEKDAY is represented as an integer where 0=Sunday, 1=Monday
et cetera."
  (let ((d-week-number (date-week-day d)))
    (if (>= d-week-number until-weekday)
        (+ (- 7 d-week-number) until-weekday)
        (- until-weekday d-week-number))))
(export days-until-weekday)

(define-public (add-days d days)
  "Add DAYS onto D"
  (julian-day->date (+ days (date->julian-day d))))


(define-public (parse-date date-str)
  "Parse DATE-STR into a SRFI-19 date."
  (let* ((capitalised-date (string-upcase date-str))
        (day-num (assoc-ref weekdays-alist capitalised-date)))
    (cond
     ((string=? capitalised-date "TODAY") (date-remove-time (current-date)))
     (day-num (date-remove-time (add-days (current-date) (days-until-weekday day-num))))
     (else (string->date date-str "~d/~m/~Y")))))
