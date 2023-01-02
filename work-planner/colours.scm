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

(define-module (work-planner colours))

(define* (colour-scheme #:key header list-item)
  (list
   (cons "header" header)
   (cons "list-item" list-item)))
(export colour-scheme)

(define-public (colour-scheme-header scheme)
  (assoc-ref scheme "header"))

(define-public (colour-scheme-list-item scheme)
  (assoc-ref scheme "list-item"))

(define-public colour-red "\x1b[31m")
(define-public colour-yellow "\x1b[33m")
(define-public reset-colours "\x1b[0m")

(define-public default-colour-scheme
  (colour-scheme
   #:header colour-red
   #:list-item colour-yellow))

(define-public no-colour-scheme
  (colour-scheme
   #:header ""
   #:list-item ""))
