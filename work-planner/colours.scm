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
