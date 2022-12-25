;; Copyright 2020 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (f ports)
  #:use-module ((ice-9 binary-ports) #:prefix i9:)
  #:use-module ((ice-9 textual-ports) #:prefix i9:)
  #:export (read-line
            read-lines))

(define (read-line port)
  (i9:get-line port))

(define (read-lines port)
  (letrec ((loop (lambda (l ls)
                   (if (eof-object? l)
                       ls
                       (loop (i9:get-line port) (cons l ls))))))
    (reverse (loop (i9:get-line port) '()))))
