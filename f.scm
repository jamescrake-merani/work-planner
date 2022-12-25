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

(define-module (f)
  #:use-module ((ice-9 binary-ports) #:prefix i9:)
  #:use-module ((ice-9 textual-ports) #:prefix i9:)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module ((f ports) #:prefix p:)
  #:use-module ((f re-exports) #:prefix re:)
  #:export (read-bytes
            read-text
            read-line
            read-lines
            write-bytes
            write-text
            write-line
            write-lines
            ls
            mkdir
            delete
            traverse
            copy
            extension
            no-extension
            replace-extension)
  #:re-export (chown
               chmod
               (rename-file . move)
               link
               symlink
               rmdir))

(define (read-bytes path)
  (let* ((file (open-file path "rb"))
         (bytes (i9:get-bytevector-all file)))
    (close-port file)
    bytes))

(define (read-text path)
  (let* ((file (open-file path "r"))
         (text (i9:get-string-all file)))
    (close-port file)
    text))

(define (read-line path)
  (let* ((file (open-file path "r"))
         (line (p:read-line file)))
    (close-port file)
    line))

(define (read-lines path)
  (let* ((file (open-file path "r"))
         (lines (p:read-lines file)))
    (close-port file)
    lines))

(define* (write-bytes path bytes #:key (append #f))
  (let ((file (open-file path (if append "a" "w"))))
    (i9:put-bytevector file bytes)
    (close-port file)))

(define* (write-text path text #:key (append #f))
  (let ((file (open-file path (if append "a" "w"))))
    (i9:put-string file text)
    (close-port file)))

(define* (write-line path text #:key (append #f))
  (let ((file (open-file path (if append "a" "w"))))
    (i9:put-string file (format #f "~a~%" text))
    (close-port file)))

(define* (write-lines path lines #:key (append #f))
  (let ((file (open-file path (if append "a" "w"))))
    (for-each (lambda (l)
                (i9:put-string file (format #f "~a~%" l)))
              lines)
    (close-port file)))

(define* (mkdir path #:key (parents #f))
  (if (not parents)
      (re:mkdir path)
      (letrec ((is-dir? (lambda (path)
                          (and (file-exists? path) (file-is-directory? path))))
               (create-if-doesnt-exist (lambda (path)
                                         (when (not (is-dir? path))
                                           (re:mkdir path))))
               (mkdir-p (lambda (path segments)
                          (if (null? segments)
                              (create-if-doesnt-exist path)
                              (begin
                                (create-if-doesnt-exist path)
                                (mkdir-p (string-append path file-name-separator-string (car segments)) (cdr segments))))))
               (segments (string-split path #\/))) ;; TODO: split on file-name-separator-string
        (mkdir-p (car segments) (cdr segments)))))

(define* (ls #:optional (dir (getcwd)) #:key (hidden #f))
  (letrec ((d (opendir dir))
           (ls' (lambda (f fs)
                  (if (eof-object? f)
                      fs
                      (ls' (readdir d) (cons f fs))))))
    (let ((files (ls' (readdir d) '())))
      (closedir d)
      (if hidden
          (remove (lambda (x) (or (equal? x "..") (equal? x "."))) files)
          (remove (lambda (x) (equal? (string-ref x 0) #\.)) files)))))

(define* (traverse path f #:key (files-only #f))
  (if (file-is-directory? path)
      (letrec ((contents (ls path #:hidden #t))
               (loop (lambda (contents)
                       (when (not (null? contents))
                         (let ((file (string-append path "/" (car contents))))
                           (traverse file f #:files-only files-only))
                         (loop (cdr contents))))))
        (when (not files-only)
          (f path))
        (loop contents))
      (f path)))

(define (dir-is-empty? path)
  (null? (ls path #:hidden #t)))

(define* (delete path #:optional (recursive #f))
  (if (not (file-is-directory? path))
      (delete-file path)
      (let* ((contents (ls path #:hidden #t))
             (empty (null? contents)))
        (if empty
            (rmdir path)
            (if recursive
                (letrec ((loop (lambda (contents)
                                 (when (not (null? contents))
                                   (delete
                                    (string-append path "/" (car contents))
                                    ;; TODO: replace ^ with smarter path building
                                    recursive)
                                   (loop (cdr contents))))))
                  (loop contents)
                  (rmdir path))
                (throw 'f.scm "Can't delete: directory not empty. Try making the call recursive by
appending a #t at the end."))))))

(define* (copy src dest #:optional (recursive #f))
  (if (file-is-directory? src)
      (let* ((contents (ls src #:hidden #t))
             (empty (null? contents)))
        (if empty
            (mkdir dest)
            (if recursive
                (letrec ((loop (lambda (contents)
                                 (when (not (null? contents))
                                   (copy
                                    (string-append src "/" (car contents))
                                    (string-append dest "/" (car contents))
                                    recursive)
                                   (loop (cdr contents))))))
                  (mkdir dest)
                  (loop contents))
                (throw 'f.scm "Can't copy: directory not empty. Try with making the call recursive
by appending a #t at the end."))))
      (copy-file src dest)))

(define (extension file)
  "Return extension for FILE."
  (let ((ext (string-match ".+\\.(.*)" (basename file))))
    (cond
     ((string-null? file) #f)
     (ext (regexp-substitute #f ext 1))
     (else ""))))

(define (no-extension file)
  "Return FILE path without extension."
  (let ((without-ext (string-match "(.+)\\..*" (basename file))))
    (if without-ext
        (regexp-substitute #f without-ext 1)
        file)))

(define (replace-extension file ext)
  "Replace file extension in FILE with EXT.
EXT can include or exclude the beginning \".\"."
  (let ((ext (cond
              ((string-null? ext) "")
              ((string-match "^\\..+" ext) ext)
              (else (string-append "." ext)))))
    (string-append (no-extension file) ext)))
