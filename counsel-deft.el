;; counsel-deft.el --- Browse files in Emacs using ivy  -*- lexical-binding: t; -*-

;; Filename: counsel-deft.el
;; Description: Browse files in Emacs using ivy
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2021, zbelial, all rights reserved.
;; Created: 2021-02-20 17:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/counsel-deft.el
;; Package-Requires: ((ivy "0.13.0") (f "0.20.0"))
;; Keywords:
;; Compatibility: GNU Emacs 27.1
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(require 'ivy)
(require 'f)
(require 'eww)

(defgroup counsel-deft nil
  "Browse files in Emacs using ivy."
  :prefix "counsel-deft-" :group 'counsel)

;;; Custom
(defcustom counsel-deft-directory user-emacs-directory
  "The default directory used to store topic thread data."
  :type  'string
  :group 'counsel-deft)

(defcustom counsel-deft-extensions
  '("txt" "text" "md" "markdown" "org")
  "Files with these extensions will be listed."
  :type '(repeat string)
  :group 'counsel-deft)

(defcustom counsel-deft-file-handler-alist
  '(("html" . counsel-deft-open-html-with-eww))
  "An alist associating open handler with file extention."
  :type '(alist
          :key-type (string)
          :value-type (function)))

;;; Variables
(defvar counsel-deft--dir-timestamps (make-hash-table :test 'equal)
  "A Hash table used to keep diretory's timestamp of latest modification."
  )

(defvar counsel-deft--dir-files (make-hash-table :test 'equal)
  "A Hash table used to keep diretory's files(no subdir)."
  )

(defvar counsel-deft--dirs '()
  "All diretories in a given directory.")

(defun counsel-deft--file-modified-time (file)
  "File's latest modification time."
  (if (f-exists-p file)
      (time-convert (file-attribute-modification-time (file-attributes file)) 'integer)
    0))

(defun counsel-deft--dir-is-modified (dir)
  "Check whether dir is modified."
  (let ((tm (gethash (f-full dir) counsel-deft--dir-timestamps)))
    (if tm
        (> (counsel-deft--file-modified-time dir) tm)
        t)))

(defun counsel-deft--file-not-hidden? (file)
  "Return t if file is not hidden."
  (not (string= (substring (f-filename file) 0 1) ".")))

(defun counsel-deft--all-subdirs (dir)
  "All subdirs."
  (setq counsel-deft--dirs '())
  (when (f-exists? (f-full dir))
    (add-to-list 'counsel-deft--dirs (f-full dir))
    (counsel-deft--all-subdirs-recursive dir))
  counsel-deft--dirs)

(defun counsel-deft--all-subdirs-recursive (dir)
  "All subdirs, in recursive way."
  (when (f-exists? dir)
    (let ((dirs (f-directories dir #'counsel-deft--file-not-hidden? nil)))
      (when dirs
        (setq counsel-deft--dirs (append counsel-deft--dirs (mapcar #'f-full dirs)))
        (dolist (dir dirs)
          (counsel-deft--all-subdirs-recursive dir))))))

(defun counsel-deft--file-filter (file)
  "Filter out files which's ext is not in `counsel-deft-extensions'."
  (let ((ext (f-ext file)))
    (member ext counsel-deft-extensions)))

(defun counsel-deft--all-files (dir)
  "All files in directory and sub-directories, recursively."
  (let (all-files files)
    (counsel-deft--all-subdirs dir)
    (when counsel-deft--dirs
      (dolist (dir counsel-deft--dirs)
        (setq files '())
        (if (counsel-deft--dir-is-modified dir)
            (progn
              (message "directory %s is modified" dir)
              ;; (setq files (seq-filter #'counsel-deft--file-filter (f-files dir #'counsel-deft--file-not-hidden? nil)))
              (setq files (f-files dir #'counsel-deft--file-not-hidden? nil))
              (puthash dir (counsel-deft--file-modified-time dir) counsel-deft--dir-timestamps)
              (puthash dir files counsel-deft--dir-files)
              )
          (message "directory %s is not modified" dir)
          (setq files (gethash dir counsel-deft--dir-files)))
        (setq all-files (append all-files files))))
    (seq-filter #'counsel-deft--file-filter all-files)))

(defun counsel-deft-clear-cache ()
  "Clear all caches. Used primarily for test."
  (interactive)
  (setq counsel-deft--dirs '())
  (setq counsel-deft--dir-files (make-hash-table :test 'equal))
  (setq counsel-deft--dir-timestamps (make-hash-table :test 'equal))
  )

(defun counsel-deft--open (file)
  "Open file, according to `counsel-deft-file-handler-alist'."
  (let* ((ext (f-ext file))
         (handler (assoc ext counsel-deft-file-handler-alist)))
    (if handler
        (funcall (cdr handler) file)
      (find-file file))))

(defun counsel-deft ()
  "Browse files in `counsel-deft-directory'."
  (interactive)
  (let* ((all-files (counsel-deft--all-files counsel-deft-directory)))
    (ivy-read "All files: " all-files
              :action '(1
                        ("o" counsel-deft--open "open file")
                        )
              :caller 'counsel-deft)
    )
  )

(defun counsel-deft-interactive ()
  "Browse files in a directory selected interactively."
  (interactive)
  (let* ((dir (read-directory-name "Browse dir: "))
         (all-files (counsel-deft--all-files dir)))
    (ivy-read "All files: " all-files
              :action '(1
                        ("o" counsel-deft--open "open file")
                        )
              :caller 'counsel-deft)
    )
  )

(defun counsel-deft-open-html-with-eww (file)
  "Open html with eww."
  (interactive)
  (eww-browse-url (concat "file://" file) t)
  )


(provide 'counsel-deft)
