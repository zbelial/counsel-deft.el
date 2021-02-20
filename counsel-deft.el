;; counsel-deft.el --- Browse files in Emacs using ivy  -*- lexical-binding: t; -*-

;; Filename: counsel-deft.el
;; Description: Browse files in Emacs using ivy
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2020, zbelial, all rights reserved.
;; Created: 2021-02-20 17:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/counsel-deft.el
;; Package-Requires: ((ivy "0.13.0"))
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
(require 'compile) ;; compilation-info-face, compilation-line-face

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


;;; Variables
(defvar counsel--dir-timestamps (make-hash-table)
  "A Hash table used to keep diretory's timestamp of latest modification."
  )

(defvar counsel--dir-subdirs (make-hash-table)
  "A Hash table used to keep diretory's subdirs."
  )

(defvar counsel--dir-files (make-hash-table)
  "A Hash table used to keep diretory's files(no subdir)."
  )


(defface counsel-deft-annotation-face nil
  "Face used for annotation."
  :group 'counsel-deft)

(defun counsel-deft-bookmarks-all()
  (let (all bms)
    (dolist (buf (buffer-list))
      (setq bms (counsel-deft-bookmarks-in-buffer buf))
      (when bms
        (setq all (append all bms))))
    all))

(defun counsel-deft-bookmarks-in-buffer (&optional buf)
  "Gets a list of bookmarks in BUF, which can be a string or a buffer."
  (let ((buf (or buf (buffer-name)))
        (mklist (lambda (x) (if (listp x) x (list x)))))
    (funcall mklist
             (with-current-buffer buf
               (apply 'append
                      (mapcar mklist (remove nil (bm-lists))))))))

(defun counsel-deft-candidate-transformer (bm)
  "Return a string displayed in counsel buffer."
  (let ((bufname (plist-get bm :bufname))
        (lineno (plist-get bm :lineno))
        (content (plist-get bm :content))
        (annotation (plist-get bm :annotation)))
    (format "%s:%s:%s%s"
            (propertize bufname 'face compilation-info-face)
            (propertize lineno 'face compilation-line-face)
            content
            (if (s-blank? annotation) ""
              (concat "\n  "
                      (propertize annotation 'face
                                  'counsel-deft-annotation-face))))))

(defun counsel-deft-transform-to-candicate (bm)
  "Convert a BM to a CANDICATE."
  (let ((current-buf (overlay-buffer bm)))
    (with-current-buffer current-buf
      (let* ((start (overlay-start bm))
             (end (overlay-end bm))
             (bufname (buffer-name current-buf))
             (annotation (overlay-get bm 'annotation))
             (lineno (line-number-at-pos start)))
        (unless (< (- end start) 1)
          (list 
           :bufname bufname
           :lineno (int-to-string lineno)
           :content (buffer-substring-no-properties start (1- end))
           :annotation annotation))))))


(defun counsel-deft-collector (&optional all)
  (let ((bms (mapcar #'counsel-deft-transform-to-candicate
                     (if all
                         (counsel-deft-bookmarks-all)
                       (counsel-deft-bookmarks-in-buffer)))))
    (delq nil (mapcar #'(lambda (bm)
                          (cons (counsel-deft-candidate-transformer bm) bm))
                      bms))))

(defun counsel-deft-goto-line (linum &optional buf)
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line (1- linum)))))

(defun counsel-deft-jump (cand)
  (let* ((bm (cdr cand))
         (bufname (plist-get bm :bufname))
         (lineno (plist-get bm :lineno)))
    (switch-to-buffer bufname)
    (counsel-deft-goto-line (string-to-number lineno))
    (recenter)))


(defun counsel-deft ()
  (interactive)
  (let* ((all (if (equal current-prefix-arg nil)
                  nil
                t))
         (bms (counsel-deft-collector all))
         (linum (line-number-at-pos))
         (preselect 0))
    (dolist (bm bms)
      (when (< (string-to-number (plist-get (cdr bm) :lineno)) linum)
        (setq preselect (1+ preselect)))
      )
    (ivy-read "Visible bookmarks: " bms
              :preselect preselect
              :action '(1
                        ("o" counsel-deft-jump "jump to bookmark")
                        )
              :caller 'counsel-deft
              )))


(defun counsel-deft-sorter (&optional l r)
  (let* ((lr (cdr l))
         (rr (cdr r))
         (lb (plist-get lr :bufname))
         (rb (plist-get rr :bufname))
         (lp (string-to-number (plist-get lr :lineno)))
         (rp (string-to-number (plist-get rr :lineno))))
    (or (string< lb rb)
        (or (and (string= lb rb)
                 (< lp rp))))))

(ivy-configure 'counsel-deft
  :sort-fn #'counsel-deft-sorter)

(provide 'counsel-deft)
