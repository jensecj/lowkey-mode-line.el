;;; lowkey-mode-line.el --- Simple, lowkey mode-line replacement

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Version: 0.2
;; Package-Version: 20181214
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (s "1.12.0"))
;; Keywords: mode-line

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimal mode-line replacement, with utility functions for configuration.

;;; Code:

(require 's)
(require 'dash)

(defface lml-buffer-name-face
  '((t (:background "grey20")))
  "Face used for the `buffer-name' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-position-face
  '((t (:background "grey25")))
  "Face used for the `position' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-major-mode-face
  '((t (:background "grey30")))
  "Face used for the `major-mode' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-minor-modes-face
  '((t (:background "grey30")))
  "Face used for the `minor-modes' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-vc-face
  '((t (:background "grey25")))
  "Face used for the `version-control' part of the mode-line."
  :group 'lowkey-mode-line)

(cl-defun lml--mode-line-string (str &key face pad-left pad-right pad)
  "Create a string suitable for displaying in mode-line."
  (when pad-left (setq str (s-join "" (list (s-repeat pad-left " ") str))))
  (when pad-right (setq str (s-join "" (list str (s-repeat pad-right " ")))))
  (when pad (setq str (s-join "" (list (s-repeat pad " ") str (s-repeat pad " ")))))

  (propertize str 'face face))

(defun lml--mode-line-fill (reserve &optional face)
  "Fill an area of `reserve' in the mode-line."
  (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve))) 'face face))

(defun lml--mode-line-rightmost (str &optional face)
  "Display `str' at the right-most side of the mode-line."
  (when (s-blank-str? str) (setq str ""))

  (s-join ""
          (list (lml--mode-line-fill (length str) face)
                str)))

(defun lml--mode-line-position-string ()
  "String for position part of the mode-line. If visition a `pdf'
buffer, and having `pdf-tools', use pdf pages and position."
  (if (and (fboundp 'pdf-util-pdf-buffer-p)
           (fboundp 'pdf-view-current-page)
           (pdf-util-pdf-buffer-p))
      (format "%s / %s " (pdf-view-current-page) (pdf-info-number-of-pages))
    "(%p) %4l :%3c"))

(defun lml--mode-line-active-minor-modes-string ()
  "String of active minor modes for the current buffer."
  (->> (format-mode-line minor-mode-alist)
       (s-split " ")
       (-map #'s-trim)
       (-remove #'(lambda (s) (< (length s) 2)))
       (s-join " ")))

(defun lml--mode-line-vc-string ()
  "String with version control info for the current file, if any."
  (if (not vc-mode)
      ""
    (let* ((this-file (buffer-file-name))
           (backend (vc-backend this-file))
           (revision (vc-working-revision this-file backend))
           (branch (or (vc-git--symbolic-ref this-file)
                       (substring revision 0 7)))
           (state (vc-state this-file)))
      (format "%s %s (%s)" backend branch state))))

;;;###autoload
(defun lowkey-mode-line-enable ()
  (interactive)
  "Enable the lowkey-mode-line."
  (setq-default
   mode-line-format
   '("%e"
     (:eval (lml--mode-line-string (buffer-name)
                                   :face 'lml-buffer-name-face
                                   :pad-left 2 :pad-right 5))
     (:eval (lml--mode-line-string
             (lml--mode-line-position-string)
             :face 'lml-position-face :pad 2))
     (:eval (lml--mode-line-string
             (format "%s" major-mode)
             :face 'lml-major-mode-face :pad 2))
     (:eval (lml--mode-line-string
             (lml--mode-line-active-minor-modes-string)
             :face 'lml-minor-modes-face :pad 1))
     (:eval (lml--mode-line-rightmost
             (lml--mode-line-string (lml--mode-line-vc-string)
                                    :face 'lml-vc-face :pad 2)
             'lml-major-mode-face)))))

(provide 'lowkey-mode-line)
