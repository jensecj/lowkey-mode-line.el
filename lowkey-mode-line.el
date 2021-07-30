;;; lowkey-mode-line.el --- Simple, lowkey mode-line replacement -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Version: 0.4.0
;; Package-Version: 20210730
;; Package-Requires: ((emacs "28.0.0.50") (dash "2.14.1") (s "1.12.0"))
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

(require 'vc)

(require 's)
(require 'dash)

;; TODO: use `with-eval-after-load' to load pdf-tools config
(require 'pdf-tools nil 'noerror)
(require 'pdf-view nil 'noerror)
(require 'pdf-info nil 'noerror)

(defface lml-buffer-face
  '((t (:background "grey20")))
  "Face used for the `buffer-name' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-buffer-face-inactive
  '((t (:background "grey20")))
  "Inactive variant of `lml-buffer-face'"
  :group 'lowkey-mode-line)

(defface lml-position-face
  '((t (:background "grey25")))
  "Face used for the `position' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-position-face-inactive
  '((t (:background "grey20")))
  "Inactive variant of `lml-position-face'"
  :group 'lowkey-mode-line)

(defface lml-major-mode-face
  '((t (:background "grey30")))
  "Face used for the `major-mode' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-major-mode-face-inactive
  '((t (:background "grey20")))
  "Inactive variant of `lml-major-mode-face'"
  :group 'lowkey-mode-line)

(defface lml-minor-modes-face
  '((t (:background "grey30")))
  "Face used for the `minor-modes' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-minor-modes-face-inactive
  '((t (:background "grey20")))
  "Inactive variant of `lml-minor-modes-face'"
  :group 'lowkey-mode-line)

(defface lml-filler-face
  '((t (:background "grey30")))
  "Face used for the filler part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-filler-face-inactive
  '((t (:background "grey20")))
  "Inactive variant of `lml-filler-face'"
  :group 'lowkey-mode-line)

(defface lml-vc-face
  '((t (:background "grey20")))
  "Face used for the `version-control' part of the mode-line."
  :group 'lowkey-mode-line)

(defface lml-vc-face-inactive
  '((t (:background "grey20")))
  "Inactive variant of `lml-vc-face'"
  :group 'lowkey-mode-line)

(defvar lml--default-mode-line-format nil
  "Default value, before enabling `lowkey-mode-line', used for
  resetting.")

;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun lml--mode-line-string (str &key face pad-left pad-right pad)
  "Create a string suitable for displaying in mode-line."
  (when pad-left (setq str (s-join "" (list (s-repeat pad-left " ") str))))
  (when pad-right (setq str (s-join "" (list str (s-repeat pad-right " ")))))
  (when pad (setq str (s-join "" (list (s-repeat pad " ") str (s-repeat pad " ")))))
  (propertize str 'face face))

(defun lml--mode-line-fill (reserve &optional face)
  "Fill an area of `reserve' in the mode-line."
  (propertize " " 'display
              `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun lml--mode-line-rightmost (str &optional face)
  "Display `str' at the right-most side of the mode-line."
  (when (s-blank-str? str) (setq str ""))

  (s-join ""
          (list (lml--mode-line-fill (length str) face)
                str)))

(defun lml--record-selected-window ()
  "Store the currently selected window every time we change it."
  (setq lml--selected-window (selected-window)))

(defun lml--update-all ()
  "Force update mode-lines when buffers change."
  (force-mode-line-update t))

(defun lml--in-selected-window-p ()
  "Return whether the context of this function call is from the
currently selected window."
  (eq (selected-window) lml--selected-window))

(defun lml--active-or-inactive-face (active-face inactive-face)
  "Pick a face based on whether the mode-line is active or
inactive."
  (if (lml--in-selected-window-p)
      active-face
    (if (facep inactive-face)
        inactive-face
      active-face)))

;;;;;;;;;;;;;;;;;;;;;
;; mode line parts ;;
;;;;;;;;;;;;;;;;;;;;;

(defun lml--buffer-string ()
  "String for the `buffer' part of the mode-line."
  (let ((b (buffer-name)))
    ;; truncate buffer-names if window is small
    (when (and (< (window-width) 100)
               (> (length b) (/ (window-width) 3)))
      (setq b (format "…%s"
                      (substring b (- (length b) (/ (window-width) 3))))) b)

    ;; don't indicate if special buffers are dirty
    (unless (and (s-starts-with-p "*" b) (s-ends-with-p "*" b))
      (when (buffer-modified-p)
        (setq b (format "*%s" b))))

    (when buffer-read-only
      (setq b (format "[%s]" b)))

    b))

(defun lml-buffer ()
  "`Buffer' part of the mode-line."
  (lml--mode-line-string
   (lml--buffer-string)
   :face (lml--active-or-inactive-face
          'lml-buffer-face 'lml-buffer-face-inactive)
   :pad-left 2 :pad-right 3))

(defun lml-narrowed ()
  "String indicator for if the buffer is narrowed."
  (lml--mode-line-string
   (if (buffer-narrowed-p) "<narrow>" "")
   :face (lml--active-or-inactive-face
          'lml-buffer-face 'lml-buffer-face-inactive)))

(defun lml-remote ()
  "String indicator for if the buffer is narrowed."
  (lml--mode-line-string
   (if-let ((file (buffer-file-name))
            (remote? (file-remote-p (buffer-file-name))))
       "<remote>"
     "")
   :face (lml--active-or-inactive-face
          'lml-buffer-face 'lml-buffer-face-inactive)))

(defun lml--position-string ()
  "String for `position' part of the mode-line. If visition a
`pdf'-buffer, and having `pdf-tools', use pdf pages as
position."
  (if (and (fboundp #'pdf-util-pdf-buffer-p)
           (fboundp #'pdf-view-current-page)
           (pdf-util-pdf-buffer-p))
      (format "%s / %s " (pdf-view-current-page) (pdf-info-number-of-pages))
    (let ((percentage "(%p)")
          (this-line "%4l")
          ;; (last-line (window-buffer-height (selected-window)))
          (last-line 0)
          (column "%3c"))
      (format "%s %s :%s" percentage this-line column))))

(defun lml-position ()
  "`Position' part of the mode-line"
  (lml--mode-line-string
   (lml--position-string)
   :face (lml--active-or-inactive-face
          'lml-position-face 'lml-position-face-inactive)
   :pad 2))

(defun lml--major-mode-string ()
  "String for the `major-mode' part of the mode-line."
  (s-replace "-mode" "" (format "%s" major-mode)))

(defun lml-major-mode ()
  "`Major-mode' part of the mode-line"
  (lml--mode-line-string
   (lml--major-mode-string)
   :face (lml--active-or-inactive-face
          'lml-major-mode-face 'lml-major-mode-face-inactive)
   :pad 2))

(defun lml--minor-modes-string ()
  "String of active minor modes for the current buffer."
  ;; propertizing `minor-mode-alist' is wonky, and leaves extra spaces a lot of
  ;; places, fix it by doing it manually.
  (->> (format-mode-line minor-mode-alist)
       (s-split " ")
       (-map #'s-trim)
       (-remove #'(lambda (s) (< (length s) 2)))
       (s-join " ")))

(defun lml-minor-modes ()
  "`Minor-modes' part of the mode-line."
  (lml--mode-line-string
   (lml--minor-modes-string)
   :face (lml--active-or-inactive-face
          'lml-minor-modes-face 'lml-minor-modes-face-inactive)
   :pad 1))

(defun lml--vc-string ()
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

(defun lml-vc ()
  "`Version-control' part of the mode-line."
  (let* ((vc-string (lml--vc-string))
         (vc-face (lml--active-or-inactive-face 'lml-vc-face 'lml-vc-face-inactive))
         (vc-prop (lml--mode-line-string vc-string :face vc-face :pad 2))
         (filler-face (lml--active-or-inactive-face 'lml-filler-face 'lml-filler-face-inactive)))
    (lml--mode-line-rightmost vc-prop filler-face)))

;;;###autoload
(defun lowkey-mode-line-enable ()
  "Enable the lowkey-mode-line."
  (interactive)
  (when (not lml--default-mode-line-format)
    (setq lml--default-mode-line-format mode-line-format))

  (setq-default
   mode-line-format
   '("%e"
     (:eval (lml-buffer))
     (if (boundp 'spinner) spinner--mode-line-construct "")
     (:eval (lml-narrowed))
     (:eval (lml-remote))
     (:eval (lml-position))
     (:eval (lml-major-mode))
     (:eval (lml-minor-modes))
     (:eval (lml-vc))))

  (lml--record-selected-window)
  (lml--update-all)

  (add-hook 'post-command-hook 'lml--record-selected-window)
  (add-hook 'buffer-list-update-hook 'lml--update-all))

;;;###autoload
(defun lowkey-mode-line-disable ()
  "Reset the mode-line."
  (interactive)
  (remove-hook 'post-command-hook 'lml--record-selected-window)
  (remove-hook 'buffer-list-update-hook 'lml--update-all)
  (setq-default mode-line-format lml--default-mode-line-format))

(provide 'lowkey-mode-line)
