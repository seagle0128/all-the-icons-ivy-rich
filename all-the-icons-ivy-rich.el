;;; all-the-icons-ivy-rich.el --- Better experience with icons for ivy        -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/all-the-icons-ivy-rich
;; Version: 1.7.0
;; Package-Requires: ((emacs "25.1") (ivy-rich "0.1.0") (all-the-icons "2.2.0"))
;; Keywords: convenience, icons, ivy

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:

;; Better experience with icons for ivy.
;;
;; Install:
;; From melpa, `M-x package-install RET all-the-icons-ivy-rich RET`.
;; (all-the-icons-ivy-rich-mode 1)
;; or
;; (use-package all-the-icons-ivy-rich-mode
;;   :ensure t
;;   :init (all-the-icons-ivy-rich-mode 1))


;;; Code:

(require 'subr-x)
(require 'ivy-rich)
(require 'all-the-icons)

(eval-when-compile
  (require 'package)
  (require 'bookmark)
  (require 'project))



;; Depress warnings
(defvar counsel--fzf-dir)
(defvar ivy--directory)
(defvar ivy-last)
(defvar ivy-posframe-buffer)
(declare-function find-library-name 'find-func)
(declare-function ivy-posframe--display 'ivy-posframe)

;; Compatibility
(unless (fboundp #'file-attribute-user-id)
  (defsubst file-attribute-user-id (attributes)
    (nth 2 attributes)))

(unless (fboundp #'file-attribute-group-id)
  (defsubst file-attribute-group-id (attributes)
    (nth 3 attributes)))

(unless (fboundp #'file-attribute-modification-time)
  (defsubst file-attribute-modification-time (attributes)
    (nth 5 attributes)))

(unless (fboundp #'file-attribute-size)
  (defsubst file-attribute-size (attributes)
    (nth 7 attributes)))

(unless (fboundp #'file-attribute-modes)
  (defsubst file-attribute-modes (attributes)
    (nth 8 attributes)))


;;
;; Faces
;;

(defgroup all-the-icons-ivy-rich nil
  "Better experience using icons in ivy."
  :group 'all-the-icons
  :group 'ivy-rich
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/all-the-icons-ivy-rich"))

(defface all-the-icons-ivy-rich-icon-face
  '((t (:inherit default)))
  "Face used for the icons while `all-the-icons-ivy-rich-color-icon' is nil."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-dir-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for the directory icon."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-doc-face
  '((t (:inherit ivy-completions-annotations)))
  "Face used for documentation string."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-size-face
  '((t (:inherit shadow)))
  "Face used for buffer size."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-time-face
  '((t (:inherit shadow)))
  "Face used for time."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-bookmark-face
  '((t (:inherit all-the-icons-ivy-rich-doc-face)))
  "Face used for time."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-version-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for package version."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-archive-face
  '((t (:inherit font-lock-type-face)))
  "Face used for package archive."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-install-face
  '((t (:inherit font-lock-string-face)))
  "Face used for package install."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-path-face
  '((t (:inherit all-the-icons-ivy-rich-doc-face)))
  "Face used for file path."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-indicator-face
  '((t (:inherit error)))
  "Face used for file indicators."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-major-mode-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for buffer major mode."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-project-face
  '((t (:inherit font-lock-string-face)))
  "Face used for project."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-file-name-face
  '((t :inherit all-the-icons-ivy-rich-doc-face))
  "Face used for highlight file names.")

(defface all-the-icons-ivy-rich-file-modes-face
  '((t :inherit font-lock-string-face))
  "Face used for highlight file modes.")

(defface all-the-icons-ivy-rich-file-owner-face
  '((t :inherit font-lock-keyword-face))
  "Face used for highlight file owners.")

(defface all-the-icons-ivy-rich-process-id-face
  '((t :inherit default))
  "Face used for process id.")

(defface all-the-icons-ivy-rich-process-status-face
  '((t :inherit success))
  "Face used for process status.")

(defface all-the-icons-ivy-rich-process-status-alt-face
  '((t :inherit error))
  "Face used for process status: stop, exit, closed and failed.")

(defface all-the-icons-ivy-rich-process-buffer-face
  '((t :inherit font-lock-keyword-face))
  "Face used for process buffer label.")

(defface all-the-icons-ivy-rich-process-tty-face
  '((t :inherit font-lock-doc-face))
  "Face used for process tty.")

(defface all-the-icons-ivy-rich-process-thread-face
  '((t :inherit font-lock-doc-face))
  "Face used for process thread.")

(defface all-the-icons-ivy-rich-process-command-face
  '((t :inherit all-the-icons-ivy-rich-doc-face))
  "Face used for process command.")

(defface all-the-icons-ivy-rich-type-face
  '((t :inherit font-lock-keyword-face))
  "Face used for type.")

(defface all-the-icons-ivy-rich-value-face
  '((t :inherit font-lock-keyword-face))
  "Face used for variable value.")

(defface all-the-icons-ivy-rich-true-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight true variable values.")

(defface all-the-icons-ivy-rich-null-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight null or unbound variable values.")

(defface all-the-icons-ivy-rich-list-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight list expressions.")

(defface all-the-icons-ivy-rich-number-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight numeric values.")

(defface all-the-icons-ivy-rich-string-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight string values.")

(defface all-the-icons-ivy-rich-function-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight function symbols.")

(defface all-the-icons-ivy-rich-symbol-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight general symbols.")

(defface all-the-icons-ivy-rich-imenu-type-face
  '((t (:inherit all-the-icons-ivy-rich-type-face :height 0.9)))
  "Face used for imenu type."
  :group 'all-the-icons-ivy-rich)

(defface all-the-icons-ivy-rich-imenu-doc-face
  '((t (:inherit all-the-icons-ivy-rich-doc-face :height 0.9)))
  "Face used for imenu documentation."
  :group 'all-the-icons-ivy-rich)

;;
;; Customization
;;

(defcustom all-the-icons-ivy-rich-icon t
  "Whether display the icons."
  :group 'all-the-icons-ivy-rich
  :type 'boolean)

(defcustom all-the-icons-ivy-rich-color-icon t
  "Whether display the colorful icons.

It respects `all-the-icons-color-icons'."
  :group 'all-the-icons-ivy-rich
  :type 'boolean)

(defcustom all-the-icons-ivy-rich-icon-size 1.0
  "The default icon size in ivy."
  :group 'all-the-icons-ivy-rich
  :type 'float)

(defcustom all-the-icons-ivy-rich-project t
  "Whether support project root."
  :group 'all-the-icons-ivy-rich
  :type 'boolean)

(defcustom all-the-icons-ivy-rich-field-width 80
  "Maximum truncation width of annotation fields.

This value is adjusted depending on the `window-width'."
  :group 'all-the-icons-ivy-rich
  :type 'integer)

(defcustom all-the-icons-ivy-rich-display-transformers-list
  '(ivy-switch-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    ivy-switch-buffer-other-window
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")

    ;; counsel
    counsel-switch-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    counsel-switch-buffer-other-window
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    counsel-M-x
    (:columns
     ((all-the-icons-ivy-rich-function-icon)
      (counsel-M-x-transformer (:width 0.3))
      (ivy-rich-counsel-function-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-describe-function
    (:columns
     ((all-the-icons-ivy-rich-function-icon)
      (counsel-describe-function-transformer (:width 0.3))
      (all-the-icons-ivy-rich-symbol-class (:width 8 :face all-the-icons-ivy-rich-type-face))
      (all-the-icons-ivy-rich-function-args (:width 0.12 :face all-the-icons-ivy-rich-value-face))
      (ivy-rich-counsel-function-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-describe-variable
    (:columns
     ((all-the-icons-ivy-rich-variable-icon)
      (counsel-describe-variable-transformer (:width 0.3))
      (all-the-icons-ivy-rich-symbol-class (:width 8 :face all-the-icons-ivy-rich-type-face))
      (all-the-icons-ivy-rich-variable-value (:width 0.12))
      (ivy-rich-counsel-variable-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-describe-symbol
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-symbol-class (:width 8 :face all-the-icons-ivy-rich-type-face))
      (all-the-icons-ivy-rich-symbol-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-set-variable
    (:columns
     ((all-the-icons-ivy-rich-variable-icon)
      (counsel-describe-variable-transformer (:width 0.3))
      (all-the-icons-ivy-rich-symbol-class (:width 8 :face all-the-icons-ivy-rich-type-face))
      (all-the-icons-ivy-rich-variable-value (:width 0.12))
      (ivy-rich-counsel-variable-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-apropos
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-symbol-class (:width 8 :face all-the-icons-ivy-rich-type-face))
      (all-the-icons-ivy-rich-symbol-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-info-lookup-symbol
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-symbol-class (:width 8 :face all-the-icons-ivy-rich-type-face))
      (all-the-icons-ivy-rich-symbol-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-descbinds
    (:columns
     ((all-the-icons-ivy-rich-keybinding-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-keybinding-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-file-jump
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-dired
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-dired-jump
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-fzf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-git
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-recentf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.5))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-file-last-modified-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-buffer-or-recentf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (counsel-buffer-or-recentf-transformer (:width 0.5))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-file-last-modified-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-bookmark
    (:columns
     ((all-the-icons-ivy-rich-bookmark-icon)
      (all-the-icons-ivy-rich-bookmark-name (:width 0.3))
      (ivy-rich-bookmark-type)
      (all-the-icons-ivy-rich-bookmark-info (:face all-the-icons-ivy-rich-bookmark-face)))
     :delimiter "\t")
    counsel-bookmarked-directory
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-package
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-package-version (:width 16 :face all-the-icons-ivy-rich-version-face))
      (all-the-icons-ivy-rich-package-archive-summary (:width 7 :face all-the-icons-ivy-rich-archive-face))
      (all-the-icons-ivy-rich-package-install-summary (:face all-the-icons-ivy-rich-install-face)))
     :delimiter "\t")
    counsel-fonts
    (:columns
     ((all-the-icons-ivy-rich-font-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-major
    (:columns
     ((all-the-icons-ivy-rich-mode-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-find-library
    (:columns
     ((all-the-icons-ivy-rich-library-icon)
      (all-the-icons-ivy-rich-library-transformer (:width 0.3))
      (all-the-icons-ivy-rich-library-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :delimiter "\t")
    counsel-load-library
    (:columns
     ((all-the-icons-ivy-rich-library-icon)
      (all-the-icons-ivy-rich-library-transformer (:width 0.3))
      (all-the-icons-ivy-rich-library-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :delimiter "\t")
    counsel-load-theme
    (:columns
     ((all-the-icons-ivy-rich-theme-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-world-clock
    (:columns
     ((all-the-icons-ivy-rich-world-clock-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-tramp
    (:columns
     ((all-the-icons-ivy-rich-tramp-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-git-checkout
    (:columns
     ((all-the-icons-ivy-rich-git-branch-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-list-processes
    (:columns
     ((all-the-icons-ivy-rich-process-icon)
      (ivy-rich-candidate (:width 25))
      (all-the-icons-ivy-rich-process-id (:width 7 :face all-the-icons-ivy-rich-process-id-face))
      (all-the-icons-ivy-rich-process-status (:width 7))
      (all-the-icons-ivy-rich-process-buffer-name (:width 25 :face all-the-icons-ivy-rich-process-buffer-face))
      (all-the-icons-ivy-rich-process-tty-name (:width 12 :face all-the-icons-ivy-rich-process-tty-face))
      (all-the-icons-ivy-rich-process-thread (:width 12 :face all-the-icons-ivy-rich-process-thread-face))
      (all-the-icons-ivy-rich-process-command (:face all-the-icons-ivy-rich-process-command-face)))
     :delimiter "\t")
    counsel-projectile-switch-project
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4)))
     :delimiter "\t")
    counsel-projectile-switch-to-buffer
    (:columns
     ((counsel-projectile-switch-to-buffer-transformer))
     :delimiter "\t")
    counsel-projectile-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (counsel-projectile-find-file-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-projectile-find-dir
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (counsel-projectile-find-dir-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-minor
    (:columns
     ((all-the-icons-ivy-rich-mode-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-imenu
    (:columns
     ((all-the-icons-ivy-rich-imenu-icon)
      (ivy-rich-candidate)
      (all-the-icons-ivy-rich-imenu-class (:face all-the-icons-ivy-rich-imenu-type-face))
      (all-the-icons-ivy-rich-imenu-docstring (:face all-the-icons-ivy-rich-imenu-doc-face)))
     :delimiter "\t")
    counsel-company
    (:columns
     ((all-the-icons-ivy-rich-company-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    ;; project
    project-switch-project
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-or-external-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-dired
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-vc-dir
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")

    ;; package
    package-install
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate (:width 0.3))
      (ivy-rich-package-version (:width 16 :face all-the-icons-ivy-rich-version-face))
      (ivy-rich-package-archive-summary (:width 7 :face all-the-icons-ivy-rich-archive-face))
      (ivy-rich-package-install-summary (:face all-the-icons-ivy-rich-install-face)))
     :delimiter "\t")
    package-reinstall
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate (:width 0.3))
      (ivy-rich-package-version (:width 16 :face all-the-icons-ivy-rich-version-face))
      (ivy-rich-package-archive-summary (:width 7 :face all-the-icons-ivy-rich-archive-face))
      (ivy-rich-package-install-summary (:face all-the-icons-ivy-rich-install-face)))
     :delimiter "\t")
    package-delete
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    persp-switch-to-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    persp-switch
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-frame-switch
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-window-switch
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-kill
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-save-and-kill
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-import-buffers
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-import-win-conf
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-kill-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :delimiter "\t")
    persp-remove-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :delimiter "\t")
    persp-add-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    all-the-icons-ivy-rich-kill-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :delimiter "\t")

    org-switchb
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-switch-buffer-transformer (:width 0.3))
      (ivy-rich-switch-buffer-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-switch-buffer-indicators (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
      (ivy-rich-switch-buffer-major-mode (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face all-the-icons-ivy-rich-path-face)))
     :delimiter "\t")

    customize-group
    (:columns
     ((all-the-icons-ivy-rich-settings-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-custom-group-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")

    describe-character-set
    (:columns
     ((all-the-icons-ivy-rich-charset-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-charset-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")

    describe-coding-system
    (:columns
     ((all-the-icons-ivy-rich-coding-system-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-coding-system-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")

    describe-language-environment
    (:columns
     ((all-the-icons-ivy-rich-lang-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    set-input-method
    (:columns
     ((all-the-icons-ivy-rich-input-method-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-input-method-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")

    lsp-ivy-workspace-folders-remove
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (all-the-icons-ivy-rich-project-name))
     :delimiter "\t")

    treemacs-projectile
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (all-the-icons-ivy-rich-project-name))
     :delimiter "\t"))
  "Definitions for ivy-rich transformers.

See `ivy-rich-display-transformers-list' for details."
  :group 'all-the-icons-ivy-rich
  :type '(repeat sexp))


;;
;; Utilities
;;

;; Support `kill-buffer'
(defun all-the-icons-ivy-rich-kill-buffer (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME."
  (interactive
   (list (completing-read (format "Kill buffer (default %s): " (buffer-name))
                          (mapcar (lambda (b)
                                    (buffer-name b))
                                  (buffer-list))
                          nil t nil nil
                          (buffer-name))))
  (kill-buffer buffer-or-name))

(defun all-the-icons-ivy-rich--project-root ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (when all-the-icons-ivy-rich-project
    (cond
     ;; Ignore remote files due to performance issue
     ((file-remote-p default-directory)
      default-directory)
     ((fboundp 'ffip-get-project-root-directory)
      (let ((inhibit-message t))
        (ffip-get-project-root-directory)))
     ((fboundp 'projectile-project-root)
      (projectile-project-root))
     ((fboundp 'project-current)
      (when-let ((project (project-current)))
        (expand-file-name (cdr project))))
     (t default-directory))))

(defun all-the-icons-ivy-rich--file-path (cand)
  "Get the file path of CAND."
  (if (eq (ivy-state-caller ivy-last) 'counsel-fzf)
      (expand-file-name cand counsel--fzf-dir)
    (expand-file-name cand ivy--directory)))

(defun all-the-icons-ivy-rich--project-file-path (cand)
  "Get the project file path of CAND."
  (expand-file-name cand (all-the-icons-ivy-rich--project-root)))

(defun all-the-icons-ivy-rich-project-find-file-transformer (cand)
  "Transform non-visited file names with `ivy-virtual' face."
  (if (not (get-file-buffer (expand-file-name cand (cdr (project-current)))))
      (propertize cand 'face 'ivy-virtual)
    cand))

(defun all-the-icons-ivy-rich--file-modes (file)
  "Return FILE modes."
  (cond
   ((file-remote-p file) "")
   ((not (file-exists-p file)) "")
   (t (file-attribute-modes (file-attributes file)))))

(defun all-the-icons-ivy-rich--file-id (path)
  "Return file uid/gid for CAND."
  (cond
   ((file-remote-p path) "")
   ((not (file-exists-p path)) "")
   (t (let ((attributes (file-attributes path 'string)))
        (format "%s %s"
                (file-attribute-user-id attributes)
                (file-attribute-group-id attributes))))))

(defun all-the-icons-ivy-rich--file-size (file)
  "Return FILE size."
  (cond
   ((file-remote-p file) "")
   ((not (file-exists-p file)) "")
   (t (file-size-human-readable (file-attribute-size (file-attributes file))))))

(defun all-the-icons-ivy-rich--file-modification-time (file)
  "Return FILE modification time."
  (cond
   ((file-remote-p file) "")
   ((not (file-exists-p file)) "")
   (t (format-time-string
       "%b %d %H:%M"
       (file-attribute-modification-time (file-attributes file))))))

;; Support `counsel-find-file', `counsel-dired', etc.
(defun all-the-icons-ivy-rich-file-name (cand)
  "Return file name for CAND when reading files.
Display directories with different color.
Display the true name when the file is a symlink."
  (let* ((file (ivy-read-file-transformer cand))
         (path (all-the-icons-ivy-rich--file-path cand))
         (type (unless (file-remote-p path)
                 (file-symlink-p path))))
    (if (stringp type)
        (concat file
                (propertize (concat " -> " type)
                            'face 'all-the-icons-ivy-rich-doc-face))
      file)))

(defun all-the-icons-ivy-rich-file-modes (cand)
  "Return file modes for CAND."
  (all-the-icons-ivy-rich--file-modes
   (all-the-icons-ivy-rich--file-path cand)))

(defun all-the-icons-ivy-rich-file-id (cand)
  "Return file uid/gid for CAND."
  (all-the-icons-ivy-rich--file-id
   (all-the-icons-ivy-rich--file-path cand)))

(defun all-the-icons-ivy-rich-file-size (cand)
  "Return file size for CAND."
  (all-the-icons-ivy-rich--file-size
   (all-the-icons-ivy-rich--file-path cand)))

(defun all-the-icons-ivy-rich-file-modification-time (cand)
  "Return file modification time for CAND."
  (all-the-icons-ivy-rich--file-modification-time
   (all-the-icons-ivy-rich--file-path cand)))

;; Support `counsel-projectile-find-file', `counsel-projectile-dired', etc.
(defun all-the-icons-ivy-rich-project-name (cand)
  "Return project name for CAND."
  (if (or (ivy--dirname-p cand)
          (file-directory-p (all-the-icons-ivy-rich--file-path cand)))
      (propertize cand 'face 'ivy-subdir)
    cand))

(defun all-the-icons-ivy-rich-project-file-modes (cand)
  "Return file modes for CAND."
  (all-the-icons-ivy-rich--file-modes
   (all-the-icons-ivy-rich--project-file-path cand)))

(defun all-the-icons-ivy-rich-project-file-id (cand)
  "Return file uid/gid for CAND."
  (all-the-icons-ivy-rich--file-id
   (all-the-icons-ivy-rich--project-file-path cand)))

(defun all-the-icons-ivy-rich-project-file-size (cand)
  "Return file size for CAND."
  (all-the-icons-ivy-rich--file-size
   (all-the-icons-ivy-rich--project-file-path cand)))

(defun all-the-icons-ivy-rich-project-file-modification-time (cand)
  "Return file modification time for CAND."
  (all-the-icons-ivy-rich--file-modification-time
   (all-the-icons-ivy-rich--project-file-path cand)))

;; Support `counsel-bookmark'
(defun all-the-icons-ivy-rich-bookmark-name (cand)
  "Return bookmark name for CAND."
  (car (assoc cand bookmark-alist)))

(defun all-the-icons-ivy-rich-bookmark-info (cand)
  "Return bookmark name for CAND."
  (let ((file (ivy-rich-bookmark-filename cand)))
    (cond
     ((null file) "")
     ((file-remote-p file) file)
     (t file))))

;; Support `counsel-package'
(defun all-the-icons-ivy-rich-package-install-summary (cand)
  "Return package install summary for CAND. Used for `counsel-package'."
  (ivy-rich-package-install-summary (substring cand 1)))

(defun all-the-icons-ivy-rich-package-archive-summary (cand)
  "Return package archive summary for CAND. Used for `counsel-package'."
  (ivy-rich-package-archive-summary (substring cand 1)))

(defun all-the-icons-ivy-rich-package-version (cand)
  "Return package version for CAND. Used for `counsel-package'."
  (ivy-rich-package-version (substring cand 1)))

(defun all-the-icons-ivy-rich--truncate-docstring (doc)
  "Truncate DOC string."
  (if (and doc (string-match "^\\(.+\\)\\([\r\n]\\)?" doc))
      (match-string 1 doc)
    ""))

;; Support `counsel-describe-face'
(defun all-the-icons-ivy-rich-counsel-face-docstring (cand)
  "Return face's documentation for CAND."
  (all-the-icons-ivy-rich--truncate-docstring (face-doc-string (intern-soft cand))))

;; Support `counsel-describe-function'and `counsel-describe-variable'
(defun all-the-icons-ivy-rich-function-args (cand)
  "Return function arguments for CAND."
  (let ((sym (intern-soft cand))
        (tmp))
    (or
     (elisp-function-argstring
      (cond
       ((listp (setq tmp (gethash (indirect-function sym)
                                  advertised-signature-table t)))
        tmp)
       ((setq tmp (help-split-fundoc
		           (ignore-errors (documentation sym t))
		           sym))
        (substitute-command-keys (car tmp)))
       ((setq tmp (help-function-arglist sym))
        (if (and (stringp tmp)
                 (string-match-p "Arg list not available" tmp))
            "[autoload]"
          tmp))))
     "")))

(defun all-the-icons-ivy-rich-variable-value (cand)
  "Return the variable value of CAND as string."
  (let ((sym (intern-soft cand)))
    (cond
     ((not (boundp sym))
      (propertize "#<unbound>" 'face 'all-the-icons-ivy-rich-null-face))
     (t (let ((val (symbol-value sym)))
          (pcase val
            ('nil (propertize "nil" 'face 'all-the-icons-ivy-rich-null-face))
            ('t (propertize "t" 'face 'all-the-icons-ivy-rich-true-face))
            ((pred keymapp) (propertize "#<keymap>" 'face 'all-the-icons-ivy-rich-value-face))
            ((pred bool-vector-p) (propertize "#<bool-vector>" 'face 'all-the-icons-ivy-rich-value-face))
            ((pred hash-table-p) (propertize "#<hash-table>" 'face 'all-the-icons-ivy-rich-value-face))
            ((pred syntax-table-p) (propertize "#<syntax-table>" 'face 'all-the-icons-ivy-rich-value-face))
            ;; Emacs BUG: abbrev-table-p throws an error
            ((guard (ignore-errors (abbrev-table-p val))) (propertize "#<abbrev-table>" 'face 'all-the-icons-ivy-rich-value-face))
            ((pred char-table-p) (propertize "#<char-table>" 'face 'all-the-icons-ivy-rich-value-face))
            ((pred byte-code-function-p) (propertize "#<byte-code-function>" 'face 'all-the-icons-ivy-rich-function-face))
            ((and (pred functionp) (pred symbolp))
             ;; NOTE: We are not consistent here, values are generally printed unquoted. But we
             ;; make an exception for function symbols to visually distinguish them from symbols.
             ;; I am not entirely happy with this, but we should not add quotation to every type.
             (format (propertize "#'%s" 'face 'all-the-icons-ivy-rich-function-face) val))
            ((pred recordp) (format (propertize "#<record %s>" 'face 'all-the-icons-ivy-rich-value-face) (type-of val)))
            ((pred symbolp) (propertize (symbol-name val) 'face 'all-the-icons-ivy-rich-symbol-face))
            ((pred numberp) (propertize (number-to-string val) 'face 'all-the-icons-ivy-rich-number-face))
            (_ (let ((print-escape-newlines t)
                     (print-escape-control-characters t)
                     (print-escape-multibyte t)
                     (print-level 10)
                     (print-length all-the-icons-ivy-rich-field-width))
                 (propertize
                  (prin1-to-string
                   (if (stringp val)
                       ;; Get rid of string properties to save some of the precious space
                       (substring-no-properties
                        val 0
                        (min (length val) all-the-icons-ivy-rich-field-width))
                     val))
                  'face
                  (cond
                   ((listp val) 'all-the-icons-ivy-rich-list-face)
                   ((stringp val) 'all-the-icons-ivy-rich-string-face)
                   (t 'all-the-icons-ivy-rich-value-face)))))))))))

;; Support `counsel-describe-symbol', `counsel-info-lookup-symbol' and `counsel-apropos'

;; Taken from advice--make-docstring
(defun all-the-icons-ivy-rich--advised (fun)
  "Return t if function FUN is advised."
  (let ((flist (indirect-function fun)))
    (advice--p (if (eq 'macro (car-safe flist)) (cdr flist) flist))))

;; Symbol class characters from Emacs 28 `help--symbol-completion-table-affixation'
;; ! and * are additions. Same as marginalia
(defun all-the-icons-ivy-rich-symbol-class (cand)
  "Return symbol class characters for symbol S.

Function:
f function
c command
C interactive-only command
m macro
M special-form
p pure
s side-effect-free
@ autoloaded
! advised
- obsolete

Variable:
u custom (U modified compared to global value)
v variable
l local (L modified compared to default value)
- obsolete

Other:
a face
t cl-type"
  (let ((s (intern-soft cand)))
    (format
     "%-6s"
     (concat
      (when (fboundp s)
        (concat
         (cond
          ((get s 'pure) "p")
          ((get s 'side-effect-free) "s"))
         (cond
          ((commandp s) (if (get s 'interactive-only) "C" "c"))
          ((macrop (symbol-function s)) "m")
          ((special-form-p (symbol-function s)) "M")
          (t "f"))
         (and (autoloadp (symbol-function s)) "@")
         (and (all-the-icons-ivy-rich--advised s) "!")
         (and (get s 'byte-obsolete-info) "-")))
      (when (boundp s)
        (concat
         (when (local-variable-if-set-p s)
           (if (ignore-errors
                 (not (equal (symbol-value s)
                             (default-value s))))
               "L" "l"))
         (if (custom-variable-p s)
             (if (ignore-errors
                   (not (equal
                         (symbol-value s)
                         (eval (car (get s 'standard-value))))))
                 "U" "u")
           "v")
         (and (get s 'byte-obsolete-variable) "-")))
      (and (facep s) "a")
      (and (fboundp 'cl-find-class) (cl-find-class s) "t")))))

(defun all-the-icons-ivy-rich-symbol-docstring (cand)
  "Return symbol's documentation for CAND."
  (let ((symbol (intern-soft cand)))
    (cond
     ((fboundp symbol)
      (ivy-rich-counsel-function-docstring cand))
     ((facep symbol)
      (all-the-icons-ivy-rich-counsel-face-docstring cand))
     ((and (boundp symbol) (not (keywordp symbol)))
      (ivy-rich-counsel-variable-docstring cand))
     (t ""))))

;; Support `counsel-imenu'
(defun all-the-icons-ivy-rich--counsel-imenu-symbol (cand)
  "Return imenu symbol from CAND."
  (let ((str (split-string cand ": ")))
    (or (cadr str) (car str))))

(defun all-the-icons-ivy-rich-imenu-class (cand)
  "Return imenu's class characters for CAND.

Only available in `emacs-lisp-mode'."
  (if (derived-mode-p 'emacs-lisp-mode)
      (string-trim
       (all-the-icons-ivy-rich-symbol-class
        (all-the-icons-ivy-rich--counsel-imenu-symbol cand)))
    ""))

(defun all-the-icons-ivy-rich-imenu-docstring (cand)
  "Return imenu's documentation for CAND.

Only available in `emacs-lisp-mode'."
  (if (derived-mode-p 'emacs-lisp-mode)
      (all-the-icons-ivy-rich-symbol-docstring
       (all-the-icons-ivy-rich--counsel-imenu-symbol cand))
    ""))

;; Support `counsel-descbinds'
(defun all-the-icons-ivy-rich-keybinding-docstring (cand)
  "Return keybinding's documentation for CAND."
  (let ((width 15))
    (if (length> cand width)
        (all-the-icons-ivy-rich--truncate-docstring
         (describe-key-briefly (kbd (substring-no-properties cand 0 width))))
      "")))

;; Support `customize-group'
(defun all-the-icons-ivy-rich-custom-group-docstring (cand)
  "Return custom group's documentation for CAND."
  (all-the-icons-ivy-rich--truncate-docstring
   (or (documentation-property (intern cand) 'group-documentation) "")))

;; Support `describe-character-set'
(defun all-the-icons-ivy-rich-charset-docstring (cand)
  "Return charset's documentation for CAND."
  (all-the-icons-ivy-rich--truncate-docstring (charset-description (intern cand))))

;; Support `describe-coding-system'
(defun all-the-icons-ivy-rich-coding-system-docstring (cand)
  "Return coding system's documentation for CAND."
  (all-the-icons-ivy-rich--truncate-docstring (coding-system-doc-string (intern cand))))

;; Support `set-input-method'
(defun all-the-icons-ivy-rich-input-method-docstring (cand)
  "Return input method's documentation for CAND."
  (nth 4 (assoc cand input-method-alist)))

;; Support `counsel-list-processes'
(defun all-the-icons-ivy-rich-process-id (cand)
  "Return process id for CAND.

For a network, serial, and pipe connections, return \"--\"."
  (let ((p (get-process cand)))
    (when (processp p)
      (format "%s" (or (process-id p) "--")))))

(defun all-the-icons-ivy-rich-process-status (cand)
  "Return process status for CAND."
  (let ((p (get-process cand)))
    (when (processp p)
      (let* ((status (process-status p))
             (face (if (memq status '(stop exit closed failed))
                       'all-the-icons-ivy-rich-process-status-alt-face
                     'all-the-icons-ivy-rich-process-status-face)))
        (propertize (symbol-name status) 'face face)))))

(defun all-the-icons-ivy-rich-process-buffer-name (cand)
  "Return process buffer name for CAND.

If the buffer is killed, return \"--\"."
  (let ((p (get-process cand)))
    (when (processp p)
      (let ((buf (process-buffer p)))
        (if (buffer-live-p buf)
		    (buffer-name buf)
		  "--")))))

(defun all-the-icons-ivy-rich-process-tty-name (cand)
  "Return the name of the terminal process uses for CAND."
  (let ((p (get-process cand)))
    (when (processp p)
      (or (process-tty-name p) "--"))))

(defun all-the-icons-ivy-rich-process-thread (cand)
  "Return process thread for CAND."
  (let ((p (get-process cand)))
    (when (processp p)
      (cond
       ((or
         (null (process-thread p))
         (not (fboundp 'thread-name))) "--")
       ((eq (process-thread p) main-thread) "Main")
	   ((thread-name (process-thread p)))
	   (t "--")))))

(defun all-the-icons-ivy-rich-process-command (cand)
  "Return process command for CAND."
  (let ((p (get-process cand)))
    (when (processp p)
      (let ((type (process-type p)))
        (if (memq type '(network serial pipe))
		    (let ((contact (process-contact p t t)))
			  (if (eq type 'network)
			      (format "(%s %s)"
				          (if (plist-get contact :type)
					          "datagram"
				            "network")
				          (if (plist-get contact :server)
					          (format
                               "server on %s"
					           (if (plist-get contact :host)
                                   (format "%s:%s"
						                   (plist-get contact :host)
                                           (plist-get
                                            contact :service))
					             (plist-get contact :local)))
				            (format "connection to %s:%s"
					                (plist-get contact :host)
					                (plist-get contact :service))))
			    (format "(serial port %s%s)"
				        (or (plist-get contact :port) "?")
				        (let ((speed (plist-get contact :speed)))
				          (if speed
					          (format " at %s b/s" speed)
				            "")))))
		  (mapconcat 'identity (process-command p) " "))))))

;; Support `counsel-find-library' and `counsel-load-library'
(defun all-the-icons-ivy-rich-library-transformer (cand)
  "Return library name for CAND."
  (if (featurep (intern-soft cand))
      cand
    (propertize cand 'face 'ivy-virtual)))

(defun all-the-icons-ivy-rich-library-path (cand)
  "Return library path for CAND."
  (abbreviate-file-name (find-library-name cand)))

;;
;; Icons
;;

(defun all-the-icons-ivy-rich--align-icons ()
  "Set tab size to 1, to insert tabs as delimiters."
  (setq-local tab-width 1))

(defun all-the-icons-ivy-rich-minibuffer-align-icons ()
  "Align the icons in `minibuffer'."
  (all-the-icons-ivy-rich--align-icons))

(defun all-the-icons-ivy-rich-ivy-posframe-align-icons (&rest _)
  "Align the icons in `ivy-posframe'."
  (with-current-buffer ivy-posframe-buffer
    (all-the-icons-ivy-rich--align-icons)))

(defun all-the-icons-ivy-rich--format-icon (icon)
  "Format ICON'."
  (let* ((props (get-text-property 0 'face icon))
         (family (plist-get props :family))
         (face (if all-the-icons-ivy-rich-color-icon
                   (or (plist-get props :inherit) props)
                 'all-the-icons-ivy-rich-icon-face))
         (new-face `(:inherit ,face
                     :family ,family
                     :height ,all-the-icons-ivy-rich-icon-size)))
    (format " %s" (propertize icon 'face new-face))))

(defun all-the-icons-ivy-rich-buffer-icon (cand)
  "Display buffer icon for CAND in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (let ((icon (with-current-buffer (get-buffer cand)
                  (if (eq major-mode 'dired-mode)
                      (all-the-icons-icon-for-dir cand :face 'all-the-icons-ivy-rich-dir-face)
                    (all-the-icons-icon-for-buffer)))))
      (all-the-icons-ivy-rich--format-icon
       (if (or (null icon) (symbolp icon))
           (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
         (propertize icon 'display '(raise 0.0)))))))

(defun all-the-icons-ivy-rich-file-icon (cand)
  "Display file icon for CAND in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (let ((icon (cond
                 ((ivy--dirname-p cand)
                  (all-the-icons-icon-for-dir cand
                                              :height 0.9
                                              :v-adjust 0.01
                                              :face 'all-the-icons-ivy-rich-dir-face))
                 ((not (string-empty-p cand))
                  (all-the-icons-icon-for-file (file-name-nondirectory cand) :height 0.9 :v-adjust 0.0)))))
      (all-the-icons-ivy-rich--format-icon
       (if (or (null icon) (symbolp icon))
           (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
         (propertize icon 'display '(raise 0.0)))))))

(defun all-the-icons-ivy-rich-dir-icon (_cand)
  "Display project icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01 :face 'all-the-icons-silver))))

(defun all-the-icons-ivy-rich-mode-icon (_cand)
  "Display mode icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue))))

(defun all-the-icons-ivy-rich-function-icon (cand)
  "Display function icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (if (commandp (intern cand))
         (all-the-icons-faicon "cog" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)
       (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))))

(defun all-the-icons-ivy-rich-variable-icon (cand)
  "Display the variable icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (if (custom-variable-p (intern cand))
         (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)
       (all-the-icons-octicon "tag" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-lblue)))))

(defun all-the-icons-ivy-rich-face-icon (_cand)
  "Display face icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-blue))))

(defun all-the-icons-ivy-rich-symbol-icon (cand)
  "Display the symbol icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (let ((sym (intern cand)))
      (cond
       ((functionp sym)
        (all-the-icons-ivy-rich-function-icon cand))
       ((facep sym)
        (all-the-icons-ivy-rich-face-icon cand))
       ((symbolp sym)
        (all-the-icons-ivy-rich-variable-icon cand))
       (t (all-the-icons-ivy-rich--format-icon
           (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))))))

(defun all-the-icons-ivy-rich-company-icon (cand)
  "Display the symbol icon of company in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (if (fboundp 'company-box--get-icon)
         (company-box--get-icon cand)
       (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))))

(defun all-the-icons-ivy-rich-theme-icon (_cand)
  "Display the theme icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lcyan))))

(defun all-the-icons-ivy-rich-keybinding-icon (_cand)
  "Display the keybindings icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "keyboard-o" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lsilver))))

(defun all-the-icons-ivy-rich-library-icon (_cand)
  "Display the library icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-package-icon (_cand)
  "Display the package icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))))

(defun all-the-icons-ivy-rich-font-icon (_cand)
  "Display the font icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-world-clock-icon (_cand)
  "Display the world clock icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-tramp-icon (_cand)
  "Display the tramp icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.01))))

(defun all-the-icons-ivy-rich-git-branch-icon (_cand)
  "Display the git branch icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green))))

(defun all-the-icons-ivy-rich-process-icon (_cand)
  "Display the process icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "zap" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-imenu-icon (cand)
  "Display the imenu icon for CAND in `ivy-rich'."
  (if (derived-mode-p 'emacs-lisp-mode)
      (all-the-icons-ivy-rich-symbol-icon (all-the-icons-ivy-rich--counsel-imenu-symbol cand))
    (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
      (all-the-icons-ivy-rich--format-icon
       (let ((case-fold-search nil))
         (cond
          ((string-match-p "Type Parameters?[:)]" cand)
           (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
          ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" cand)
           (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
          ((string-match-p "Constants?[:)]" cand)
           (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
          ((string-match-p "Enum\\(erations?\\)?[:)]" cand)
           (all-the-icons-material "storage" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-orange))
          ((string-match-p "References?[:)]" cand)
           (all-the-icons-material "collections_bookmark" :height 0.95 :v-adjust -0.2))
          ((string-match-p "\\(Types?\\)\\|\\(Property\\)[:)]" cand)
           (all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
          ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)\\|\\(Constructors?\\)[:)]" cand)
           (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
          ((string-match-p "\\(Class\\)\\|\\(Structs?\\)[:)]" cand)
           (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
          ((string-match-p "Interfaces?[:)]" cand)
           (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
          ((string-match-p "Modules?[:)]" cand)
           (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
          ((string-match-p "Packages?[:)]" cand)
           (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))
          (t (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-blue))))))))

(defun all-the-icons-ivy-rich-bookmark-icon (cand)
  "Return bookmark type for CAND."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (let ((file (ivy-rich-bookmark-filename cand)))
       (cond
        ((null file)
         (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
        ((file-remote-p file)
         (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.01))
        ((not (file-exists-p file))
         (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
        ((file-directory-p file)
         (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.01))
        (t (all-the-icons-icon-for-file (file-name-nondirectory file) :height 0.9 :v-adjust 0.0)))))))

(defun all-the-icons-ivy-rich-settings-icon (_cand)
  "Display settings icon for CAND in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "settings" :height 0.9 :v-adjust -0.01 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-charset-icon (_cand)
  "Display charset icon for CAND in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "table" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-coding-system-icon (_cand)
  "Display coding system icon for CAND in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "table" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple))))

(defun all-the-icons-ivy-rich-lang-icon (_cand)
  "Display language icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "language" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-input-method-icon (_cand)
  "Display input method icon in `ivy-rich'."
  (when (and (display-graphic-p) all-the-icons-ivy-rich-icon)
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "keyboard-o" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))



;;
;; Modes
;;

(defvar all-the-icons-ivy-rich-display-transformers-old-list ivy-rich-display-transformers-list)

;;;###autoload
(define-minor-mode all-the-icons-ivy-rich-mode
  "Better experience with icons for ivy."
  :lighter nil
  :global t
  (if all-the-icons-ivy-rich-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'all-the-icons-ivy-rich-minibuffer-align-icons)
        (advice-add #'ivy-posframe--display :after #'all-the-icons-ivy-rich-ivy-posframe-align-icons)
        (global-set-key [remap kill-buffer] #'all-the-icons-ivy-rich-kill-buffer)
        (setq ivy-rich-display-transformers-list all-the-icons-ivy-rich-display-transformers-list))
    (progn
      (remove-hook 'minibuffer-setup-hook #'all-the-icons-ivy-rich-minibuffer-align-icons)
      (advice-remove #'ivy-posframe--display #'all-the-icons-ivy-rich-ivy-posframe-align-icons)
      (global-unset-key [remap kill-buffer])
      (setq ivy-rich-display-transformers-list all-the-icons-ivy-rich-display-transformers-old-list)))
  (ivy-rich-reload))

;;;###autoload
(defun all-the-icons-ivy-rich-reload ()
  "Reload `all-the-icons-ivy-rich'."
  (interactive)
  (when all-the-icons-ivy-rich-mode
    (all-the-icons-ivy-rich-mode -1)
    (all-the-icons-ivy-rich-mode 1)
    (message "Reload all-the-icons-ivy-rich")))

(provide 'all-the-icons-ivy-rich)

;;; all-the-icons-ivy-rich.el ends here
