;;; all-the-icons-ivy-rich.el --- Better experience with icons for ivy        -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/all-the-icons-ivy-rich
;; Version: 1.6.4
;; Package-Requires: ((emacs "25.1") (ivy-rich "0.1.0") (all-the-icons "2.2.0"))
;; Keywords: convenience, icons, ivy

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
  :type 'number)

(defcustom all-the-icons-ivy-rich-project t
  "Whether support project root."
  :group 'all-the-icons-ivy-rich
  :type 'boolean)

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
      (ivy-rich-counsel-function-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-describe-variable
    (:columns
     ((all-the-icons-ivy-rich-variable-icon)
      (counsel-describe-variable-transformer (:width 0.3))
      (ivy-rich-counsel-variable-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-describe-symbol
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-counsel-symbol-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-set-variable
    (:columns
     ((all-the-icons-ivy-rich-variable-icon)
      (counsel-describe-variable-transformer (:width 0.3))
      (ivy-rich-counsel-variable-docstring (:face all-the-icons-ivy-rich-doc-face))))
    counsel-apropos
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-counsel-symbol-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-info-lookup-symbol
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate (:width 0.3))
      (all-the-icons-ivy-rich-counsel-symbol-docstring (:face all-the-icons-ivy-rich-doc-face)))
     :delimiter "\t")
    counsel-descbinds
    (:columns
     ((all-the-icons-ivy-rich-keybinding-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-file-jump
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-dired
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-dired-jump
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-fzf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-git
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.4))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-recentf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-file-name (:width 0.5))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (ivy-rich-file-last-modified-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-buffer-or-recentf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (counsel-buffer-or-recentf-transformer (:width 0.5))
      (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
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
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-load-library
    (:columns
     ((all-the-icons-ivy-rich-library-icon)
      (ivy-rich-candidate))
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
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    counsel-projectile-find-dir
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (counsel-projectile-find-dir-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
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
      (ivy-rich-candidate))
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
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-or-external-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-dired
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
      (all-the-icons-ivy-rich-project-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
      (all-the-icons-ivy-rich-project-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
     :delimiter "\t")
    project-vc-dir
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4))
      (all-the-icons-ivy-rich-project-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
      (all-the-icons-ivy-rich-project-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
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

    lsp-ivy-workspace-folders-remove
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4)))
     :delimiter "\t")

    treemacs-projectile
    (:columns
     ((all-the-icons-ivy-rich-dir-icon)
      (all-the-icons-ivy-rich-project-name (:width 0.4)))
     :delimiter "\t"))
  "Definitions for ivy-rich transformers.

See `ivy-rich-display-transformers-list' for details."
  :group 'all-the-icons-ivy-rich
  :type '(repeat sexp))



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

(defun all-the-icons-ivy-rich--file-path (candidate)
  "Get the file path of CANDIDATE."
  (if (eq (ivy-state-caller ivy-last) 'counsel-fzf)
      (expand-file-name candidate counsel--fzf-dir)
    (expand-file-name candidate ivy--directory)))

(defun all-the-icons-ivy-rich--project-file-path (candidate)
  "Get the project file path of CANDIDATE."
  (expand-file-name candidate (all-the-icons-ivy-rich--project-root)))

(defun all-the-icons-ivy-rich-project-find-file-transformer (candidate)
  "Transform non-visited file names with `ivy-virtual' face."
  (if (not (get-file-buffer (expand-file-name candidate (cdr (project-current)))))
      (propertize candidate 'face 'ivy-virtual)
    candidate))

(defun all-the-icons-ivy-rich--file-modes (file)
  "Return FILE modes."
  (cond
   ((file-remote-p file) "")
   ((not (file-exists-p file)) "")
   (t (file-attribute-modes (file-attributes file)))))

(defun all-the-icons-ivy-rich--file-id (path)
  "Return file uid/gid from CANDIDATE."
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
(defun all-the-icons-ivy-rich-file-name (candidate)
  "Return file name from CANDIDATE when reading files.
Display directories with different color.
Display the true name when the file is a symlink."
  (let* ((file (ivy-read-file-transformer candidate))
         (path (all-the-icons-ivy-rich--file-path candidate))
         (type (unless (file-remote-p path)
                 (file-symlink-p path))))
    (if (stringp type)
        (concat file
                (propertize (concat " -> " type)
                            'face 'all-the-icons-ivy-rich-doc-face))
      file)))

(defun all-the-icons-ivy-rich-file-modes (candidate)
  "Return file modes from CANDIDATE."
  (all-the-icons-ivy-rich--file-modes
   (all-the-icons-ivy-rich--file-path candidate)))

(defun all-the-icons-ivy-rich-file-id (candidate)
  "Return file uid/gid from CANDIDATE."
  (all-the-icons-ivy-rich--file-id
   (all-the-icons-ivy-rich--file-path candidate)))

(defun all-the-icons-ivy-rich-file-size (candidate)
  "Return file size from CANDIDATE."
  (all-the-icons-ivy-rich--file-size
   (all-the-icons-ivy-rich--file-path candidate)))

(defun all-the-icons-ivy-rich-file-modification-time (candidate)
  "Return file modification time from CANDIDATE."
  (all-the-icons-ivy-rich--file-modification-time
   (all-the-icons-ivy-rich--file-path candidate)))

;; Support `counsel-projectile-find-file', `counsel-projectile-dired', etc.
(defun all-the-icons-ivy-rich-project-name (candidate)
  "Return project name from CANDIDATE."
  (if (or (ivy--dirname-p candidate)
          (file-directory-p (all-the-icons-ivy-rich--file-path candidate)))
      (propertize candidate 'face 'ivy-subdir)
    candidate))

(defun all-the-icons-ivy-rich-project-file-modes (candidate)
  "Return file modes from CANDIDATE."
  (all-the-icons-ivy-rich--file-modes
   (all-the-icons-ivy-rich--project-file-path candidate)))

(defun all-the-icons-ivy-rich-project-file-id (candidate)
  "Return file uid/gid from CANDIDATE."
  (all-the-icons-ivy-rich--file-id
   (all-the-icons-ivy-rich--project-file-path candidate)))

(defun all-the-icons-ivy-rich-project-file-size (candidate)
  "Return file size from CANDIDATE."
  (all-the-icons-ivy-rich--file-size
   (all-the-icons-ivy-rich--project-file-path candidate)))

(defun all-the-icons-ivy-rich-project-file-modification-time (candidate)
  "Return file modification time from CANDIDATE."
  (all-the-icons-ivy-rich--file-modification-time
   (all-the-icons-ivy-rich--project-file-path candidate)))

;; Support `counsel-bookmark'
(defun all-the-icons-ivy-rich-bookmark-name (candidate)
  "Return bookmark name from CANDIDATE."
  (car (assoc candidate bookmark-alist)))

(defun all-the-icons-ivy-rich-bookmark-info (candidate)
  "Return bookmark name from CANDIDATE."
  (let ((file (ivy-rich-bookmark-filename candidate)))
    (cond
     ((null file) "")
     ((file-remote-p file) file)
     (t file))))

;; Support `counsel-package'
(defun all-the-icons-ivy-rich-package-install-summary (candidate)
  "Return package install summary from CANDIDATE. Used for `counsel-package'."
  (ivy-rich-package-install-summary (substring candidate 1)))

(defun all-the-icons-ivy-rich-package-archive-summary (candidate)
  "Return package archive summary from CANDIDATE. Used for `counsel-package'."
  (ivy-rich-package-archive-summary (substring candidate 1)))

(defun all-the-icons-ivy-rich-package-version (candidate)
  "Return package version from CANDIDATE. Used for `counsel-package'."
  (ivy-rich-package-version (substring candidate 1)))

;; Support `counsel-describe-face'
(defun all-the-icons-ivy-rich-counsel-face-docstring (candidate)
  "Return face's documentation from CANDIDATE."
  (let ((doc (face-doc-string (intern-soft candidate))))
    (if (and doc (string-match "^\\(.+\\)\\([\r\n]\\)?" doc))
        (setq doc (match-string 1 doc))
      "")))

;; Support `counsel-describe-symbol', `counsel-info-lookup-symbol' and `counsel-apropos'
(defun all-the-icons-ivy-rich-counsel-symbol-docstring (candidate)
  "Return symbol's documentation from CANDIDATE."
  (let ((symbol (intern-soft candidate)))
    (cond
     ((fboundp symbol)
      (ivy-rich-counsel-function-docstring candidate))
     ((facep symbol)
      (all-the-icons-ivy-rich-counsel-face-docstring candidate))
     ((and (boundp symbol) (not (keywordp symbol)))
      (ivy-rich-counsel-variable-docstring candidate))
     (t ""))))

;; Support `counsel-list-processes'
(defun all-the-icons-ivy-rich-process-id (candidate)
  "Return process id from CANDIDATE.

For a network, serial, and pipe connections, return \"--\"."
  (let ((p (get-process candidate)))
    (when (processp p)
      (format "%s" (or (process-id p) "--")))))

(defun all-the-icons-ivy-rich-process-status (candidate)
  "Return process status from CANDIDATE."
  (let ((p (get-process candidate)))
    (when (processp p)
      (let* ((status (process-status p))
             (face (if (memq status '(stop exit closed failed))
                       'all-the-icons-ivy-rich-process-status-alt-face
                     'all-the-icons-ivy-rich-process-status-face)))
        (propertize (symbol-name status) 'face face)))))

(defun all-the-icons-ivy-rich-process-buffer-name (candidate)
  "Return process buffer name from CANDIDATE.

If the buffer is killed, return \"--\"."
  (let ((p (get-process candidate)))
    (when (processp p)
      (let ((buf (process-buffer p)))
        (if (buffer-live-p buf)
		    (buffer-name buf)
		  "--")))))

(defun all-the-icons-ivy-rich-process-tty-name (candidate)
  "Return the name of the terminal process uses from CANDIDATE."
  (let ((p (get-process candidate)))
    (when (processp p)
      (or (process-tty-name p) "--"))))

(defun all-the-icons-ivy-rich-process-thread (candidate)
  "Return process thread from CANDIDATE."
  (let ((p (get-process candidate)))
    (when (processp p)
      (cond
       ((or
         (null (process-thread p))
         (not (fboundp 'thread-name))) "--")
       ((eq (process-thread p) main-thread) "Main")
	   ((thread-name (process-thread p)))
	   (t "--")))))

(defun all-the-icons-ivy-rich-process-command (candidate)
  "Return process command from CANDIDATE."
  (let ((p (get-process candidate)))
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
  (when (and (display-graphic-p)
             all-the-icons-ivy-rich-icon
             icon)
    (format " %s"
            (let* ((props (get-text-property 0 'face icon))
                   (family (plist-get props :family))
                   (face (if all-the-icons-ivy-rich-color-icon
                             (or (plist-get props :inherit) props)
                           'all-the-icons-ivy-rich-icon-face))
                   (new-face `(:inherit ,face
                               :family ,family
                               :height ,all-the-icons-ivy-rich-icon-size)))
              (propertize icon 'face new-face)))))

(defun all-the-icons-ivy-rich-buffer-icon (candidate)
  "Display buffer icon from CANDIDATE in `ivy-rich'."
  (let ((icon (with-current-buffer (get-buffer candidate)
                (if (eq major-mode 'dired-mode)
                    (all-the-icons-icon-for-dir candidate :face 'all-the-icons-ivy-rich-dir-face)
                  (all-the-icons-icon-for-buffer)))))
    (all-the-icons-ivy-rich--format-icon
     (if (or (null icon) (symbolp icon))
         (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
       (propertize icon 'display '(raise 0.0))))))

(defun all-the-icons-ivy-rich-file-icon (candidate)
  "Display file icon from CANDIDATE in `ivy-rich'."
  (let ((icon (cond
               ((ivy--dirname-p candidate)
                (all-the-icons-icon-for-dir candidate
                                            :height 0.9
                                            :v-adjust 0.01
                                            :face 'all-the-icons-ivy-rich-dir-face))
               ((not (string-empty-p candidate))
                (all-the-icons-icon-for-file (file-name-nondirectory candidate) :height 0.9 :v-adjust 0.0)))))
    (all-the-icons-ivy-rich--format-icon
     (if (or (null icon) (symbolp icon))
         (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
       (propertize icon 'display '(raise 0.0))))))

(defun all-the-icons-ivy-rich-dir-icon (_candidate)
  "Display project icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01 :face 'all-the-icons-silver)))

(defun all-the-icons-ivy-rich-mode-icon (_candidate)
  "Display mode icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)))

(defun all-the-icons-ivy-rich-function-icon (candidate)
  "Display function icon in `ivy-rich'."
  (if (commandp (intern candidate))
      (all-the-icons-ivy-rich--format-icon
       (all-the-icons-faicon "cog" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue))
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))))

(defun all-the-icons-ivy-rich-variable-icon (candidate)
  "Display the variable icon in `ivy-rich'."
  (if (custom-variable-p (intern candidate))
      (all-the-icons-ivy-rich--format-icon
       (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))
    (all-the-icons-ivy-rich--format-icon
     (all-the-icons-octicon "tag" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-lblue))))

(defun all-the-icons-ivy-rich-face-icon (_candidate)
  "Display face icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-blue)))

(defun all-the-icons-ivy-rich-symbol-icon (candidate)
  "Display the symbol icon in `ivy-rich'."
  (let ((sym (intern candidate)))
    (cond
     ((functionp sym)
      (all-the-icons-ivy-rich-function-icon candidate))
     ((facep sym)
      (all-the-icons-ivy-rich-face-icon candidate))
     ((symbolp sym)
      (all-the-icons-ivy-rich-variable-icon candidate))
     (t (all-the-icons-ivy-rich--format-icon
         (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))))))

(defun all-the-icons-ivy-rich-company-icon (candidate)
  "Display the symbol icon of company in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (if (fboundp 'company-box--get-icon)
       (company-box--get-icon candidate)
     (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))))

(defun all-the-icons-ivy-rich-theme-icon (_candidate)
  "Display the theme icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lcyan)))

(defun all-the-icons-ivy-rich-keybinding-icon (_candidate)
  "Display the keybindings icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "keyboard-o" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lsilver)))

(defun all-the-icons-ivy-rich-library-icon (_candidate)
  "Display the library icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue)))

(defun all-the-icons-ivy-rich-package-icon (_candidate)
  "Display the package icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))

(defun all-the-icons-ivy-rich-font-icon (_candidate)
  "Display the font icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))

(defun all-the-icons-ivy-rich-world-clock-icon (_candidate)
  "Display the world clock icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

(defun all-the-icons-ivy-rich-tramp-icon (_candidate)
  "Display the tramp icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.01)))

(defun all-the-icons-ivy-rich-git-branch-icon (_candidate)
  "Display the git branch icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)))

(defun all-the-icons-ivy-rich-process-icon (_candidate)
  "Display the process icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "zap" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)))

(defun all-the-icons-ivy-rich-imenu-icon (candidate)
  "Display the imenu icon from CANDIDATE in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (let ((case-fold-search nil))
     (cond
      ((string-match-p "Type Parameters?[:)]" candidate)
       (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
      ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" candidate)
       (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
      ((string-match-p "Constants?[:)]" candidate)
       (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
      ((string-match-p "Enum\\(erations?\\)?[:)]" candidate)
       (all-the-icons-material "storage" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-orange))
      ((string-match-p "References?[:)]" candidate)
       (all-the-icons-material "collections_bookmark" :height 0.95 :v-adjust -0.2))
      ((string-match-p "\\(Types?\\)\\|\\(Property\\)[:)]" candidate)
       (all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
      ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)\\|\\(Constructors?\\)[:)]" candidate)
       (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
      ((string-match-p "\\(Class\\)\\|\\(Structs?\\)[:)]" candidate)
       (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
      ((string-match-p "Interfaces?[:)]" candidate)
       (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
      ((string-match-p "Modules?[:)]" candidate)
       (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
      ((string-match-p "Packages?[:)]" candidate)
       (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))
      (t (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue))))))

(defun all-the-icons-ivy-rich-bookmark-icon (candidate)
  "Return bookmark type from CANDIDATE."
  (all-the-icons-ivy-rich--format-icon
   (let ((file (ivy-rich-bookmark-filename candidate)))
     (cond
      ((null file)
       (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
      ((file-remote-p file)
       (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.01))
      ((not (file-exists-p file))
       (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
      ((file-directory-p file)
       (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.01))
      (t (all-the-icons-icon-for-file (file-name-nondirectory file) :height 0.9 :v-adjust 0.0))))))

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
