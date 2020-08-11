;;; all-the-icons-ivy-rich.el --- Better experience with icons for ivy        -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/all-the-icons-ivy-rich
;; Version: 1.4.1
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

(defgroup all-the-icons-ivy-rich nil
  "Better experience using icons in ivy."
  :group 'all-the-icons
  :group 'ivy-rich
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/all-the-icons-ivy-rich"))

(defface all-the-icons-ivy-rich-icon-face
  '((t (:inherit default)))
  "Face used for the icons while `all-the-icons-ivy-rich-color-icon' is nil."
  :group 'all-the-icons-ivy-rich)

(defcustom all-the-icons-ivy-rich-color-icon t
  "Whether display the colorful icons.

It respects `all-the-icons-color-icons'."
  :group 'all-the-icons-ivy-rich
  :type 'boolean)

(defcustom all-the-icons-ivy-rich-icon-size 1.0
  "The default icon size in ivy."
  :group 'all-the-icons-ivy-rich
  :type 'number)

(defcustom all-the-icons-ivy-rich-display-transformers-list
  '(ivy-switch-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 15 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    ivy-switch-buffer-other-window
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 15 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")

    ;; counsel
    counsel-switch-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 15 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    counsel-switch-buffer-other-window
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 15 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    counsel-M-x
    (:columns
     ((all-the-icons-ivy-rich-function-icon)
      (counsel-M-x-transformer (:width 40))
      (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
    counsel-describe-function
    (:columns
     ((all-the-icons-ivy-rich-function-icon)
      (counsel-describe-function-transformer (:width 40))
      (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
    counsel-describe-variable
    (:columns
     ((all-the-icons-ivy-rich-variable-icon)
      (counsel-describe-variable-transformer (:width 40))
      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
    counsel-describe-symbol
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-set-variable
    (:columns
     ((all-the-icons-ivy-rich-variable-icon)
      (counsel-describe-variable-transformer (:width 40))
      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
    counsel-apropos
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-info-lookup-symbol
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-descbinds
    (:columns
     ((all-the-icons-ivy-rich-keybinding-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-read-file-transformer))
     :delimiter "\t")
    counsel-file-jump
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-dired
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-read-file-transformer))
     :delimiter "\t")
    counsel-dired-jump
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-el
    (:columns
     ((all-the-icons-ivy-rich-symbol-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-fzf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-git
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-recentf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate (:width 0.8))
      (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
     :delimiter "\t")
    counsel-buffer-or-recentf
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (counsel-buffer-or-recentf-transformer (:width 0.8))
      (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
     :delimiter "\t")
    counsel-bookmark
    (:columns
     ((all-the-icons-ivy-rich-bookmark-type)
      (all-the-icons-ivy-rich-bookmark-name (:width 40))
      (all-the-icons-ivy-rich-bookmark-info))
     :delimiter "\t")
    counsel-bookmarked-directory
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-package
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate (:width 30))
      (all-the-icons-ivy-rich-package-version (:width 16 :face font-lock-comment-face))
      (all-the-icons-ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
      (all-the-icons-ivy-rich-package-install-summary (:face font-lock-doc-face)))
     :delimiter "\t")
    counsel-fonts
    (:columns
     ((all-the-icons-ivy-rich-font-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-major
    (:columns
     ((all-the-icons-ivy-rich-function-icon)
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
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-projectile-switch-project
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-projectile-find-file
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (counsel-projectile-find-file-transformer))
     :delimiter "\t")
    counsel-projectile-find-dir
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (counsel-projectile-find-dir-transformer))
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
    counsel-cd
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    counsel-company
    (:columns
     ((all-the-icons-ivy-rich-company-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    ;; pkacage
    package-install
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
      (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
      (ivy-rich-package-install-summary (:face font-lock-doc-face)))
     :delimiter "\t")
    package-reinstall
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
      (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
      (ivy-rich-package-install-summary (:face font-lock-doc-face)))
     :delimiter "\t")
    package-delete
    (:columns
     ((all-the-icons-ivy-rich-package-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    persp-switch-to-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 15 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t")
    persp-switch
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-frame-switch
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-window-switch
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-kill
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-save-and-kill
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-import-buffers
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-import-win-conf
    (:columns
     ((all-the-icons-ivy-rich-project-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-kill-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-remove-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate))
     :delimiter "\t")
    persp-add-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    all-the-icons-ivy-rich-kill-buffer
    (:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate))
     :delimiter "\t")

    treemacs-projectile
    (:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-candidate))
     :delimiter "\t"))
  "Definitions for ivy-rich transformers.

See `ivy-rich-display-transformers-list' for details."
  :group 'all-the-icons-ivy-rich
  :type '(repeat sexp))



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

(defun all-the-icons-ivy-rich-bookmark-name (candidate)
  "Return bookmark name from CANDIDATE."
  (car (assoc candidate bookmark-alist)))

(defun all-the-icons-ivy-rich-bookmark-info (candidate)
  "Return bookmark name from CANDIDATE."
  (let ((filename (ivy-rich-bookmark-filename candidate)))
    (cond (filename
           (cond ((null filename)
                  "")
                 ((file-remote-p filename)
                  filename)
                 ((file-exists-p filename)
                  (file-truename filename))
                 (t filename))))))

(defun all-the-icons-ivy-rich-package-install-summary (candidate)
  "Return package install summary from CANDIDATE. Used for `counsel-package'."
  (ivy-rich-package-install-summary (substring candidate 1)))

(defun all-the-icons-ivy-rich-package-archive-summary (candidate)
  "Return package archive summary from CANDIDATE. Used for `counsel-package'."
  (ivy-rich-package-archive-summary (substring candidate 1)))

(defun all-the-icons-ivy-rich-package-version (candidate)
  "Return package version from CANDIDATE. Used for `counsel-package'."
  (ivy-rich-package-version (substring candidate 1)))

(defun all-the-icons-ivy-rich-align-icons ()
  "Set tab size to 1, to insert tabs as delimiters."
  (setq-local tab-width 1))

(defun all-the-icons-ivy-rich--format-icon (icon)
  "Format ICON'."
  (when icon
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
  (let* ((buffer (get-buffer candidate))
         (buffer-file-name (buffer-file-name buffer))
         (major-mode (buffer-local-value 'major-mode buffer))
         (icon (with-current-buffer buffer (all-the-icons-icon-for-buffer))))
    (all-the-icons-ivy-rich--format-icon
     (if (or (null icon) (symbolp icon))
         (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
       (propertize icon 'display '(raise 0.0))))))

(defun all-the-icons-ivy-rich-file-icon (candidate)
  "Display file icon from CANDIDATE in `ivy-rich'."
  (let* ((path (concat ivy--directory candidate))
         (file (file-name-nondirectory path))
         (icon (cond
                ((file-remote-p path)
                 (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.0))
                ((file-directory-p path)
                 (all-the-icons-icon-for-dir path :v-adjust 0.0))
                ((not (string-empty-p file))
                 (all-the-icons-icon-for-file file :v-adjust 0.0)))))
    (all-the-icons-ivy-rich--format-icon
     (if (or (null icon) (symbolp icon))
         (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
       (propertize icon 'display '(raise 0.0))))))

(defun all-the-icons-ivy-rich-project-icon (_candidate)
  "Display project icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01 :face 'all-the-icons-silver)))

(defun all-the-icons-ivy-rich-mode-icon (_candidate)
  "Display mode icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)))

(defun all-the-icons-ivy-rich-function-icon (_candidate)
  "Display function icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))

(defun all-the-icons-ivy-rich-variable-icon (_candidate)
  "Display the variable icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)))

(defun all-the-icons-ivy-rich-symbol-icon (candidate)
  "Display the symbol icon in `ivy-rich'."
  (let ((sym (intern candidate)))
    (cond ((functionp sym)
           (all-the-icons-ivy-rich-function-icon candidate))
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
   (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01)))

(defun all-the-icons-ivy-rich-git-branch-icon (_candidate)
  "Display the git branch icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)))

(defun all-the-icons-ivy-rich-process-icon (_candidate)
  "Display the process icon in `ivy-rich'."
  (all-the-icons-ivy-rich--format-icon
   (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)))

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
      (t (all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.125))))))

(defun all-the-icons-ivy-rich-bookmark-type (candidate)
  "Return bookmark type from CANDIDATE."
  (all-the-icons-ivy-rich--format-icon
   (let ((filename (ivy-rich-bookmark-filename candidate)))
     (cond ((null filename)
            (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
           ((file-remote-p filename)
            (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
           ((not (file-exists-p filename))
            (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
           ((file-directory-p filename)
            (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
           (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :v-adjust 0.0))))))

(defvar all-the-icons-ivy-rich-display-transformers-old-list ivy-rich-display-transformers-list)

;;;###autoload
(define-minor-mode all-the-icons-ivy-rich-mode
  "Better experience with icons for ivy."
  :lighter nil
  :global t
  (if all-the-icons-ivy-rich-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'all-the-icons-ivy-rich-align-icons)
        (global-set-key [remap kill-buffer] #'all-the-icons-ivy-rich-kill-buffer)
        (setq ivy-rich-display-transformers-list all-the-icons-ivy-rich-display-transformers-list))
    (progn
      (remove-hook 'minibuffer-setup-hook #'all-the-icons-ivy-rich-align-icons)
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
