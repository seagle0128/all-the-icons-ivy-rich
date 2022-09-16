# all-the-icons-ivy-rich

[![Build Status](https://github.com/seagle0128/all-the-icons-ivy-rich/workflows/CI/badge.svg?branch=master)](https://github.com/seagle0128/all-the-icons-ivy-rich/actions)
[![Release Tag](https://img.shields.io/github/tag/seagle0128/all-the-icons-ivy-rich.svg?label=Release)](https://github.com/seagle0128/all-the-icons-ivy-rich/releases)
[![License](http://img.shields.io/:License-GPL3-blue.svg)](License)
[![MELPA](https://melpa.org/packages/all-the-icons-ivy-rich-badge.svg)](https://melpa.org/#/all-the-icons-ivy-rich)
[![MELPA Stable](https://stable.melpa.org/packages/all-the-icons-ivy-rich-badge.svg)](https://stable.melpa.org/#/all-the-icons-ivy-rich)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

## Table of Contents

- [Install](#install)
  - [Manual](#manual)
  - [Use-package](#use-package)
- [Customize](#customize)
- [Screenshots](#screenshots)
- [Donate](#donate)

<!-- markdown-toc end -->

Display icons for all buffers in [ivy](https://github.com/abo-abo/swiper).

This package is extracted from [Centaur
Emacs](https://github.com/seagle0128/.emacs.d) and leverages
[ivy-rich](https://github.com/Yevgnen/ivy-rich) and
[all-the-icons](https://github.com/domtronn/all-the-icons.el).

## Install

### Manual

From melpa, `M-x package-install RET all-the-icons-ivy-rich RET`.

```emacs-lisp
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)
```

### Use-package

```emacs-lisp
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

```

_NOTE_:

1. `all-the-icons-ivy-rich-mode` depends on
   [ivy-rich](https://github.com/Yevgnen/ivy-rich) and respects `ivy-rich-mode`.
1. To display icons correctly, you should run `M-x all-the-icons-install-fonts`
   to install the necessary fonts.
1. For better performance, enable `all-the-icons-ivy-rich-mode` before `ivy-rich-mode` .
1. Enable other packages like `counsel-projectile` before enabling `all-the-icons-ivy-rich-mode`.

Enjoy! :smile:

## Customize

```emacs-lisp
;; Whether display the icons
(setq all-the-icons-ivy-rich-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ivy-rich-color-icon t)

;; The icon size
(setq all-the-icons-ivy-rich-icon-size 1.0)

;; Whether support project root
(setq all-the-icons-ivy-rich-project t)

;; Maximum truncation width of annotation fields.
;; This value is adjusted depending on the `window-width'.
(setq all-the-icons-ivy-rich-field-width 80)

;; Definitions for ivy-rich transformers.
;; See `ivy-rich-display-transformers-list' for details."
all-the-icons-ivy-rich-display-transformers-list

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)
```

In `counsel-describe-function`, `counsel-describe-variable`and `counsel-describe-symbol`, the symbol
classes will be displayed. The details are below.

| Function                   | Variable                                       | Other     |
| -------------------------- | ---------------------------------------------- | --------- |
| f function                 | u custom (U modified compared to global value) | a face    |
| c cooamd                   | v variable                                     | t cl-type |
| C interactive-only command | l local (L modified compared to default value) |           |
| m macto                    | - obsolete                                     |           |
| M special form             |                                                |           |
| g cl-generic               |                                                |           |
| p pure                     |                                                |           |
| s side-effect-free         |                                                |           |
| @ autoloaded               |                                                |           |
| ! advised                  |                                                |           |

## Screenshots

![ivy-switch-buffer](https://user-images.githubusercontent.com/140797/154795765-786a29c2-3dc6-4a81-9992-fcd7043ae1ab.png "ivy-switch-buffer")

![counsel-find-file](https://user-images.githubusercontent.com/140797/154795929-0987d4fe-14d8-4866-bf98-1e95d5493014.png "counsel-find-file")

![counsel-buffer-or-recentf](https://user-images.githubusercontent.com/140797/154795792-f95a119f-c313-4b1f-b32f-9e312bb2fa15.png "counsel-buffer-or-recentf")

![counsel-M-x](https://user-images.githubusercontent.com/140797/154795826-0fb8f5ea-825a-4108-a565-daeb5a6e7e96.png "counsel-M-x")

![counsel-describe-function](https://user-images.githubusercontent.com/140797/154796653-718aabfa-dca8-4478-afa1-6272f1399362.png "counsel-describe-function")

![counsel-describe-variable](https://user-images.githubusercontent.com/140797/154796713-f2d11548-83bb-46d1-bb0b-a1b621cac5b7.png "counsel-describe-variable")

![counsel-imenu](https://user-images.githubusercontent.com/140797/154795862-a56b92a4-be07-42d7-9fec-9392e87cb83c.png "counsel-imenu")

![counsel-bookmark](https://user-images.githubusercontent.com/140797/154795890-3b86a6c6-850c-4153-afdd-748d503ff265.png "counsel-bookmark")

And more...

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<img
src="https://user-images.githubusercontent.com/140797/65818854-44204900-e248-11e9-9cc5-3e6339587cd8.png"
alt="Alipay" width="120"/>
&nbsp;&nbsp;&nbsp;&nbsp;
<img
src="https://user-images.githubusercontent.com/140797/65818844-366ac380-e248-11e9-931c-4bd872d0566b.png"
alt="Wechat Pay" width="120"/>

<a href="https://paypal.me/seagle0128" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.buymeacoffee.com/s9giES1" target="_blank">
<img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee"
width="160"/>
</a>
