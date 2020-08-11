# all-the-icons-ivy-rich

[![Build Status](https://github.com/seagle0128/all-the-icons-ivy-rich/workflows/CI/badge.svg?branch=master)](https://github.com/seagle0128/all-the-icons-ivy-rich/actions)
[![MELPA](https://melpa.org/packages/all-the-icons-ivy-rich-badge.svg)](https://melpa.org/#/all-the-icons-ivy-rich)
[![MELPA Stable](https://stable.melpa.org/packages/all-the-icons-ivy-rich-badge.svg)](https://stable.melpa.org/#/all-the-icons-ivy-rich)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [all-the-icons-ivy-rich](#all-the-icons-ivy-rich)
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

``` emacs-lisp
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)
```

### Use-package

``` emacs-lisp
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

```

*NOTE*:

1. `all-the-icons-ivy-rich-mode` depends on
   [ivy-rich](https://github.com/Yevgnen/ivy-rich) and respects `ivy-rich-mode`.
1. To display icons correctly, you should run `M-x all-the-icons-install-fonts`
   to install the necessary fonts.
1. For better performance, enable `all-the-icons-ivy-rich-mode` before `ivy-rich-mode` .
1. Enable other packages like `counsel-projectile` before enabling `all-the-icons-ivy-rich-mode`.

Enjoy! :smile:

## Customize

``` emacs-lisp
;; The icon size
(setq all-the-icons-ivy-rich-icon-size 1.0)

;; Definitions for ivy-rich transformers.
;; See `ivy-rich-display-transformers-list' for details."
all-the-icons-ivy-rich-display-transformers-list

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)
```

## Screenshots

![ivy-switch-buffer](https://user-images.githubusercontent.com/140797/73594570-6c97b700-454a-11ea-90bd-fdee19c4c4b2.png
"ivy-switch-buffer")

![counsel-buffer-or-recentf](https://user-images.githubusercontent.com/140797/73594526-2b070c00-454a-11ea-923d-d0621d589819.png
"counsel-buffer-or-recentf")

![counsel-M-x](https://user-images.githubusercontent.com/140797/73594512-0b6fe380-454a-11ea-9289-d6bfb8c53a38.png
"counsel-M-x")

![counsel-imenu](https://user-images.githubusercontent.com/140797/73594509-014de500-454a-11ea-89c9-7360ebe198b7.png
"counsel-imenu")

![counsel-bookmark](https://user-images.githubusercontent.com/140797/73594541-3e19dc00-454a-11ea-8e84-ac97f55518d2.png
"counsel-bookmark")

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
