;;; packages.el --- Melpa packages -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2013-2020 gniuk <isgniuk@gmail.com>

;;; Commentary:
;; none

;;; Code:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

(require 'package)
(setq-default package-check-signature nil) ; failed to verify signature auctex
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    ;(add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.emacs-china.org/gnu/")))))
(package-initialize)

(defvar required-packages
  '(
    company
    ;company-c-headers
    company-anaconda
    yasnippet
    yasnippet-snippets
    ;autopair ; replaced by smartparens
    ;paredit ; replaced by smartparens
    switch-window
    expand-region
    ;hl-line
    helm
    ;helm-gtags ; rarely used now but maybe useful in some bare place
    js2-mode
    graphviz-dot-mode
    goto-last-change
    nasm-mode
    undo-tree
    ;tern
    ;company-tern
    rainbow-delimiters
    ;rainbow-identifiers ; use color-identifiers-mode instead
    ;auto-highlight-symbol
    ;smartscan ; symbol_with_underscore not recognized. using highlight-symbol to navigate
    ;ace-jump-mode
    ;fill-column-indicator
    ;col-highlight ; not in melpa now, put it in nonmelpa
    ;company-ycmd
    flycheck
    helm-flycheck
    ;; irony
    ;; company-irony
    ;; company-irony-c-headers
    ;; irony-eldoc
    ;; flycheck-irony
    ;; helm-rtags ; Long time not used, mark irony+rtags as a stand-by. Uncomment and call use-rtags to use them.
    ;projectile ; as a requirement of helm-projectile
    helm-projectile
    ;; color-theme-sanityinc-solarized
    ;; color-theme-sanityinc-tomorrow
    ;; afternoon-theme ; mark the ex color theme
    magit
    cmake-mode
    eldoc-cmake
    web-mode
    company-web
    web-beautify
    helm-css-scss
    css-eldoc
    emmet-mode
    helm-emmet
    ;guide-key ; replaced by which-key
    helm-c-yasnippet
    multiple-cursors
    ace-jump-buffer
    quickrun
    hlinum  ;; replace hl-line
    ;smooth-scrolling ;; makes emacs laggy, just tuning the builtin scroll settings
    readline-complete
    ;dired+ ; no more dired+ in melpa, use plain builtin dired, or just neotree is enough
    ;neotree ; replaced by treemacs
    ;indent-guide ; interfere company popup window, use highlight-indent-guides instead
    window-numbering
    anzu
    buffer-move
    mwim
    ;codesearch
    ;projectile-codesearch  ;; mark it
    helm-swoop
    ace-jump-zap
    ace-pinyin
    company-auctex
    shell-pop
    ;helm-company
    popwin
    youdao-dictionary
    color-identifiers-mode
    chinese-yasdcv
    helm-ag
    ;; helm-rg ; now use rg with helm-ag, helm-rg seems no need.
    ;god-mode
    which-key
    ;workgroups2
    ;ace-isearch
    ;highlight-symbol
    highlight-indent-guides
    company-shell
    pcmpl-args
    go-mode
    company-go
    go-eldoc
    gotest
    go-playground
    markdown-mode
    flymd
    pandoc-mode
    doom-modeline
    ;; doom-themes
    evil
    evil-matchit
    vterm
    golden-ratio
    disaster
    spacemacs-theme
    pyim
    ;; company-statistics ; seems no need?
    x86-lookup
    org-bullets
    symbol-overlay
    vc-msg
    smartparens
    git-timemachine
    eyebrowse
    dumb-jump
    htmlize
    ccls
    ;company-lsp ; use company-capf instead
    helm-lsp
    helm-xref
    lsp-ui
    dtrt-indent
    realgud
    cheat-sh
    treemacs ; replace neotree
    avy
    ascii-table
    nov
    tiny
    restclient
    restclient-helm
    iasm-mode
    rfc-mode
    ;evil-escape ; my Ctrl key is convenient :)
    ;evil-vimish-fold ; overrides evil default fold, not good
    vimish-fold
    git-gutter
    ;vdiff-magit ; patience is not good as expected, ediff is enough, beyond compare is the goal
    ranger
    ;; rg ; helm-ag is good enough
    ;; company-restclient ; mark
    ;; org-brain ; mark
    ;; benchmark-init
    ) "A list of packages to ensure that they have been installed.")

(require 'cl-lib)

(cl-defun packages-installed-p ()
  "Method to check if all packages are installed."
  (cl-loop for p in required-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (cl-dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'packages)

;;; packages.el ends here
