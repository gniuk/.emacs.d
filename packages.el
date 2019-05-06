; packages.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar required-packages
  '(
    company
    company-c-headers
    company-web
    company-anaconda
    yasnippet
    yasnippet-snippets
    autopair
    paredit
    switch-window
    expand-region
    ;hl-line
    helm
    helm-gtags
    js2-mode
    graphviz-dot-mode
    goto-last-change
    nasm-mode
    undo-tree
    tern
    company-tern
    rainbow-delimiters
    rainbow-identifiers
    auto-highlight-symbol
    smartscan
    ace-jump-mode
    ;fill-column-indicator
    ;col-highlight ; not in melpa now, put it in nonmelpa
    ;company-ycmd
    flycheck
    helm-flycheck
    irony
    company-irony
    company-irony-c-headers
    flycheck-irony
    projectile
    helm-projectile
    rtags
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    afternoon-theme
    magit
    cmake-mode
    ;web-mode  ;; not yet needed
    ;guide-key ; replaced by which-key
    nyan-mode
    helm-c-yasnippet
    multiple-cursors
    ace-jump-buffer
    quickrun
    hlinum  ;; replace hl-line
    smooth-scrolling
    readline-complete
    dired+
    neotree
    indent-guide
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
    helm-company
    popwin
    youdao-dictionary
    color-identifiers-mode
    chinese-yasdcv
    helm-ag
    god-mode
    which-key
    ) "a list of packages to ensure are installed at launch.")

(require 'cl)

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
