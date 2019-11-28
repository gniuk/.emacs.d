;;; init-packages.el --- configuration file for packages in packages.el

;;; Commentary:
;; none

;;;;;======================= packages' configuration =======================;;;;;
;;; Code:

(load "~/.emacs.d/packages.el")

;;; company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-sort-by-occurrence t)
(global-set-key (kbd "TAB") 'company-files)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;;; yasnippet
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)
;(yas-load-directory "~/.emacs.d/snippets")
; here you may need to change the version number after upgrading yasnippet.
;(yas-load-directory "~/.emacs.d/elpa/yasnippet-20151126.518/snippets")
;; no more yas-load-directory for official snippets,
;; yasnippet will automatically find yasnippet-snippets-whatever.xxxxxx


;;; autopair
(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t)

;;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; switch-window
(global-set-key (kbd "C-x o") 'switch-window)

;;; expand-region
(require 'expand-region)
;(global-set-key (kbd "C-=") 'er/expand-region) ; use it in god-mode, so efficient. 2017-07-27
;(global-set-key (kbd "M-+") 'er/expand-region) ; the Ctrl key not functioning well with some keys on remote server via ssh, change to Meta(Alt) key
;(global-set-key (kbd "C-M-h") 'er/mark-defun) ; the key captured by fcitx to type english words, but we can use it in god-mode, good. 2017-07-27
; see the override keybinds. 2019-11-24

; 2019-11-24, set key bindings for the er/mark function group
(global-set-key (kbd "C-c m f") 'er/mark-defun)
(global-set-key (kbd "C-c m p") 'er/mark-inside-pairs)
(global-set-key (kbd "C-c m q") 'er/mark-inside-quotes)
(global-set-key (kbd "C-c m u") 'er/mark-url)

;;; hl-line, highlight current line. Replaced by hlinum 2015-11-11, double 11, yeah.
;(global-hl-line-mode t)

;;; helm
(require 'helm-config)
(helm-mode 1)
(helm-adaptive-mode 1)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "M-x") 'helm-M-x)
    ;-------2015-5-18
    ;use Ctrl-J to goto next dir in helm Ctrl-X Ctrl-F, not Enter
    ;Ctrl-x Ctrl-F Ctrl-? to see help
    ;(global-set-key (kbd "M-x") 'helm-M-x); Great info!
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    ;C-x C-f C-s to show files greping lines match regexp
    (global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-M-x-fuzzy-match t)
    ;-------

;  C-M-f     Move forward over a balanced expression
;  C-M-b     Move backward over a balanced expression
;  C-M-k     Kill balanced expression forward
;  C-M-h     mark a whole function
;  C-M-SPC   put the mark at the end of the sexp.
;  C-M-n  Move forward over a parenthetical group
;  C-M-p  Move backward over a parenthetical group
;  C-M-t  trans balanced block

;  ;; C-M key binding can also be done by --> ESC Control-key
;M-x customize-group RET smartparens RET
;Old key M-j to avoid indent
;C-M-a runs beginning-of-defun, which moves point to beginning of a function
;C-M-e runs end-of-defun, which moves point to end of a function

;;; goto-last-change
(require 'goto-last-change)
(global-set-key (kbd "C-c l l") 'goto-last-change-with-auto-marks)

;;; Python
;(require 'python-mode)
;(add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(eval-after-load "company"
 '(progn
    (add-to-list 'company-backends 'company-anaconda)
    ))
(add-hook 'python-mode-hook 'anaconda-mode)
(eval-after-load "anaconda-mode"
  '(progn
     ;(define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
     (define-key anaconda-mode-map (kbd "M-s") 'anaconda-mode-find-assignments)))
(setq python-indent-offset 4)

;;; C
(eval-after-load "company"
 '(progn
    (add-to-list 'company-backends 'company-c-headers)
    (remove 'company-semantic company-backends)))
; delete should be in the eval-after-load ! yeah! (Symbol's value as variable is void: company-backends) GONE!
;(setq company-backends (delete 'company-semantic company-backends))
;(setq company-backends (remove 'company-semantic company-backends))

; we have to delete company-semantic, otherwise company-complete will use company-semantic instead of company-clang



;;; nodejs
;; sudo npm install -g tern
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(eval-after-load "company"
 '(progn
    (add-to-list 'company-backends 'company-tern)))
;(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode t)))

;; (add-hook 'javascript-mode 'js2-mode)

;;; undo-tree
(global-undo-tree-mode 1)
(global-set-key (kbd "C-c z") 'undo)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-c Z") 'redo)

;;; rainbow
(add-hook 'company-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'company-mode-hook 'rainbow-identifiers-mode)

;;; auto-highlight-symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
; golang was invented in 2009, and this package is write in 2010 :), no go-mode in ahs-modes !
(add-hook 'go-mode-hook 'auto-highlight-symbol-mode)
; no auto-highlight-symbol-mode in gnu gas asm-mode too, add it.
(add-hook 'asm-mode-hook 'auto-highlight-symbol-mode)
; cmake-mode too.
(add-hook 'cmake-mode-hook 'auto-highlight-symbol-mode)

;;; helm-gtags
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (custom-set-variables
;;  '(helm-gtags-path-style 'relative)
;;  '(helm-gtags-ignore-case t)
;;  '(helm-gtags-auto-update t))

(eval-after-load "helm-gtags"
      '(progn
         (define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
         (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
         (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
         (define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-parse-file)
         (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
         (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
         (define-key helm-gtags-mode-map (kbd "C-c g ,") 'helm-gtags-pop-stack)))

;;;smartscan
;; ; smart scan replace text! M-' good!
;; (global-smartscan-mode 1)
;; (add-hook 'company-mode-hook 'global-smartscan-mode)
;; (global-set-key (kbd "M-S-n") 'smartscan-symbol-go-forward)
;; (global-set-key (kbd "M-S-p") 'smartscan-symbol-go-backward)
; bug, symbol_with_underscore-minus not recognized. replace text maybe use highlight-symbol replace.
;;; load global-set.el
;(load "~/.emacs.d/global-set.el")

;;; ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;(global-set-key (kbd "C-c j") 'ace-jump-mode)
;(global-set-key (kbd "C-'") 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
;(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;; fill-column-indicator
;(require 'fill-column-indicator)

;;; col-highlight
(push "~/.emacs.d/nonmelpa/col-highlight" load-path)
(require 'col-highlight)
(setq col-highlight-period 2)
(global-set-key (kbd "<f8>") 'flash-column-highlight) ; manually flash current column when necessary

;;; ycmd
;; (require 'ycmd)
;; (add-hook 'after-init-hook #'global-ycmd-mode)
;; (require 'company-ycmd)

;;; built-in hs-minor-mode
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'cc-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'hs-minor-mode)
(add-hook 'go-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c h h") 'hs-hide-all)
(global-set-key (kbd "C-c h a") 'hs-show-all)
(global-set-key (kbd "C-c h t") 'hs-toggle-hiding)

;;; flycheck
;; for python, sudo pip intall pylint
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; irony-mode
;; M-x irony-install-server to install server
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

(require 'company-irony-c-headers)
   ;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; irony-eldoc
(add-hook 'irony-mode-hook #'irony-eldoc)

;;; projectile
(projectile-global-mode)
(global-set-key (kbd "C-x p f") 'helm-projectile)
(global-set-key (kbd "C-x p s") 'helm-projectile-ag) ; silversearcher-ag needed, use distribution package manager to install it
(global-set-key (kbd "C-x p g") 'helm-projectile-grep) ; use grep if silversearcher-ag not present
(which-function-mode t)

;;; rtags
;; install clang, llvm...
;; https://github.com/Andersbakken/rtags, need to install rtags first
;; cd rtags; mkdir build && cd build; cmake ..; make; sudo make install
;; in project, cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 or use bear with other
;; build system to generate clang compile-commands.json database.
;; rdm &
;; rc -J .
;(push "/usr/local/share/emacs/site-lisp/rtags" load-path) ; emacs won't find rtags.el in /usr/local/share/emacs/site-lisp/
;(push "/usr/share/emacs/site-lisp/rtags" load-path) ; emacs will find rtags.el in /usr/share/emacs/site-lisp/, rtags cmake -DCMAKE_INSTALL_PREFIX=/usr .., using rtags-2.22
(require 'rtags)

(require 'company-rtags)
(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
(require 'helm-rtags)
(setq rtags-use-helm t)
(setq rtags-display-result-backend 'helm)

(define-key irony-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key irony-mode-map (kbd "M-,") 'rtags-location-stack-back)
(define-key irony-mode-map (kbd "M-r") 'rtags-find-references-at-point)
(define-key irony-mode-map (kbd "C-<") 'rtags-find-virtuals-at-point)
;(define-key irony-mode-map (kbd "M-i") 'rtags-imenu) ; use C-c r I. M-i used for helm-swoop!

;;; shell-mode, comint-previous-input and comint-next-input
;; use <C-up>, <C-down> to step find history, (M-p and M-n are override by
;; smartscan). Or, M-r to start search, and then C-r to find history.
;; C-c C-p goto last shell prompt
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map (kbd "M-<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "M-<down>") 'comint-next-input)
            (define-key comint-mode-map (kbd "ESC <up>") 'comint-next-input) ;used in console with Alt(Meta) key
            (define-key comint-mode-map (kbd "ESC <down>") 'comint-next-input)))

(add-hook 'shell-mode-hook
     '(lambda () (toggle-truncate-lines 1)))
;(setq comint-prompt-read-only t)    ; When debugging in output, sometimes need select all(C-x h) to clear. readonly prevent this.

;; ;;; theme
;; (require 'color-theme-sanityinc-solarized)
;; (load-theme 'sanityinc-solarized-light)

;; (color-theme-sanityinc-solarized--define-theme light)

;;; cmake-mode
(require 'cmake-mode)

;;; guide-key ; replaced by which-key now
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence t)
;; (setq guide-key/idle-delay 1)
;; (guide-key-mode 1)

;;; nyan-mode
;(nyan-mode 1)

;;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m a") 'mc/mark-all-in-region)
(global-set-key (kbd "C-M-=") 'mc/mark-next-like-this)

;;; ace-jump-buffer
(require 'ace-jump-buffer)
(global-set-key (kbd "C-x SPC") 'ace-jump-buffer)

;;; quickrun
(require 'quickrun)
(global-set-key (kbd "<f5>") 'quickrun)
; or use M-|, shell-command-on-region to execute on region

;;; hlinum
(require 'hlinum)
(hlinum-activate)
(setq linum-highlight-in-all-buffersp t)

;;; smooth-scrolling
(require 'smooth-scrolling)

;;; readline-complete [awesome!]
(require 'readline-complete)
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-readline))
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
; noting, color in shell(powered by $TERM=xterm,xterm-256color,screen. etc)
; conflicts to readline-complete in shell. Finally find it!


;;; neotree
(require 'neotree)
(global-set-key (kbd "<f7>") 'neotree-toggle)

;;; indent-guide. replaced by highlight-indent-guides
;; (indent-guide-global-mode t)
;; ;(set-face-background 'indent-guide-face "dimgray")
;; (setq indent-guide-delay 0.1)
;; (setq indent-guide-recursive t)
;; (setq indent-guide-char "|")

;;; window-numbering
(window-numbering-mode t)

;;; anzu
(global-anzu-mode 1)

;;; buffer-move
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;; mwim
(autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
(autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
(autoload 'mwim-end-of-code-or-line "mwim" nil t)
(autoload 'mwim-end-of-line-or-code "mwim" nil t)
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)

;;; helm-swoop
(require 'helm)
(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
;(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; ;; Match/Search methods (Fuzzy matching, Migemo)
;; ;; If you do not preferr fuzzy, remove it from the list below
;; (defvar helm-c-source-swoop-match-functions
;;   '(helm-mm-exact-match
;;     helm-mm-match
;;     helm-fuzzy-match
;;     helm-mm-3-migemo-match))
;; (setq helm-c-source-swoop-search-functions
;;       '(helm-mm-exact-search
;;         helm-mm-search
;;         helm-candidates-in-buffer-search-default-fn
;;         helm-fuzzy-search
;;         helm-mm-3-migemo-search))

;;; ace-jump-zap-to-char
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)

;;; ace-pinyin
(require 'ace-pinyin)
(setq ace-pinyin-use-avy nil)
(ace-pinyin-global-mode +1)
(turn-on-ace-pinyin-mode)
;(global-set-key (kbd "C-'") 'ace-pinyin-jump-char)
(global-set-key (kbd "C-c j") 'ace-pinyin-jump-char)
(global-set-key (kbd "C-.") 'ace-pinyin-jump-char)
(global-set-key (kbd "C-c SPC") 'ace-pinyin-jump-char)
(global-set-key (kbd "M-g a") 'ace-pinyin-jump-char)
;; override C-' in org-mode
;; (eval-after-load 'org
;;   '(define-key org-mode-map (kbd "C-'") 'ace-pinyin-jump-char))
;; (eval-after-load 'org
;;   '(setq org-log-done t))
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-'") 'ace-pinyin-jump-char)
            (setq org-log-done t)))

;;; auctex
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)

;;; company-auctex
(require 'company-auctex)
(company-auctex-init)

;;; shell-pop
(require 'shell-pop)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(shell-pop-default-directory ".")
 '(shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (shell shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "<f1>")
 '(shell-pop-window-size 45)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "right"))

;;; dot in org-mode
; #+BEGIN_SRC dot :file dot_output.png :cmdline -Kdot -Tpng
; #+END_SRC
; C-c C-c to run src code
; put cursor on link, C-c C-x v to toggle view pic
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;;; proced
(global-set-key (kbd "C-x p p") 'proced)

;;; helm-company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;;; popwin
(require 'popwin)
(popwin-mode 1)

;;; youdao-dictionary
; sudo pacman -S ydcv
; sudo npm install fanyi -g
; ydcv, fanyi not required, just tools used in console
;; Enable Cache
(setq url-automatic-caching t)

;; Example Key binding
(global-set-key (kbd "C-c f") 'youdao-dictionary-search-at-point)

;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
(push "*Youdao Dictionary*" popwin:special-display-config)

;; Set file path for saving search history
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;;; color-identifiers-mode
(add-hook 'after-init-hook 'global-color-identifiers-mode)

(let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face)))
  (dolist (face faces)
    (set-face-attribute face nil :foreground nil :weight 'normal :slant 'normal)))

(set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
(set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)

;;; cc-mode
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "C-c C-d") 'kill-whole-line)))

;;; chinese-yasdcv
; 1. commandline tool "sdcv"(stardict command version) needed
; 2. http://download.huzheng.org/ to download offline dicts, uncompress and put them in ~/.stardict/dic/ (see man sdcv)
; C-h v to see the variables
(require 'chinese-yasdcv)
(setq yasdcv-sdcv-dicts
      '(("oald_cn" "牛津高阶英汉双解" "oald" nil)
        ("21shijishuangxiangcidian" "21世纪英汉汉英双向词典" "21cen" nil)
        ("langdao-ec-gb" "朗道英汉字典5.0" "langdao" t)
        ("langdao-ce-gb" "朗道汉英字典5.0" "langdao" t)
        ("hanyuchengyucidian" "汉语成语词典" "chengyu" nil)
        ("chengyudacidian" "中华成语大词典 2.1" "chengyu" t)))
(global-set-key (kbd "C-c d") 'yasdcv-translate-at-point)
; 第一个参数，字典代号(不重要)
; 第二个参数，字典名(重要，在所有字典ifo文件中查找对应 bookname)
; 第三个参数，自定义的输出美化函数
; 第四个参数，是否开启该词典

;;; god-mode
;;; My left hand holding a cup of water, my right can navigate without my left hand.
;;; Sometimes I feel nervous when I'm not able to do that. Evil is not consistent
;;; with my flavor or the emacs environment, omit it.
;;; Now I'm happy.
;;; Most important, the navigation keys is my emacs key bindings without the
;;; modify key while the original key bindings are still useful there.
;;; And C-x C-q can be used to enter readonly mode in order to avoid mistyping. Greate!
(require 'god-mode)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
;(global-set-key (kbd "<escape>") 'god-local-mode)
;(global-set-key (kbd "C-c g") 'god-local-mode)
; set "M-g g" to god-mode, now only "M-g M-g" binding to goto-line
(global-set-key (kbd "M-g g") 'god-local-mode)
; oh, somelike vim. OK, I now use both emacs and vim.
; use xmodmap to map escape key to caps_lock.
;(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "z") 'repeat) ; bind z to repeat, . is for C-. in ace-pinyin-jump
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(add-to-list 'auto-mode-alist '("\\.asm$" . nasm-mode))

;;; custom Emacs key set
(global-unset-key (kbd "C-z")) ; C-z is suspend-frame, in case of typing it by mistake, disable it
(global-unset-key (kbd "C-x C-b"))      ; it is for list-buffers, no need
(global-set-key (kbd "C-x C-b") 'helm-mini) ; rebind it to helm-mini, then use it in god-mode, then use xb to switch buffer quickly.

;;; which-key, replace guide-key
(which-key-mode)
(setq which-key--god-mode-support-enabled t)

;;; diminish
(diminish 'global-flycheck-mode)
(diminish 'paredit-mode)
(diminish 'global-undo-tree-mode)
(diminish 'undo-tree-mode)
(diminish 'helm-mode)
(diminish 'projectile-mode)
(diminish 'flycheck-mode)
(diminish 'company-mode "CP")

;;; workgroups2
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c w"))      ; default is C-c z. c,v,A,k,C-s,C-f
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)

;;; ace-isearch. ace-jump-mode, isearch, helm-swoop
; L = 1     : `ace-jump-mode' or `avy'
; 1 < L < 6 : `isearch'
; L >= 6    : `helm-swoop' or `swiper'
(require 'ace-isearch)
(global-ace-isearch-mode +1)
(setq ace-isearch-jump-delay 0.8)

;;; highlight-symbol, replace smart-scan using highlight-symbol
(require 'highlight-symbol)
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;;; highlight-indent-guides, replace indent-guide.
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-delay 0.5)

;;; company-shell. $PATH bin, fish-shell-builtin, env
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-shell company-shell-env company-fish-shell)))

;;; pcmpl-args
(require 'pcmpl-args)

;;; company-go
(eval-after-load "company"
 '(progn
    (add-to-list 'company-backends 'company-go)))
;(require 'company-go)

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
;; customize company popup menu (actually it is global company). More comfortable in terminal!
;; set TERM=xterm-256color to fix the color theme problem in terminal!!
;; (custom-set-faces
;;  '(company-preview
;;    ((t (:foreground "darkgray" :underline t))))
;;  '(company-preview-common
;;    ((t (:inherit company-preview))))
;;  '(company-tooltip
;;    ((t (:background "lightgray" :foreground "black"))))
;;  '(company-tooltip-selection
;;    ((t (:background "steelblue" :foreground "white"))))
;;  '(company-tooltip-common
;;    ((((type x)) (:inherit company-tooltip :weight bold))
;;     (t (:inherit company-tooltip))))
;;  '(company-tooltip-common-selection
;;    ((((type x)) (:inherit company-tooltip-selection :weight bold))
;;     (t (:inherit company-tooltip-selection)))))

;;; go mode

;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; go-eldoc
;(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; flymd
;(require 'flymd)

;;; pandoc-mode
(add-hook 'markdown-mode-hook 'pandoc-mode)
;(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;; emacs-livedown
(push "~/.emacs.d/nonmelpa/emacs-livedown/" load-path)
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser nil))  ; browser to use
(require 'livedown)

;;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)

;;; evil
(evil-mode 1)

;;; override keybinds
; expand-region and multiple-cursor mark next
;(define-key window-numbering-keymap (kbd "M-8") nil) ; we have no eight windows in one small screen. use the precious and convenient keybinding.
;(global-set-key (kbd "M-8") 'mc/mark-next-like-this-symbol) ; M-8 and M-= can be a pair to select arbitrary symbol pattern.
;(global-set-key (kbd "M-=") 'er/expand-region) ; M-= is for emacs count-words, not used frequently, bind to expand-region. use M-x to count-words.

(define-key window-numbering-keymap (kbd "M-9") nil)
(global-set-key (kbd "M-9") 'mc/mark-next-like-this-symbol)
(define-key window-numbering-keymap (kbd "M-0") nil)
(global-set-key (kbd "M-0") 'er/expand-region)

(define-key window-numbering-keymap (kbd "M-8") nil)
(global-set-key (kbd "M-8") 'mc/edit-lines)

; override c-toggle-auto-newline in C and C++, using custom function
(define-key c-mode-map (kbd "C-c C-a") nil)
(define-key c++-mode-map (kbd "C-c C-a") nil)

; override evil key bindings
; override C-e evil-scroll-line-down, make it original mwim-end-of-code-or-line
(define-key evil-motion-state-map (kbd "C-e") nil)
; rtags find definition or other jump to definition commands
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "M-.") nil)
; i to evil-emacs-state, C-z to evil-normal-state
(define-key evil-normal-state-map (kbd "i") 'evil-emacs-state)
(define-key evil-normal-state-map (kbd "C-z") 'evil-normal-state)
; C-p previous-line, C-n next-line
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-n") 'next-line)
; C-v not act as VISUAL MODE, but original emacs scroll-up-command, pair with M-v. Block editing with Emacs.
(define-key evil-normal-state-map (kbd "C-v") 'scroll-up-command)
; C-f, C-b act as in emacs. Use only C-v, M-v to scroll page.
(define-key evil-normal-state-map (kbd "C-f") 'forward-char)
(define-key evil-normal-state-map (kbd "C-b") 'backward-char)

;;; other config
; automatic delete trailing spaces when saving file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;Turn on documentation in elisp mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	     (turn-on-eldoc-mode)))

(provide 'init-packages)
;;; init-packages.el ends here
