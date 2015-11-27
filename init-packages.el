;;; init-packages.el --- configuration file for packages in packages.el

;;; Commentary:
;; none

;;;;;======================= packages' configuration =======================;;;;;
;;; Code:

(load "~/.emacs.d/packages.el")

;;; company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-sort-by-occurrence t)
(global-set-key (kbd "TAB") 'company-files)

;;; yasnippet
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)
;(yas-load-directory "~/.emacs.d/snippets")
; here you may need to change the version number after upgrading yasnippet.
(yas-load-directory "~/.emacs.d/elpa/yasnippet-20151108.1505/snippets")

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
(global-set-key (kbd "C-=") 'er/expand-region)
;(global-set-key (kbd "C-M-h") 'er/mark-defun) ; the key captured by fcitx.

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
     (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
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

;;; helm-gtags
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (eval-after-load "helm-gtags"
;;       '(progn
;;          (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
;;          (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;          (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;          (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;          (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;          (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;          (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;; smartscan
; smart scan replace text! M-' good!
(global-smartscan-mode 1)
(add-hook 'company-mode-hook 'global-smartscan-mode)
(global-set-key (kbd "M-S-n") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-S-p") 'smartscan-symbol-go-backward)

;;; load global-set.el
;(load "~/.emacs.d/global-set.el")

;;; ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
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
(global-set-key (kbd "<f8>") 'flash-column-highlight)

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

(eval-after-load 'company                         
  '(add-to-list 'company-backends 'company-irony))

(require 'company-irony-c-headers)
   ;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;; projectile
(projectile-global-mode)
(global-set-key (kbd "C-x p f") 'helm-projectile)
(which-function-mode t)

;;; rtags
;; install clang, llvm...
;; https://github.com/Andersbakken/rtags, need to install rtags first
;; cd rtags; mkdir build && cd build; cmake ..; make; sudo make install
;; in project, cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 or use bear with other
;; build system to generate clang compile-commands.json database.
;; rdm &
;; rc -J .
(define-key irony-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key irony-mode-map (kbd "M-,") 'rtags-location-stack-back)
(define-key irony-mode-map (kbd "M-r") 'rtags-find-references-at-point)
(define-key irony-mode-map (kbd "C-<") 'rtags-find-virtuals-at-point)
(define-key irony-mode-map (kbd "M-i") 'rtags-imenu)

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

;; ;;; theme
;; (require 'color-theme-sanityinc-solarized)
;; (load-theme 'sanityinc-solarized-light)

;; (color-theme-sanityinc-solarized--define-theme light)

;;; cmake-mode
(require 'cmake-mode)

;;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(setq guide-key/idle-delay 1)
(guide-key-mode 1)

;;; nyan-mode
(nyan-mode 1)

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

;;; neotree
(require 'neotree)
(global-set-key (kbd "<f7>") 'neotree-toggle)

;;; indent-guide
(indent-guide-global-mode t)
;(set-face-background 'indent-guide-face "dimgray")
(setq indent-guide-delay 0.1)
(setq indent-guide-recursive t)
(setq indent-guide-char "|")

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
(global-set-key (kbd "C-'") 'ace-pinyin-jump-char)
(global-set-key (kbd "C-c j") 'ace-pinyin-jump-char)
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
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

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
 '(shell-pop-window-size 40)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom"))

;;; dot in org-mode
; #+BEGIN_SRC dot :file dot_output.png :cmdline -Kdot -Tpng
; #+END_SRC
; C-c C-c to run src code
; put cursor on link, C-c C-x v to toggle view pic
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(provide 'init-packages)
;;; init-packages.el ends here

