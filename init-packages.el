;;; init-packages.el --- configuration file for packages in packages.el

;;; Commentary:
;; none

;;;;;======================= packages' configuration =======================;;;;;
;;; Code:

(load "~/.emacs.d/packages.el")
;; ;;; benchmark-init
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;;; company
;(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-sort-by-occurrence 1)
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay 0.2)                        ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-selection-wrap-around t)                    ; make previous/next selection cycle style
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-require-match nil)
  ;(setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "M-n") #'company-other-backend)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (define-key company-search-map (kbd "M-n") nil)
  (define-key company-search-map (kbd "M-p") nil)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous)
  ;(require 'company-statistics)
  (add-hook 'after-init-hook 'company-statistics-mode))

;(global-set-key (kbd "TAB") 'company-complete)
(defun gniuk/boip ()
  "Beginning of indentation -> bool."
  (let ((originalpos (point)))
    (save-excursion
      (back-to-indentation)
      (= originalpos (point)))))
(defun gniuk/indent-or-complete ()
  "Indent when at beginning otherwise try to complete."
  (interactive)
  (if (or (bolp) (gniuk/boip))
      (indent-according-to-mode)
    (company-complete)))
(global-set-key (kbd "TAB") 'gniuk/indent-or-complete)


; https://www.emacswiki.org/emacs/CompanyMode
; https://github.com/SXGNH/emacs/blob/e0ad40c09dbe8ac6f59089f0010febdf9774d7a4/theme/steelblue-theme.el
(custom-set-faces
 '(company-tooltip
   ((t (:background "wheat2" :foreground "gray28"))))
 '(company-tooltip-selection
   ((t (:background "SteelBlue1" :foreground "blue2"))))
 '(company-tooltip-annotation
   ((t (:foreground "red2"))))
 '(company-tooltip-annotation-selection
   ((t (:foreground "red2"))))
 '(company-tooltip-common
   ((t (:background "tan1" :foreground "black"))))
 '(company-tooltip-common-selection
   ((t (:background "SteelBlue1" :foreground "blue2"))))
  '(company-scrollbar-bg
   ((t (:background "gray15" :foreground "red"))))
 '(company-scrollbar-fg
   ((t (:background "red" :foreground "gray15")))))


;;; yasnippet
(with-eval-after-load 'yasnippet
  '(progn
     (setq helm-yas-space-match-any-greedy t)
     (add-to-list 'yas-snippet-dirs
                  "~/.emacs.d/mysnippets")))
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)

;;; yasnippet
;; (require 'yasnippet)
;; (require 'helm-c-yasnippet)
;; (setq helm-yas-space-match-any-greedy t)
;; (global-set-key (kbd "C-c y") 'helm-yas-complete)
;; (add-to-list 'yas-snippet-dirs
;;              "~/.emacs.d/mysnippets")
;; (yas-global-mode 1)
;; (with-eval-after-load 'yasnippet
;;   (add-to-list 'company-backends 'company-yasnippet -1))
;(yas-load-directory "~/.emacs.d/snippets")
; here you may need to change the version number after upgrading yasnippet.
;(yas-load-directory "~/.emacs.d/elpa/yasnippet-20151126.518/snippets")
;; no more yas-load-directory for official snippets,
;; yasnippet will automatically find yasnippet-snippets-whatever.xxxxxx


;;; autopair
;; using smartparens instead
;(require 'autopair)
;; (autopair-global-mode t)
;; (with-eval-after-load 'autopair
;;   '(setq autopair-autowrap t))


;;; paredit
;; using smartparens instead
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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
(helm-adaptive-mode 1)
(helm-autoresize-mode 1)
(global-set-key (kbd "C-c m i") 'helm-imenu)
(global-set-key (kbd "C-c m m") 'helm-man-woman)
(global-set-key (kbd "C-c m b") 'helm-resume)
(global-set-key (kbd "C-c m h") 'helm-apropos) ; help apropos, cover C-h f,v,m,...
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(with-eval-after-load 'helm
  (setq helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-bookmark-show-location t
        helm-buffers-fuzzy-matching t
        helm-completion-in-region-fuzzy-match t
        helm-ff-fuzzy-matching t
        helm-file-cache-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-locate-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-projectile-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        ;; http://tuhdo.github.io/helm-intro.html
        helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
        ;; helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
        ;; helm-echo-input-in-header-line t
        helm-ff-file-name-history-use-recentf t
        helm-imenu-execute-action-at-once-if-one nil)
  (custom-set-faces '(helm-selection ((t (:background "#fdf6e3" :foreground "firebrick3"))))
                    '(helm-selection-line ((t (:background "green" :foreground "black"))))))
(helm-mode 1)


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
;; (eval-after-load "company"
;;  '(progn
;;     (add-to-list 'company-backends 'company-anaconda)
;;     ))
(add-hook 'python-mode-hook 'anaconda-mode)
(with-eval-after-load "anaconda-mode"
  '(progn
     ;(define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
     (define-key anaconda-mode-map (kbd "M-s") 'anaconda-mode-find-assignments)))
(add-hook 'python-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-anaconda company-files company-yasnippet (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))))
(setq python-indent-offset 4)

;;; C
;; (eval-after-load "company"
;;  '(progn
;;     (add-to-list 'company-backends 'company-c-headers)
;;     (remove 'company-semantic company-backends)))
; delete should be in the eval-after-load ! yeah! (Symbol's value as variable is void: company-backends) GONE!
;(setq company-backends (delete 'company-semantic company-backends))
;(setq company-backends (remove 'company-semantic company-backends))

; we have to delete company-semantic, otherwise company-complete will use company-semantic instead of company-clang



;;; nodejs
;; sudo npm install -g tern
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (eval-after-load "company"
;;  '(progn
;;     (add-to-list 'company-backends 'company-tern)))
;(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-tern company-files company-yasnippet (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))
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
;; (require 'auto-highlight-symbol)
;; ;; Can not wrap ahs-forward or ahs-backward, don't know why, more elisp knowledge needed.
;; ;; May be try it again some days later. So now still use highlight-symbol to navigate.
;; ;; But highlight-symbol can navigate symbol in string, which ahs not able to.
;; ;; So use ahs to highlight and edit(with range control) symbol, hs to navigate.
;; ;; (spacemacs wraps the ahs functions successfully)
;; (ahs-set-idle-interval 1.2)
;; (setq ahs-default-range 'ahs-range-display)
;; (setq ahs-case-fold-search nil)
;; (define-key auto-highlight-symbol-mode-map (kbd "C-c s s") 'ahs-change-range)
;; (global-auto-highlight-symbol-mode t)
;; ; golang was invented in 2009, and this package is write in 2010 :), no go-mode in ahs-modes !
;; ;(add-hook 'go-mode-hook 'auto-highlight-symbol-mode)
;; (pushnew 'go-mode ahs-modes)
;; ; other modes
;; (pushnew 'org-mode ahs-modes)
;; (pushnew 'cmake-mode ahs-modes)
;; (pushnew 'asm-mode ahs-modes)
;; (pushnew 'nasm-mode ahs-modes)

;;; helm-gtags
;; ;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; ;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; ;; (add-hook 'asm-mode-hook 'helm-gtags-mode)
;; (add-hook 'prog-mode-hook 'helm-gtags-mode)

;; ;; (custom-set-variables
;; ;;  '(helm-gtags-path-style 'relative)
;; ;;  '(helm-gtags-ignore-case t)
;; ;;  '(helm-gtags-auto-update t))

;; (eval-after-load "helm-gtags"
;;   '(progn
;;      (define-key helm-gtags-mode-map (kbd "C-c g .") 'helm-gtags-find-tag-from-here)
;;      (define-key helm-gtags-mode-map (kbd "C-c g ,") 'helm-gtags-pop-stack)
;;      (define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
;;      (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
;;      (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
;;      (define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-parse-file)
;;      (define-key helm-gtags-mode-map (kbd "C-c g <") 'helm-gtags-previous-history)
;;      (define-key helm-gtags-mode-map (kbd "C-c g >") 'helm-gtags-next-history)))

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
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
;; ;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; ;(global-set-key (kbd "C-c j") 'ace-jump-mode)
;; ;(global-set-key (kbd "C-'") 'ace-jump-mode)
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; ;(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

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
(global-set-key (kbd "C-c h b") 'hs-hide-block)
(global-set-key (kbd "C-c h l") 'hs-hide-level)
(global-set-key (kbd "C-c h a") 'hs-show-all)
(global-set-key (kbd "C-c h s") 'hs-show-block)
(global-set-key (kbd "C-c h t") 'hs-toggle-hiding)

;;; flycheck
;; for python, sudo pip intall pylint
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ;;; irony-mode
;; ;; M-x irony-install-server to install server
;; (add-hook 'c-mode-common-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; ;; (eval-after-load 'company
;; ;;   '(add-to-list 'company-backends 'company-irony))

;; ;; (require 'company-irony-c-headers)
;; ;;    ;; Load with `irony-mode` as a grouped backend
;; ;; (eval-after-load 'company
;; ;;   '(add-to-list
;; ;;     'company-backends '(company-irony-c-headers company-irony)))

;; ;(require 'company-irony-c-headers)
;; ; Decouple company-irony-c-headers and company-irony, since company-irony also has header completions,
;; ; and it is bad. Let company-irony-c-headers take precedence over company-irony when complete header files.
;; ; Use company-diag to see all the backends and the backend currently used.
;; ;; (eval-after-load 'company
;; ;;   '(add-to-list
;; ;;     'company-backends 'company-irony-c-headers))
;; ; redundance backends
;; ;; (eval-after-load 'company
;; ;;   '(progn
;; ;;      (setq company-backends (delete 'company-clang company-backends))
;; ;;      (setq company-backends (delete 'company-capf company-backends))))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '(company-irony-c-headers company-irony company-files company-yasnippet (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))))

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; ;; irony-eldoc
;; (add-hook 'irony-mode-hook #'irony-eldoc)

;;; projectile and helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)
(setq helm-ag-insert-at-point 'symbol)
(global-set-key (kbd "C-x p f") 'helm-projectile-find-file-dwim)
;; (global-set-key (kbd "C-x p .") 'helm-projectile-ag) ; silversearcher-ag needed, use distribution package manager to install it
(global-set-key (kbd "C-x p s") 'helm-projectile-rg) ; ripgrep is faster. s means search
(global-set-key (kbd "C-x p ,") 'helm-ag-pop-stack) ; go back to where we do search using "C-x p s"
(global-set-key (kbd "C-x p g") 'helm-projectile-grep) ; use grep if silversearcher-ag or rg(ripgrep) not present
;; (global-set-key (kbd "C-x C-b") 'helm-projectile-recentf)
(global-set-key (kbd "C-x p b") 'helm-projectile-recentf)
(global-set-key (kbd "C-x p w") 'helm-projectile-switch-project)
(global-set-key (kbd "C-x p o") 'helm-projectile-find-other-file) ; switch between files with the same name but different extensions
(global-set-key (kbd "C-x p O") 'projectile-find-other-file-other-window)
(global-set-key (kbd "C-x p d") 'helm-projectile-find-dir)
(global-set-key (kbd "C-x p i") 'projectile-invalidate-cache)
(add-to-list 'projectile-globally-ignored-files "projectile.cache")
(add-to-list 'projectile-globally-ignored-files "company-statistics-cache.el")
(which-function-mode t)

;; ;;; rtags
;; ;; install clang, llvm...
;; ;; https://github.com/Andersbakken/rtags, need to install rtags first
;; ;; cd rtags; mkdir build && cd build; cmake ..; make; sudo make install
;; ;; in project, cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 or use bear with other
;; ;; build system to generate clang compile-commands.json database.
;; ;; rdm &
;; ;; rc -J .
;; ;(push "/usr/local/share/emacs/site-lisp/rtags" load-path) ; emacs won't find rtags.el in /usr/local/share/emacs/site-lisp/
;; ;(push "/usr/share/emacs/site-lisp/rtags" load-path) ; emacs will find rtags.el in /usr/share/emacs/site-lisp/, rtags cmake -DCMAKE_INSTALL_PREFIX=/usr .., using rtags-2.22
;; ;; (require 'rtags)

;; ; company-rtags backend has no return type for function, use only company-irony.
;; ;; (require 'company-rtags)
;; ;; (setq rtags-completions-enabled t)
;; ;; (eval-after-load 'company
;; ;;   '(add-to-list
;; ;;     'company-backends 'company-rtags))
;; ;; ;(setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)
;; ;(require 'helm-rtags)
;; (setq rtags-use-helm t)
;; (setq rtags-display-result-backend 'helm)

;; ;(define-key irony-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
;; ;(define-key irony-mode-map (kbd "M-,") 'rtags-location-stack-back)
;; ;(define-key irony-mode-map (kbd "M-r") 'rtags-find-references-at-point)
;; ;(define-key irony-mode-map (kbd "C-<") 'rtags-find-virtuals-at-point)
;; ;(define-key irony-mode-map (kbd "M-i") 'rtags-imenu) ; use C-c r I. M-i used for helm-swoop!
;; (add-hook 'irony-mode-hook '(lambda ()
;;                               (define-key irony-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
;;                               (define-key irony-mode-map (kbd "M-,") 'rtags-location-stack-back)
;;                               (define-key irony-mode-map (kbd "M-r") 'rtags-find-references-at-point)
;;                               (define-key irony-mode-map (kbd "C-<") 'rtags-find-virtuals-at-point)))

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

;;; cmake-mode
;(require 'cmake-mode)

;;; guide-key ; replaced by which-key now
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence t)
;; (setq guide-key/idle-delay 1)
;; (guide-key-mode 1)

;;; nyan-mode
;(nyan-mode 1)

;;; multiple-cursors
;(require 'multiple-cursors)
(global-set-key (kbd "C-c m a") 'mc/mark-all-in-region)
(global-set-key (kbd "C-M-=") 'mc/mark-next-like-this)

;;; ace-jump-buffer
;(require 'ace-jump-buffer)
(global-set-key (kbd "C-x SPC") 'ace-jump-buffer)

;;; quickrun
;(require 'quickrun)
(global-set-key (kbd "<f5>") 'quickrun)
; or use M-|, shell-command-on-region to execute on region

;;; hlinum
(require 'hlinum)
(hlinum-activate)
(setq linum-highlight-in-all-buffersp t)

;;; smooth-scrolling
;(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)
(smooth-scrolling-mode)

;;; readline-complete [awesome!]
;(require 'readline-complete)
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-readline))
;; (push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
; noting, color in shell(powered by $TERM=xterm,xterm-256color,screen. etc)
; conflicts to readline-complete in shell. Finally find it!


;;; neotree
;(require 'neotree)
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
;(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;; mwim
(autoload 'mwim-beginning-of-code-or-line-or-comment "mwim" nil t)
(autoload 'mwim-end-of-code-or-line "mwim" nil t)
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line-or-comment)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)

;;; helm-swoop
(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

(with-eval-after-load 'helm-swoop
  (setq
   ;; Save buffer when helm-multi-swoop-edit complete
   helm-multi-swoop-edit-save t

   ;; If this value is t, split window inside the current window
   helm-swoop-split-with-multiple-windows nil

   ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
   helm-swoop-split-direction 'split-window-vertically

   ;; If nil, you can slightly boost invoke speed in exchange for text color
   helm-swoop-speed-or-color nil

   ;; Go to the opposite side of line from the end or beginning of line
   helm-swoop-move-to-line-cycle t

   ;; Optional face for line numbers
   ;; Face name is `helm-swoop-line-number-face`
   helm-swoop-use-line-number-face t)

  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  ;; (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  (custom-set-faces
   '(helm-swoop-target-line-face
     ;; ((t (:background "#fdf6e3" :foreground "firebrick3")))) ; https://github.com/altercation/solarized/tree/master/emacs-colors-solarized
     ;; ((t (:foreground "orange red" :inverse-video t))))
     ((t (:foreground "#fdf6e3" :background "red"))))
   ;; '(helm-swoop-target-line-block-face ((t (:background "#fdf6e3" :foreground "firebrick3"))))
   '(helm-swoop-target-word-face ((t (:background "dark orchid" :foreground "#fdf6e3"))))))


;;; ace-jump-zap-to-char
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)

;;; ace-pinyin
;(require 'ace-pinyin)
(setq ace-pinyin-use-avy nil)
(ace-pinyin-global-mode +1)
(turn-on-ace-pinyin-mode)
;(global-set-key (kbd "C-'") 'ace-pinyin-jump-char)
;; (global-set-key (kbd "C-c j") 'ace-pinyin-jump-char)
;; (global-set-key (kbd "C-.") 'ace-pinyin-jump-char)
(global-set-key (kbd "C-c SPC") 'ace-pinyin-jump-char)
(global-set-key (kbd "M-g a") 'ace-pinyin-jump-char)

;;; auctex
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)

;;; company-auctex
;(require 'company-auctex)
(company-auctex-init)

;;; shell-pop
;(require 'shell-pop)
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
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))

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

;; (let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face)))
;;   (dolist (face faces)
;;     (set-face-attribute face nil :foreground nil :weight 'normal :slant 'normal)))

;; (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
;; (set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
;; (set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)

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

;; ;;; god-mode
;; ;;; My left hand holding a cup of water, my right can navigate without my left hand.
;; ;;; Sometimes I feel nervous when I'm not able to do that. Evil is not consistent
;; ;;; with my flavor or the emacs environment, omit it.
;; ;;; Now I'm happy.
;; ;;; Most important, the navigation keys is my emacs key bindings without the
;; ;;; modify key while the original key bindings are still useful there.
;; ;;; And C-x C-q can be used to enter readonly mode in order to avoid mistyping. Greate!
;; (require 'god-mode)
;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)
;; ;(global-set-key (kbd "<escape>") 'god-local-mode)
;; ;(global-set-key (kbd "C-c g") 'god-local-mode)
;; ; set "M-g g" to god-mode, now only "M-g M-g" binding to goto-line
;; (global-set-key (kbd "M-g g") 'god-local-mode)
;; ; oh, somelike vim. OK, I now use both emacs and vim.
;; ; use xmodmap to map escape key to caps_lock.
;; ;(define-key god-local-mode-map (kbd ".") 'repeat)
;; (define-key god-local-mode-map (kbd "z") 'repeat) ; bind z to repeat, . is for C-. in ace-pinyin-jump
;; (defun my-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'box
;;                       'bar)))

;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)

(add-to-list 'auto-mode-alist '("\\.asm$" . nasm-mode))

;;; custom Emacs key set
(global-unset-key (kbd "C-z")) ; C-z is suspend-frame, in case of typing it by mistake, disable it
(global-unset-key (kbd "C-x C-b"))      ; it is for list-buffers, no need
(global-set-key (kbd "C-x C-b") 'helm-mini) ; rebind it to helm-mini, then use it in god-mode, then use xb to switch buffer quickly.

;;; which-key, replace guide-key
(which-key-mode)
(setq which-key-idle-secondary-delay 0.1)
(setq which-key--god-mode-support-enabled t)

;;; workgroups2
;; not that useful, because helm-mini containing recentf is there, and we can use registers to save window or frame configuration.
;; (require 'workgroups2)
;; (setq wg-prefix-key (kbd "C-c w"))      ; default is C-c z. c,v,A,k,C-s,C-f
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;; (workgroups-mode 1)

;;; ace-isearch. ace-jump-mode, isearch, helm-swoop
; L = 1     : `ace-jump-mode' or `avy'
; 1 < L < 6 : `isearch'
; L >= 6    : `helm-swoop' or `swiper'
(require 'ace-isearch)
(global-ace-isearch-mode +1)
(setq ace-isearch-jump-delay 0.8)

;;; highlight-symbol, replace smart-scan using highlight-symbol
;; (require 'highlight-symbol)
;; (global-set-key (kbd "M-n") 'highlight-symbol-next)
;; (global-set-key (kbd "M-p") 'highlight-symbol-prev)

;;; highlight-indent-guides, replace indent-guide.
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-delay 0.5)

; emacs -q: list-colors-display, /usr/share/X11/rgb.txt, or https://jonasjacek.github.io/colors/
(setq highlight-indent-guides-auto-enabled nil)
(set-face-foreground 'highlight-indent-guides-character-face "grey23")

;;; company-shell. $PATH bin, fish-shell-builtin, env
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends '(company-shell company-shell-env company-fish-shell)))

; company-files not working in shell-mode, can't figure out yet
(add-hook 'shell-mode-hook '(lambda ()
                              (set (make-local-variable 'company-backends) '((company-shell company-shell-env company-fish-shell company-files) company-readline company-dabbrev))
                              (evil-emacs-state)
                              (linum-mode -1)))

;;; pcmpl-args
(require 'pcmpl-args)

;;; company-go
(add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go company-files company-yasnippet (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))))

;;; go mode

;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef

(defun my-go-mode-hook ()
  "Call Gofmt before saving."
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; go-eldoc
;(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;; set go-playground basedir, default is ~/go/src/playground
;(require 'go-playground)
(custom-set-variables '(go-playground-basedir (getenv "GO_PLAYGROUND_BASE_DIR")))

;;; ----- lsp-mode, but the gopls language server is slow in a little big project. NOT NOW!
;;; Use gocode and the counterparts instead
;;; lsp-mode and lsp related. Mark them here, as well as the related packages in packages.el
;(require 'lsp-mode)
;(add-hook 'go-mode-hook #'lsp-deferred)
;; company-lsp
;(require 'company-lsp)
;(push 'company-lsp company-backends)

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
;(require 'livedown)

;;; doom-modeline
;(require 'doom-modeline)
(setq doom-modeline-bar-width 0)        ; left side little bar width
(display-battery-mode)

; https://www.gonsie.com/blorg/modeline.html
(if (eq window-system nil)
    (progn
      (set-face-attribute 'mode-line nil
                          :background "black"
                          ;; :foreground "white"
                          ;; :box '(:line-width 8 :color "#505560")
                          ;; :overline nil
                          ;; :underline nil
                          )
      (set-face-attribute 'mode-line-inactive nil
                          :background "gray9"
                          ;; :foreground "#fdf6e3"
                          :foreground "#a38650"
                          :box '(:line-width 8 :color "gray9")
                          :overline nil
                          :underline nil)
      ))

(doom-modeline-mode 1)

;;; evil
(evil-mode 1)
(setq evil-move-beyond-eol t)
(setq evil-move-cursor-back nil)

;;; evil-matchit
(setq evilmi-shortcut "n")
(global-evil-matchit-mode 1)

;;; vterm
;(require 'vterm)
(global-set-key (kbd "<f6>") 'vterm)
(add-hook 'vterm-mode-hook
          '(lambda ()
             (evil-emacs-state)
             (linum-mode -1)))

;;; golden-ratio
; manually call golden-ratio-mode or golden-ratio
(global-set-key (kbd "C-x g r") 'golden-ratio)

;;; disaster
(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-map (kbd "C-x x d") 'disaster)
            (define-key c++-mode-map (kbd "C-x x d") 'disaster)))


;;; pyim
(require 'pyim)
(setq default-input-method "pyim")

;; ========== rime as first choice ==========
; use librime as input engine:
; 1. install librime: 1) using package management system or 2) manually build it
; 2. install rime-data: 1) using package management system or 2) manually install via plum
; 3. install liberime, make liberime
(push "~/.emacs.d/pyim/liberime/build/" load-path)
(require 'liberime)
;; (setq rime-data-dir "/usr/share/rime-data") ; if install rime-data via package manager, or as dependency of fcitx-rime or ibus-rime
;; (setq rime-data-dir (expand-file-name "~/.emacs.d/pyim/rime")) ; if install schema files via plum
(if (file-exists-p "/usr/share/rime-data/default.yaml")
    (setq rime-data-dir "/usr/share/rime-data")
  (setq rime-data-dir (expand-file-name "~/.emacs.d/pyim/rime")))
(liberime-start rime-data-dir (expand-file-name "~/.emacs.d/pyim/rime"))
;(liberime-get-schema-list)
(setq pyim-default-scheme 'rime)
;; (liberime-select-schema "luna_pinyin_simp") ; 使用全拼
(liberime-select-schema "double_pinyin_flypy") ; 使用小鹤双拼
;; ========== rime as first choice ==========

;; ;; ========== pyim as second choice ==========
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)
;; ;; (setq pyim-default-scheme 'quanpin) ; !!! 积累自己的频繁字词 ~/.emacs.d/pyim/dcache !!!
;; (setq pyim-default-scheme 'xiaohe-shuangpin)
;; ; --- pyim using dicts
;; ; http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz
;; (setq pyim-dicts
;;       '((:name "default_big_dict" :file "~/.emacs.d/pyim/dicts/pyim-bigdict.pyim")))
;; ;; 让 Emacs 启动时自动加载 pyim 词库
;; (add-hook 'emacs-startup-hook
;;           #'(lambda () (pyim-restart-1 t)))
;; ;; ========== pyim as second choice ==========

;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 C-x j 快捷键，强制将光标前的拼音字符串转换为中文。
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))
;; 使用 pupup-el 来绘制选词框
(setq pyim-page-tooltip 'popup)
;; 选词框显示9个候选词
(setq pyim-page-length 9)

;; bind
(global-set-key (kbd "C-x j") 'pyim-convert-string-at-point)
(global-set-key (kbd "C-\\") 'toggle-input-method)


;;; magit
(with-eval-after-load 'magit
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (define-key magit-file-mode-map (kbd "C-x g") nil))
(global-set-key (kbd "C-x g g") 'magit-status)
(global-set-key (kbd "C-x g b") 'magit-blame)

;;; x86-lookup
(setq x86-lookup-pdf "~/Books/OS/325383-sdm-vol-2abcd.pdf")
(global-set-key (kbd "C-x x l") #'x86-lookup)
(setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-evince)
;(setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-mupdf)

;;; org
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c SPC") nil)
  (define-key org-mode-map (kbd "C-c C-SPC") 'org-table-blank-field))

;;; org-bullets
(add-hook 'org-mode-hook 'org-bullets-mode)

;;; symbol-overlay
;(require 'symbol-overlay)
(add-hook 'after-init-hook 'symbol-overlay-mode)
(add-hook 'prog-mode-hook 'symbol-overlay-mode)
;; (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
;; (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
(global-set-key (kbd "C-c s i") 'symbol-overlay-put)
(global-set-key (kbd "C-c i") 'symbol-overlay-put) ; frequent key stroke
;(global-set-key (kbd "C-c s a") 'symbol-overlay-remove-all)
(global-set-key (kbd "C-c s t") 'symbol-overlay-toggle-in-scope)
(global-set-key (kbd "C-c s m") 'symbol-overlay-mode)
(global-set-key (kbd "C-c s p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "C-c s n") 'symbol-overlay-switch-forward)
(let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'symbol-overlay-put)
    (define-key map (kbd "C-h h") 'symbol-overlay-map-help)
    (define-key map (kbd "p") 'symbol-overlay-jump-prev)
    (define-key map (kbd "n") 'symbol-overlay-jump-next)
    (define-key map (kbd "<") 'symbol-overlay-jump-first)
    (define-key map (kbd ">") 'symbol-overlay-jump-last)
    (define-key map (kbd "M-w") 'symbol-overlay-save-symbol)
    (define-key map (kbd "t") 'symbol-overlay-toggle-in-scope)
    ;(define-key map (kbd "e") 'symbol-overlay-echo-mark)
    ;(define-key map (kbd "d") 'symbol-overlay-jump-to-definition)
    ;(define-key map (kbd "s") 'symbol-overlay-isearch-literally)
    ;(define-key map (kbd "q") 'symbol-overlay-query-replace)
    (define-key map (kbd "r") 'symbol-overlay-rename)
    (define-key map (kbd "a") 'symbol-overlay-remove-all)
    (setq symbol-overlay-map map))
; http://ergoemacs.org/emacs/elisp_define_face.html
; http://jonasjacek.github.io/colors/
; https://pdos.csail.mit.edu/~jinyang/rgb.html
(face-spec-set
 'symbol-overlay-default-face
 ;'((t :background "LightGoldenrod" :foreground "black" :weight bold :underline t))
 ;'((t :background "LightGoldenrod" :foreground "black"))
 '((t :background "LightGoldenrod4" :foreground "black")) ; distinguish mark activated state from symbol-overlay state
 ;'((t :background "gray28"))
   'face-defface-spec)
(face-spec-set
 'symbol-overlay-face-4
 ;'((t :background "blue violet" :foreground "black"))
 '((t :background "dark orchid" :foreground "black"))
 'face-defface-spec)

;;; vc-msg
(eval-after-load 'vc-msg-git
  '(progn
     ;; show code of commit
     (setq vc-msg-git-show-commit-function 'magit-show-commit)
     ;; open file of certain revision
     (push '("m"
             "[m]agit-find-file"
             (lambda ()
               (let* ((info vc-msg-previous-commit-info)
                      (git-dir (locate-dominating-file default-directory ".git")))
                 (magit-find-file (plist-get info :id )
                                  (concat git-dir (plist-get info :filename))))))
           vc-msg-git-extra)))
(global-set-key (kbd "C-x g s") 'vc-msg-show)

;;; smartparens
(require 'smartparens-config)
;; (add-hook 'prog-mode-hook #'smartparens-mode)
(smartparens-global-mode)
(global-set-key (kbd "C-c s r") 'sp-rewrap-sexp)
(global-set-key (kbd "C-c s d") 'sp-unwrap-sexp)
(global-set-key (kbd "C-c s f") 'sp-down-sexp)
(global-set-key (kbd "C-c s b") 'sp-backward-up-sexp)

;; (defun gniuk/create-newline-and-enter-sexp (&rest _ignored)
;;   "Open a new brace or bracket expression, with relevant newlines and indent."
;;   (newline)
;;   (indent-according-to-mode)
;;   (forward-line -1)
;;   (indent-according-to-mode))
;; (sp-local-pair 'c-mode "{" nil :post-handlers '((gniuk/create-newline-and-enter-sexp "RET")))
;; (sp-local-pair 'c++-mode "{" nil :post-handlers '((gniuk/create-newline-and-enter-sexp "RET")))

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;;; git-timemachine
(global-set-key (kbd "C-x g t") 'git-timemachine)
(add-hook 'git-timemachine-mode-hook '(lambda ()
                                        (evil-emacs-state)))

;;; eyebrowse
(eyebrowse-mode t)
(global-set-key (kbd "C-c 1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "C-c 2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "C-c 3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "C-c 4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "C-c 5") 'eyebrowse-switch-to-window-config-5)

;;; dumb-jump
;; (require 'dumb-jump)
;; (setq dumb-jump-force-searcher 'rg)  ; ripgrep is faster
;; (setq dumb-jump-prefer-searcher 'rg)
(with-eval-after-load 'dumb-jump
  (setq dumb-jump-force-searcher 'rg))  ; ripgrep with pcre2 feature needed, cargo build --release --features 'pcre2'
(global-set-key (kbd "C-c j .") 'dumb-jump-go)
(global-set-key (kbd "C-c j ,") 'dumb-jump-back)
(global-set-key (kbd "C-c j s") 'dumb-jump-go-prompt)
(global-set-key (kbd "C-c j l") 'dumb-jump-quick-look)
(global-set-key (kbd "C-c j o") 'dumb-jump-go-other-window)
(global-set-key (kbd "C-c j x") 'dumb-jump-go-prefer-external)
(global-set-key (kbd "C-c j z") 'dumb-jump-go-prefer-external-other-window)

;;; helm-xref
;; hijack the miserable raw xref
(require 'helm-xref)

;;; lsp, ccls, lsp-ui
(add-hook 'c-mode-common-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-lsp company-files company-yasnippet (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))
            (setq ccls-executable "/usr/bin/ccls")
            (require 'ccls)
            (require 'lsp-ui)
            (lsp-ui-mode)
            (lsp)
            (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
            (setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
            ;; (define-key lsp-mode-map (kbd "M-r") 'lsp-ui-peek-find-references) ; use helm interface instead.
            (define-key lsp-mode-map (kbd "C-c r c") 'ccls-call-hierarchy)
            (define-key lsp-mode-map (kbd "C-c r d") 'ccls-inheritance-hierarchy)
            (define-key lsp-mode-map (kbd "C-c r m") 'ccls-member-hierarchy)
            (setq lsp-enable-symbol-highlighting nil) ; shall not collide with symbol-overlay
            (setq lsp-prefer-flymake nil)
            (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
            (custom-set-faces
             '(lsp-ui-peek-filename     ; use helm interface instead.
               ((t (:background "dodger blue" :foreground "black"))))
             '(lsp-ui-peek-peek
               ((t (:background "#fdf6e3"))))
             )))

;;; dtrt-indent
;; guess file indentation
(require 'dtrt-indent)
(setq dtrt-indent-max-lines 2000)
(dtrt-indent-global-mode 1)

;; ;;; org-brain
;; (require 'org-brain)
;; (setq org-brain-visualize-default-choices 'all
;;       org-brain-title-max-length 24
;;       org-brain-include-file-entries nil
;;       org-brain-file-entries-use-title nil)
;; (with-eval-after-load 'org-capture
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;;           "* %i%?" :empty-lines 1)
;;         org-capture-templates))
;; (with-eval-after-load 'evil
;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))

;;; spacemacs theme and other themes
;; (load-theme 'sanityinc-tomorrow-eighties t)
;; (load-theme 'doom-vibrant t)
(setq spacemacs-theme-comment-bg nil)
(setq spacemacs-theme-comment-italic t)
(load-theme 'spacemacs-dark t)
(set-face-attribute 'font-lock-comment-face nil :foreground "#5d7878")


;;; override keybinds
; expand-region and multiple-cursor mark next
;(define-key window-numbering-keymap (kbd "M-8") nil) ; we have no eight windows in one small screen. use the precious and convenient keybinding.
;(global-set-key (kbd "M-8") 'mc/mark-next-like-this-symbol) ; M-8 and M-= can be a pair to select arbitrary symbol pattern.
;(global-set-key (kbd "M-=") 'er/expand-region) ; M-= is for emacs count-words, not used frequently, bind to expand-region. use M-x to count-words.

(define-key window-numbering-keymap (kbd "M-9") nil)
(global-set-key (kbd "M-9") 'mc/mark-next-like-this)
(define-key window-numbering-keymap (kbd "M-0") nil)
(global-set-key (kbd "M-0") 'er/expand-region)

(define-key window-numbering-keymap (kbd "M-8") nil)
(global-set-key (kbd "M-8") 'mc/edit-lines)

; override go-import-add in go-mode
(add-hook 'go-mode-hook (lambda ()
                          (define-key go-mode-map (kbd "C-c C-i") 'go-import-add)))

; override evil key bindings
; override C-e evil-scroll-line-down, make it original mwim-end-of-code-or-line
(define-key evil-motion-state-map (kbd "C-e") nil)
; rtags find definition or other jump to definition commands
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "M-.") nil)
; i to evil-emacs-state, C-z to evil-normal-state
(define-key evil-normal-state-map (kbd "i") 'evil-emacs-state)
(define-key evil-normal-state-map (kbd "C-z") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-z") 'evil-normal-state)
; C-p previous-line, C-n next-line
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-n") 'next-line)
; C-v not act as VISUAL MODE, but original emacs scroll-up-command, pair with M-v. Block editing with Emacs.
(define-key evil-normal-state-map (kbd "C-v") 'scroll-up-command)
(define-key evil-motion-state-map (kbd "C-v") 'scroll-up-command)
; C-f, C-b act as in emacs. Use only C-v, M-v to scroll page.
(define-key evil-normal-state-map (kbd "C-f") 'forward-char)
(define-key evil-normal-state-map (kbd "C-b") 'backward-char)
(define-key evil-motion-state-map (kbd "C-f") 'forward-char)
(define-key evil-motion-state-map (kbd "C-b") 'backward-char)
; C-r isearch-backward, not undo-tree-redo
(define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
; q quit, not evil-record-macro
(define-key evil-normal-state-map (kbd "q") nil)

;; keybindings for some C-M prefix commands
(global-set-key (kbd "C-c a") 'beginning-of-defun)
(global-set-key (kbd "C-c e") 'end-of-defun)
(global-set-key (kbd "C-c s k") 'kill-sexp)
(global-set-key (kbd "C-c s SPC") 'mark-sexp)

;;; other config
; automatic delete trailing spaces when saving file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; turn on documentation in elisp mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	     (turn-on-eldoc-mode)))

;; ediff
; 1. make ediff not open it's command interface in an external frame(window) under gui
; 2. make ediff split horizontally
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

; split horizontally preferred
(setq-default split-height-threshold nil
              split-width-threshold 100)

;; keep session when restart
;(desktop-save-mode)
;; desktop-save-mode is time consuming, use bookmark and the helm interface instead

;; comment current line without marking the line first
(global-set-key (kbd "C-x M-;") 'comment-line)

;; c,c++-mode tuning
;; let dtrt-indent guess the indentation of existing file, otherwise use bsd style.
(setq c-default-style "bsd")
(setq-default c-basic-offset 4)

(provide 'init-packages)
;;; init-packages.el ends here
