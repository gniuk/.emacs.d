;;; init.el --- init file
;;; Commentary:
;; none

;;; Code:
(setq gc-cons-threshold (* 300 1024 1024))
(global-linum-mode t)
(column-number-mode t)
;;; remote use the following theme and color settings
;(load-theme 'manoj-dark)
;(set-background-color "gray25")
(show-paren-mode t)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; (add-to-list 'default-frame-alist
;;              '(font . "LiberationMono-14"))
(add-to-list 'default-frame-alist
             '(font . "SourceCodePro-17"))
;(set-frame-font "Monospace-14")
;(set-frame-font "Inconsolata-g-14")
;(set-frame-font "SourceCodePro-14")
;(set-frame-font "LiberationMono-14")
;(set-default-font "Inconsolata-14")
;(set-default-font "Monospace-14")
;(tool-bar-mode 0)
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)
(if (boundp 'x-toolkit-scroll-bars)
    (scroll-bar-mode 0))
;(scroll-bar-mode 0)
(setq-default indent-tabs-mode nil)
;(setq kill-whole-line t)
;(display-time-mode 1)
;(setq display-time-day-and-date 1)
;(defvar display-time-24hr-format)
;(setq display-time-24hr-format 1)

;built-in
;(require 'cc-mode)
;(require 'semantic)
;(require 'semantic/db)
;(global-semanticdb-minor-mode 1)
;(global-semantic-idle-scheduler-mode 1)
;(semantic-mode 1)

;(global-set-key (kbd "C-c C-d") 'kill-whole-line)
;(global-set-key (kbd "<C-backspace>") 'kill-whole-line) ;kill whole line at any
                                        ;position, like vim dd

; Enable EDE (Project Management) features
(global-ede-mode 1)
;(semantic-load-enable-excessive-code-helpers)      ; Enable prototype help and
                                        ;smart completion

;;(global-srecode-minor-mode 1)            ; Enable template insertion menu
;; Semantic
;(global-semantic-idle-scheduler-mode)
;(global-semantic-idle-completions-mode)
;(global-semantic-decoration-mode)
;(global-semantic-highlight-func-mode)
;(global-semantic-show-unmatched-syntax-mode)
; CC-mode
;(add-hook 'c-mode-common-hook '(lambda ()
;        (setq ac-sources (append '(ac-source-semantic) ac-sources))
;))


(load "~/.emacs.d/init-packages.el")

(setq gdb-many-windows t)
(setq gdb-show-main t)
;(flyspell-mode 1)

;;; global set keys
;(global-set-key (kbd "<C-backspace>") 'kill-whole-line) ;kill whole line at any
;position, like vim dd
;(global-set-key (kbd "M-g") 'goto-line)

(push "~/.emacs.d/mylisp" load-path)
(require 'useful-single-key)

(global-subword-mode)                   ;navigate camelCase word!
;(load-theme 'sanityinc-solarized-light t)
;(load-theme 'sanityinc-tomorrow-eighties t)
(load-theme 'spacemacs-dark t)

(setq gc-cons-threshold (* 2 1024 1024))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some skills                                                               ;;
;; paredit                                                                    ;;
; C-) - “Slurp” the next expression into this expression                      ;;
; C-( - “Slurp” the previous expression into this expression                  ;;
; C-} - “Barf” the current expression out to the right of its parent          ;;
; expression                                                                  ;;
; C-{ - “Barf” the current expression out to the left of its parent           ;;
; expression                                                                  ;;
; M-s - delete single paren                                                   ;;
                                                                              ;;
;; undo-tree                                                                  ;;
; Now you can use "C-c z" for undo and "C-c Z" for redo.                      ;;
; Even better though is using C-x u to run undo-tree-visualize,               ;;
; which opens a second buffer displaying a tree view of your undo history in a;;
; buffer.                                                                     ;;
; You can navigate this with the arrow keys and watch the main buffer change  ;;
; through its previous                                                        ;;
; states, and hit q to exit when you have the buffer the way you wanted it, or;;
; C-q to quit without                                                         ;;
; making any changes.                                                         ;;
                                                                              ;;
;; dired                                                                      ;;
; M-x dired  or C-x d  goto dired mode                                        ;;
; C-h m to see help                                                           ;;
; C-u s to sort by ls options                                                 ;;
                                                                              ;;
;; others                                                                     ;;
; C-h k f m to get the most useful helps                                      ;;
; C-q to insert raw char                                                      ;;
; M-SPC - just-one-space                                                      ;;
; M-\ - delete-horizontal-space                                               ;;
; M-m - back-to-indentation, ie. the first none space char.                   ;;
; M-S-^ join this line to previous.                                           ;;
; C-M-o at line beginning to open new line above and jump to it.              ;;
; C-M-\ indent region, so nice.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here
