;;; init.el --- init file
;;; Commentary:
;; none

;;; Code:
(setq gc-cons-threshold (* 500 1024 1024))
(setq gc-cons-percentage 0.6)
(setq-default linum-delay t)
(global-linum-mode t)
(column-number-mode t)
(show-paren-mode t)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)

(if (not (eq window-system nil))
    (progn
      (add-to-list 'default-frame-alist
                   '(font . "SourceCodePro-17"))
      (set-frame-font "SourceCodePro-17")
      ; M-x describe-char on a 汉字 or 标点，find the script, e.g. han or cjk-misc.
      ; use the font setting menu of terminal(e.g. lxterminal) to find the right name of the font needed.
      ; 再来试试，可以了，这下好了，非常棒。
      (set-fontset-font "fontset-default" 'han "NotoSansMonoCJKSC-17") ; for jiantihanzi, 简体中文
      (set-fontset-font "fontset-default" 'cjk-misc "NotoSansMonoCJKSC-17") ; for punctuations, 标点符号
      ))

;; disable tool bar and scroll bar, in both GUI and TUI
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)
(if (boundp 'x-toolkit-scroll-bars)
    (scroll-bar-mode 0))

;; use space to indent by default
(setq-default indent-tabs-mode nil)
;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)
;(display-time-mode 1)
;(setq display-time-day-and-date 1)
;(defvar display-time-24hr-format)
;(setq display-time-24hr-format 1)

(load "~/.emacs.d/init-packages.el")

;(setq gdb-many-windows t)
;(setq gdb-show-main t)
;(flyspell-mode 1)

(push "~/.emacs.d/mylisp" load-path)
(require 'useful-single-key)

(global-subword-mode)                   ;navigate camelCase word!

(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.1)
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
