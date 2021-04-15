;;; init.el --- init file
;;; Commentary:
;; none

;;; Code:
;; (setq gc-cons-threshold (* 500 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)

(defvar original/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun revert-file-name-handler-alist ()
  "Revert `file-name-handler-alist to original value."
  (setq file-name-handler-alist original/file-name-handler-alist))

(add-hook 'emacs-startup-hook 'revert-file-name-handler-alist)

;; The custom set variables are put here. We check it and load it at the last.
(setq custom-file "~/.emacs.d/.emacs-custom-vars.el")

(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; M-x describe-char on a 汉字 or 标点，find the script, e.g. han or cjk-misc.
;; use the font setting menu of terminal(e.g. lxterminal) to find the right name of the font needed.
;; use fc-list to find the available fonts in the system.
;; fc-cache -v [-f] fontdir to cache new font
;; fc-list ":charset=HexCodePoint[-codepoint2]" to find which font can display this/these character[s]
;; 再来试试，可以了，这下好了，非常棒。
(if (not (eq window-system nil))
    (progn
      (defun guess-preferable-font-size ()
        (cond
         ((and (= (display-pixel-height) 1080) (>= (display-mm-height) 285)) 11) ; 1920x1080 with >=22? inches physical monitor
         ((and (= (display-pixel-height) 1440) (>= (display-mm-height) 300)) 13) ; 2560x1440 with >=23.8? inches physical monitor
         (17)                           ; my 2560x1440 laptop with 14 inches physical monitor
         ))
      (defun get-properly-sized-font (fontname)
        (concat fontname "-" (number-to-string (guess-preferable-font-size))))
      (defun get-preferable-coding-font ()
        (cond
         ((member "Source Code Pro" (font-family-list)) (get-properly-sized-font "SourceCodePro"))
         ((member "Ubuntu Mono" (font-family-list)) (get-properly-sized-font "UbuntuMono"))
         ((member "DejaVu Sans Mono" (font-family-list)) (get-properly-sized-font "DejaVuSansMono"))
         ((member "Liberation Mono" (font-family-list)) (get-properly-sized-font "LiberationMono"))))
      (defun get-preferable-cjk-sc-font ()
        (cond
         ((member "Sarasa Term SC" (font-family-list)) (get-properly-sized-font "Sarasa Mono SC")) ; don't know why the name "Sarasa Mono SC" not detected.
         ((member "Noto Sans Mono CJK SC" (font-family-list)) (get-properly-sized-font "NotoSansMonoCJKSC")))) ; Noto Sans CJK has no italic style

      (add-to-list 'default-frame-alist
                   `(font . ,(get-preferable-coding-font)))
      (set-frame-font (get-preferable-coding-font))
      (set-fontset-font                 ; for SimplifiedChinese, jiantihanzi, 简体中文。
       "fontset-default"                ; t means "fontset-default", see C-h f set-fontset-font
       'han                             ; describe-char to find the script name corresponding to SC
       (get-preferable-cjk-sc-font))
      (set-fontset-font t 'cjk-misc (get-preferable-cjk-sc-font)) ; for punctuations, 标点符号。
      ;; some infrequent [traditional] chinese characters can be found in 新細明體 mingliub.ttc
      (if (member "PMingLiU-ExtB" (font-family-list))
          (set-fontset-font t 'han "PMingLiU-ExtB" nil 'append))
      ;; for unicode emojies and symbols
      (if (member "Noto Color Emoji" (font-family-list))
          (set-fontset-font t 'symbol "NotoColorEmoji" nil 'prepend))
      (if (member "Symbola" (font-family-list))
          (set-fontset-font t 'symbol "Symbola" nil 'append))
      (if (member "JoyPixels" (font-family-list))
          (set-fontset-font t 'symbol "JoyPixels" nil 'append))
      ;; linear-a
      (if (member "Noto Sans Linear A" (font-family-list))
          (set-fontset-font t 'linear-a "NotoSansLinearA" nil 'prepend))
      ;; linear-b
      (if (member "Noto Sans Linear B" (font-family-list))
          (set-fontset-font t 'linear-b "NotoSansLinearB" nil 'prepend))
      ;; gothic
      (if (member "Noto Sans Gothic" (font-family-list))
          (set-fontset-font t 'gothic "NotoSansGothic" nil 'prepend))
      (if (member "Symbola" (font-family-list))
          (set-fontset-font t 'gothic "Symbola" nil 'append))
      ;; sutton-sign-writing
      (if (member "Noto Sans SignWriting" (font-family-list))
          (set-fontset-font t 'sutton-sign-writing "NotoSansSignWriting" nil 'prepend))
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

(cl-pushnew "~/.emacs.d/mylisp" load-path :test #'string=)
(require 'useful-single-key)

;; setup left side line number display
(if (version<= "26.0.50" emacs-version)
    (progn
      (defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
        "Major modes on which to disable the linum mode, exempts them from global requirement"
        :group 'display-line-numbers
        :type 'list
        :version "green")

      (defun display-line-numbers--turn-on ()
        "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
        (if (and
             (not (member major-mode display-line-numbers-exempt-modes))
             (not (minibufferp)))
            (display-line-numbers-mode)))

      (setq display-line-numbers-grow-only t) ; do not shrink line number width, because it cause misalign of code
      (global-display-line-numbers-mode)
      (set-face-attribute 'line-number-current-line nil
                          :background "grey" :foreground "black"))
  (global-linum-mode))
;; make display-line-numbers-mode same looking as linum-mode
;; src/xdisp.c:
;; pint2str (lnum_buf, it->lnum_width + 1, lnum_to_display); -->
;; pint2str (lnum_buf, it->lnum_width, lnum_to_display);
;; strcat (lnum_buf, " "); -->
;; // strcat (lnum_buf, " ");
;; ===
;; more performance tuning, 0.5s ↓ in my case
;; src/lread.c getc -> getc_unlocked (thread safe)

(line-number-mode -1) ;; don't show duplicate line number in modeline
(column-number-mode t) ;; show column number in modeline

(global-subword-mode)                   ;navigate camelCase word!
(show-paren-mode t)


;; (setq gc-cons-threshold (* 64 1024 1024))
;; (setq gc-cons-percentage 0.1)
;; maybe this is the right way to make emacs a little fluent
(run-with-idle-timer 5 t #'garbage-collect)

(defun my-minibuffer-setup-hook ()
  "Prohibit garbage collection in minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Restore a appropriate threshold of garbage collection."
  (setq gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Check the previously set custom variables file
(if (file-exists-p custom-file)
    (load custom-file))

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
