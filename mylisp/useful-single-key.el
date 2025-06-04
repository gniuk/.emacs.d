;;; useful-single-key.el --- customized multi key strokes into one

;; copyleft (C) 2015 gniuk

;; Author: gniuk <isgniuk@gmail.com>
;; Keywords: useful key strokes
;; URL:
;; Version: DEV

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;; add this to init file.
;; (push "~/.emacs.d/mylisp" load-path)
;; (require 'useful-single-key)

;;; Code:

(defun gniuk/insert-line-below ()
  "Insert line below current line and goto it."
  (interactive)
  (evil-emacs-state nil)
  (move-end-of-line 1)
  (newline-and-indent))

; like Vim o command, fast open a new line and goto it no matter where you are
; in current line.
(global-set-key (kbd "C-o") 'gniuk/insert-line-below)

;; clear eshell buffer, by type clear and execute
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


(defun gniuk/dd ()
  "Like vim dd, but no prefix blank chars or tailing newline char.
No more indentation adjustment after paste to the destination point."
  (interactive)
  (back-to-indentation)
  (kill-line)
  (delete-blank-lines))
;(global-set-key (kbd "C-c C-d") 'gniuk/dd)

(defun gniuk/cpAboveLine ()
  "Copy above line at cursor position."
  (interactive)
  (let ((p1 nil) (p2 nil))
    (forward-line -1)
    (move-beginning-of-line 1)
    (setq p1 (point))
    (move-end-of-line 1)
    (setq p2 (point))
    (kill-ring-save p1 p2)
    (forward-line)
    (yank)
    (delete-horizontal-space)))
(defun gniuk/cpAndCommentOutAboveLine ()
  "Copy above line at cursor position and comment out the previous one."
  (interactive)
  (let ((p1 nil) (p2 nil))
    (forward-line -1)
    (move-beginning-of-line 1)
    (setq p1 (point))
    (move-end-of-line 1)
    (setq p2 (point))
    (kill-ring-save p1 p2)
    (comment-line 1)
    (beginning-of-line 1)
    (yank)))
(global-set-key (kbd "C-x x c") 'gniuk/cpAboveLine)
(global-set-key (kbd "C-x x ;") 'gniuk/cpAndCommentOutAboveLine)

(defun gniuk/copyLine (arg)
  "Copy lines (as many as prefix ARG) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key (kbd "C-x x L") 'gniuk/copyLine)

;; overload highlight-symbol M-n and M-p, to quickly navigate pairs
; pairs: (),[],{},<>
(defun gniuk/goto-pair-forward ()
  "Goto pair forward."
  (interactive)
  (if (looking-at "(\\|\\[\\|{\\|<\\|\"")
      (forward-sexp)
    ;(highlight-symbol-next)))
    (symbol-overlay-jump-next)))
(global-set-key (kbd "M-n") 'gniuk/goto-pair-forward)

(defun gniuk/goto-pair-backward ()
  "Goto pair backward."
  (interactive)
  (backward-char)
  (if (looking-at ")\\|\\]\\|}\\|>\\|\"")
      (progn (forward-char)
             (backward-sexp))
    (progn (forward-char)
           ;(highlight-symbol-prev))))
           (symbol-overlay-jump-prev))))
(global-set-key (kbd "M-p") 'gniuk/goto-pair-backward)


(defun gniuk/backward-kill-word ()
  "Kill space splitted WORD backward.
Stop at beginning of line, then on next call deletes content from previous line."
  (interactive)
  (let ((cur (point))
        (bol (pos-bol)))
    ;; If at beginning of line and not at buffer beginning, move to previous line end
    (if (and (eq cur bol) (> cur (point-min)))
        (progn
          (backward-char)
          (delete-horizontal-space 1))
      ;; Otherwise proceed with normal backward deletion
      (delete-horizontal-space 1)
      (let* ((new-cur (point))
             (bol (pos-bol))
             (spc (search-backward " " bol t 1)))
        (goto-char new-cur)
        (if (or (eq spc nil) (<= spc bol))
            (gniuk/kill-back-to-indentation)
          (zap-up-to-char -1 ?\s))))))

;; make C-w a bit like when it is in bash command line
(defun gniuk/kill-region-or-kill-backward-to-whitespace ()
  "Kill region when there is a region marked, otherwise kill backward to whitespace."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    ;(zap-up-to-char -1 ?\s)))
    (gniuk/backward-kill-word)))
(global-set-key (kbd "C-w") 'gniuk/kill-region-or-kill-backward-to-whitespace)

;; make C-backspace kill back to indentation, same as d^ in vim.
(defun gniuk/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line.  A bit like vim d^."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(global-set-key (kbd "C-<backspace>") 'gniuk/kill-back-to-indentation)


(defun gniuk/hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(global-set-key (kbd "C-c h r") 'gniuk/hide-ctrl-M)


;; what if I need these functions to unfill paragraphs and regions
;; Source:
;;        http://ergoemacs.org/emacs/emacs_unfill-paragraph.html
(defun gniuk/unfill-paragraph ()
    "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (fill-paragraph)))

(defun gniuk/unfill-region (start end)
    "Replace newline chars in region START END by single spaces.
This command does the reverse of `fill-region'."
    (interactive "r")
    (let ((fill-column most-positive-fixnum))
      (fill-region start end)))

(defun gniuk/restore-window-layout-config ()
  "Restore the frequently used window config. A bit like zoom-the-active-pane in tmux when restore the panes."
  (interactive)
  (jump-to-register 1))
(global-set-key (kbd "C-c z") 'gniuk/restore-window-layout-config)

(defun gniuk/indent-region-line-by-line-without-format (start end)
  "Indent region START END without format, not like `indent-region'."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (copy-marker end))
    (goto-char start)
    (beginning-of-line)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (indent-according-to-mode))
      (forward-line 1)))
  (setq deactivate-mark t))
(global-set-key (kbd "C-x g TAB") 'gniuk/indent-region-line-by-line-without-format)

(defun show-full-font-family-list ()
  "Display the full list of font families in a new buffer."
  (interactive)
  (let ((font-families (font-family-list)))
    (with-current-buffer (get-buffer-create "*Font Families*")
      (erase-buffer)
      (insert (mapconcat #'identity font-families "\n"))
      (goto-char (point-min)))
    (display-buffer "*Font Families*")))

;; Define the global variable for target column
(defvar gniuk-insert-target-column 32
  "Default target column for `gniuk-move-to-target-column'.")

(defun gniuk-set-target-column (column)
  "Set the value of `gniuk-insert-target-column' to COLUMN.
COLUMN should be a positive integer."
  (interactive "nTarget column: ")
  (setq gniuk-insert-target-column column)
  (message "Target column set to %d" gniuk-insert-target-column))

(defun gniuk-move-to-target-column (&optional column)
  "Move to the target column specified by COLUMN.
If COLUMN is not provided, use the value of `gniuk-insert-target-column'.
If the current line is shorter than the target column,
spaces will be inserted to reach the target column."
  (interactive (list (when current-prefix-arg
                       (prefix-numeric-value current-prefix-arg))))
  (let ((target (or column gniuk-insert-target-column)))
    ;; First move to the end of the line
    (end-of-line)
    ;; Calculate current column position
    (let ((current-col (current-column)))
      (if (>= current-col target)
          ;; If the line already extends past the target column, just move to it
          (move-to-column target)
        ;; Otherwise insert spaces to reach the target column from end of line
        (insert (make-string (- target current-col) ?\s))))))
(global-set-key (kbd "C-x g i") 'gniuk-move-to-target-column)

(provide 'useful-single-key)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:

;;; useful-single-key.el ends here
