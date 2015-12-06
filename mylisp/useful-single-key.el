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
  (move-end-of-line 1)
  (newline-and-indent))

; like Vim o command, fast open a new line and goto it no matter where you are
; in current line.
(global-set-key (kbd "C-o") 'gniuk/insert-line-below)

(provide 'useful-single-key)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:

;;; useful-single-key.el ends here
