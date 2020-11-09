;;; lsp-c-cpp-setup.el --- lsp c/c++ mode stuff setup
;;; Commentary:
;;; Code:

(defvar gniuk-lsp-c-cpp--enabled t)

(defun gniuk/enable-lsp-c-cpp ()
  "Enable lsp."
  (interactive)
  (setq gniuk-lsp-c-cpp--enabled t))

(defun gniuk/disable-lsp-c-cpp ()
  "Disable lsp."
  (interactive)
  (setq gniuk-lsp-c-cpp--enabled nil))

(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (not gniuk-lsp-c-cpp--enabled)
              (set (make-local-variable 'company-backends)
                   '(company-capf company-files company-yasnippet (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))
              (setq ccls-executable "/usr/bin/ccls")
              (require 'ccls)
              (require 'lsp-ui)
              (lsp-ui-mode)
              (lsp)
              (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
              (setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
              (setq lsp-ui-doc-use-childframe nil)    ; don't popup child frame in gui
              ;; (define-key lsp-mode-map (kbd "M-r") 'lsp-ui-peek-find-references) ; use helm interface instead.
              (setq lsp-enable-symbol-highlighting nil) ; shall not collide with symbol-overlay
              (setq lsp-prefer-flymake nil)
              (setq lsp-file-watch-threshold nil) ; don't bother me
              (setq read-process-output-max (* 4 1024 1024)) ;; 4mb
              (setq lsp-idle-delay 0.500)
              ;(setq lsp-completion-provider :capf)
              (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
              (custom-set-faces
               '(lsp-ui-peek-filename     ; use helm interface instead.
                 ((t (:background "dodger blue" :foreground "black"))))
               '(lsp-ui-peek-peek
                 ((t (:background "#fdf6e3"))))
               ))))

(provide 'lsp-c-cpp-setup)

;;; lsp-c-cpp-setup.el ends here
