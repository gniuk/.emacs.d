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
              (define-key ccls-tree-mode-map (kbd "n") #'next-line)
              (define-key ccls-tree-mode-map (kbd "p") #'previous-line)
              (lsp-ui-mode)
              (lsp)
              (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
              )))

(provide 'lsp-c-cpp-setup)

;;; lsp-c-cpp-setup.el ends here
