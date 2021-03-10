;;; ccls-extras.el --- ccls extra setup here
;;; Commentary:
;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el
;;; Code:

(defun ccls/callee ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind)
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind)
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 128)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

(add-hook 'lsp-mode-hook
          (lambda ()
            (unless (not gniuk-lsp-c-cpp--enabled)
              (setq xref-prompt-for-identifier '(not xref-find-definitions
                                                     xref-find-definitions-other-window
                                                     xref-find-definitions-other-frame
                                                     xref-find-references))
              ;; (define-key c-mode-base-map (kbd "M-r") 'xref-find-references)
              (define-key c-mode-base-map (kbd "M-.")       'lsp-find-definition)
              (define-key c-mode-base-map (kbd "M-r")       'lsp-find-references)
              (define-key c-mode-base-map (kbd "C-c M-.")   'lsp-find-type-definition)
              (define-key c-mode-base-map (kbd "C-c c s")   'lsp-ui-find-workspace-symbol)
              (define-key c-mode-base-map (kbd "C-c c a")   'ccls/references-address)
              (define-key c-mode-base-map (kbd "C-c c c")   'ccls/caller)
              (define-key c-mode-base-map (kbd "C-c c C")   'ccls/callee)
              (define-key c-mode-base-map (kbd "C-c c f")   'ccls/references-not-call)
              (define-key c-mode-base-map (kbd "C-c c h")   'ccls-call-hierarchy)
              (define-key c-mode-base-map (kbd "C-c c i")   'ccls-inheritance-hierarchy)
              (define-key c-mode-base-map (kbd "C-c c l")   'ccls-reload)
              (define-key c-mode-base-map (kbd "C-c c m h") 'ccls-member-hierarchy)
              (define-key c-mode-base-map (kbd "C-c c M")   'ccls/references-macro)
              (define-key c-mode-base-map (kbd "C-c c p")   'ccls-preprocess-file)
              (define-key c-mode-base-map (kbd "C-c c r")   'ccls/references-read)
              (define-key c-mode-base-map (kbd "C-c c w")   'ccls/references-write)
              (define-key c-mode-base-map (kbd "C-c c m v") (lambda () (interactive) (ccls/member 0)))
              (define-key c-mode-base-map (kbd "C-c c m t") (lambda () (interactive) (ccls/member 2)))
              (define-key c-mode-base-map (kbd "C-c c m f") (lambda () (interactive) (ccls/member 3)))
              (define-key c-mode-base-map (kbd "C-c c D")   (lambda () (interactive) (ccls-navigate "D")))
              (define-key c-mode-base-map (kbd "C-c c L")   (lambda () (interactive) (ccls-navigate "L")))
              (define-key c-mode-base-map (kbd "C-c c R")   (lambda () (interactive) (ccls-navigate "R")))
              (define-key c-mode-base-map (kbd "C-c c U")   (lambda () (interactive) (ccls-navigate "U")))
              )))

(provide 'ccls-extras)

;;; ccls-extras.el ends here
