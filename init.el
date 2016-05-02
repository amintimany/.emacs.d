;; General settings
;    disable startup message
(setq inhibit-startup-message t)
;    delete selected text and replace it
(delete-selection-mode 1)
;    show column number
(column-number-mode 1)
;    correct behavior of home and end keys
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
;-----------------------------------------------------------------------------------------
;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;  The first time packages of melpa are not listed and so attemting to install
;;  causes an error. Use "M-x package-refresh-contents" solve this problem.

;; Make sure a package exists
;; Inspired by https://github.com/tgross/emacs-init/blob/master/init.el
(defun check-install-package (package)
 (or (package-installed-p package)
     (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
	 (package-install package))))
;-----------------------------------------------------------------------------------------

;; Load monokai theme
(check-install-package 'monokai-theme)
(load-theme 'monokai t)
;-----------------------------------------------------------------------------------------
;;  fill-column-mode
(check-install-package 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 90)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'coq-mode-hook 'column-enforce-mode)
;-----------------------------------------------------------------------------------------
;; Agda input method for math/unicode input
(load-file "~/.emacs.d/agda-input.el")
(require 'agda-input)
;-----------------------------------------------------------------------------------------
;; Tuareg for OCaml
(load-file "~/.emacs.d/tuareg/tuareg-site-file.el")
;-----------------------------------------------------------------------------------------
;; Coq and Proof General
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-double-hit-enable t)
 '(inhibit-startup-screen t)
 '(proof-three-window-mode-policy (quote hybrid))
 '(show-paren-mode t))

(defun coq-set-up () (set-input-method "Agda"))

(add-hook 'coq-mode-hook 'coq-set-up)

;; Load company-coq when opening Coq files
(check-install-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
;-----------------------------------------------------------------------------------------
;; Making a mode git-mode and a global-flyspell-mode.
;; We add enabling global-flyspell-mode (when aspell is available) at git-mode-hook below.
;; To enable git-mode for git commits use:
;;
;;           `git config --global core.editor "emacs -f git-mode"`
;;

(define-globalized-minor-mode global-flyspell-mode flyspell-mode
  (lambda () (flyspell-mode 1)))

(define-minor-mode git-mode "mode for git messages" :lighter " git" :global t)

;-----------------------------------------------------------------------------------------
;; Enable ispell -- requires aspell to be installed.
(defun enable_spelling ()
  (progn (setq ispell-program-name "/usr/local/bin/aspell"
	       ispell-dictionary "english")
	 (add-hook 'prog-mode-hook 'flyspell-prog-mode)
	 (add-hook 'coq-mode-hook 'flyspell-prog-mode)
	 (add-hook 'git-mode-hook 'global-flyspell-mode)
	 )
  )

(if (file-exists-p "/usr/local/bin/aspell")
    (enable_spelling)
  (message "aspell was not detected and therefore not enabled!")
  )
