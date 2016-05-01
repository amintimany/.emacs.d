;; General settings
;    show line number
(global-linum-mode t)
;    disable startup message
(setq inhibit-startup-message t)
;    no new line at the end
(setq require-final-newline t)
;    delete selected text and replace it
(delete-selection-mode 1)
;    show column number
(column-number-mode 1)
;    correct behavior of home and end keys
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

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

;; Load monokai theme
(check-install-package 'monokai-theme)
(load-theme 'monokai t)

;;  fill-column-mode
(check-install-package 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 90)
(define-globalized-minor-mode global-cle-mode column-enforce-mode
  (lambda () (column-enforce-mode 1)))
(global-cle-mode 1)

;; Agda input method for math/unicode input
(load-file "~/.emacs.d/agda-input.el")
(require 'agda-input)

;; Tuareg for OCaml
(load-file "~/.emacs.d/tuareg/tuareg-site-file.el")

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

;; Enable ispell -- requires aspell to be installed.
(if (file-exists-p "/usr/local/bin/aspell")
    (setq ispell-program-name "/usr/local/bin/aspell"
	  ispell-dictionary "english")
  ()
  )
