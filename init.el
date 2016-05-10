;; General settings
;;    Disable startup message
(setq inhibit-startup-message t)
;;    Delete selected text and replace it
(delete-selection-mode 1)
;;    Show column number
(column-number-mode 1)
;;    Correct behavior of home and end keys
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
;;    Start emacs in maximized mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;    Disable the C-z shortcut. It makes Emacs crash on OS X and
;;    I sometimes press it unintentionally
(global-unset-key (kbd "C-z"))
;;    Disable tool-bar
(tool-bar-mode -1)
;;----------------------------------------------------------------------------------------
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
;;----------------------------------------------------------------------------------------
;; Load monokai theme
(check-install-package 'monokai-theme)
(load-theme 'monokai t)
;;----------------------------------------------------------------------------------------
;; Company mode in emacs lisp
(check-install-package 'company)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
;;----------------------------------------------------------------------------------------
(check-install-package 'magit)
;;----------------------------------------------------------------------------------------
;; Show trailing white space
(defun enable-show-trailing-whitespace () (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'enable-show-trailing-whitespace)
;; define a global minor-mode for showing white spaces
(define-minor-mode show-trailing-whitespace-mode "mode for git messages"
  :lighter "")
(define-globalized-minor-mode global-show-trailing-whitespace
  show-trailing-whitespace (lambda () (enable-show-trailing-whitespace)))
;;----------------------------------------------------------------------------------------
;;  column-enforce-mode
(check-install-package 'column-enforce-mode)
(require 'column-enforce-mode)
(define-globalized-minor-mode global-column-enforce-mode column-enforce-mode
  (lambda () (column-enforce-mode 1)))
(setq column-enforce-column 90)
(add-hook 'prog-mode-hook 'column-enforce-mode)
;;----------------------------------------------------------------------------------------
;; Agda input method for math/unicode input
(load-file "~/.emacs.d/agda-input.el")
(require 'agda-input)
(defun enable-agda-input () (interactive) (set-input-method "Agda"))
;; Add an alias for the command 'enable-agda-input which is now interactive
;; This means we can use M-x agd to enable Agda input method
(defalias 'agd 'enable-agda-input)
;;----------------------------------------------------------------------------------------
;; Tuareg for OCaml
(load-file "~/.emacs.d/tuareg/tuareg-site-file.el")
;;----------------------------------------------------------------------------------------
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

(add-hook 'coq-mode-hook 'enable-agda-input)
(add-hook 'coq-mode-hook 'column-enforce-mode)

(add-hook 'coq-mode-hook 'enable-show-trailing-whitespace)

;; Load company-coq when opening Coq files
(check-install-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
;;----------------------------------------------------------------------------------------
;; Making a mode git-mode and a global-flyspell-mode.
;; We add enabling global-flyspell-mode (when aspell is available) at git-mode-hook below.
;; To enable git-mode for git commits use:
;;
;;           `git config --global core.editor "emacs -f git-mode"`
;;

(define-globalized-minor-mode global-flyspell-mode flyspell-mode
  (lambda () (flyspell-mode 1)))

(define-minor-mode git-mode "mode for git messages" :lighter " git-co" :global t)

;; Setting up the git-mode.

(defun set-column-enforce-column-for-git () (setq column-enforce-column 80))

(add-hook 'git-mode-hook 'set-column-enforce-column-for-git)
(add-hook 'git-mode-hook 'global-column-enforce-mode)
(add-hook 'git-mode-hook 'global-show-trailing-whitespace)
;;----------------------------------------------------------------------------------------
;; Set the note mode
;; This part is best extracted in another file
(define-minor-mode note-mode "mode for note files" :lighter "notes")

(add-to-list 'auto-mode-alist '("\\.note\\'" . note-mode))

(defun set-column-enforce-column-for-notes () (setq column-enforce-column 120))

(add-hook 'note-mode-hook 'set-column-enforce-column-for-notes)
(add-hook 'note-mode-hook 'global-column-enforce-mode)
(add-hook 'note-mode-hook 'enable-show-trailing-whitespace)
(add-hook 'note-mode-hook 'enable-agda-input)
;;----------------------------------------------------------------------------------------
;; Enable ispell -- requires aspell to be installed.
(defun enable_spelling ()
  (progn (setq ispell-program-name "/usr/local/bin/aspell"
	       ispell-dictionary "english")
	 (add-hook 'prog-mode-hook 'flyspell-prog-mode)
	 (add-hook 'coq-mode-hook 'flyspell-prog-mode)
	 (add-hook 'git-mode-hook 'global-flyspell-mode)
	 (add-hook 'note-mode-hook 'flyspell-mode)
	 )
  )

(if (file-exists-p "/usr/local/bin/aspell")
    (enable_spelling)
  (message "aspell was not detected and therefore not enabled!")
  )
