;;  General settings
;;-----------------------------------------------------------------------------
;;  Disable startup message
(setq inhibit-startup-message t)
;;  Delete selected text and replace it
(delete-selection-mode 1)
;;  Show column number
(column-number-mode 1)
;;  Correct behavior of home and end keys
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
;;  Disable the C-z shortcut. It makes Emacs crash on OS X and
;;  I sometimes press it unintentionally
(global-unset-key (kbd "C-z"))
;;  Disable tool-bar
(tool-bar-mode -1)
;;  Disable scroll-bar
(toggle-scroll-bar -1) 
;;  Show matching parenthesis
(show-paren-mode 't)
;;  Disable the annoying bell function
(setq ring-bell-function 'ignore)
;; Set locale
(setenv "LANG" "en_US.UTF-8")
;; Increase the default font size
(set-face-attribute 'default nil :height 200)
;;------------------------------------------------------------------------------
;; Force emacs ask yes no question when exiting
(setq confirm-kill-emacs 'y-or-n-p)
;;-----------------------------------------------------------------------------
;; Custom require that can fail without breaking
(defun safe_require (req reqs)
  (if (equal (require req 'nil 't) req)
      nil
    (message "%s not found!" reqs)
    )
  )

;;------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------
;; Load the theme
;; (check-install-package 'monokai-theme)
(check-install-package 'dracula-theme)
(load-theme 'dracula t)
;;------------------------------------------------------------------------------
;; Make sure that AucTeX package is installed
(check-install-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-method (quote synctex))
(setq TeX-source-correlate-mode t)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b")))


;;------------------------------------------------------------------------------
;; Load the markdown-mode
(check-install-package 'markdown-mode)
;;------------------------------------------------------------------------------
;; Company mode in emacs lisp
(check-install-package 'company)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
;;------------------------------------------------------------------------------
;; magit -- the git mode for emacs; I am experimenting with it from time to time
(check-install-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
;;------------------------------------------------------------------------------
;; Show trailing white space
(defun enable-show-trailing-whitespace () (setq show-trailing-whitespace t))
;; (add-hook 'prog-mode-hook 'enable-show-trailing-whitespace)
;; define a global minor-mode for showing white spaces
(define-minor-mode show-trailing-whitespace-mode "mode for git messages"
  :lighter "")
(define-globalized-minor-mode global-show-trailing-whitespace
  show-trailing-whitespace (lambda () (enable-show-trailing-whitespace)))
;;------------------------------------------------------------------------------
;;  column-enforce-mode
(check-install-package 'column-enforce-mode)
(require 'column-enforce-mode)
(define-globalized-minor-mode global-column-enforce-mode column-enforce-mode
  (lambda () (column-enforce-mode 1)))
(setq column-enforce-column 80)
;;------------------------------------------------------------------------------
;;  git-gutter-mode
(check-install-package 'git-gutter)
(global-git-gutter-mode +1)
;;------------------------------------------------------------------------------
;;  sml-modeline-mode
(check-install-package 'sml-modeline)
(sml-modeline-mode t)
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
;;------------------------------------------------------------------------------
;; Agda input method for math/unicode input
(load-file "~/.emacs.d/agda-input.el")
(require 'agda-input)
(defun enable-agda-input () (interactive) (set-input-method "Agda"))
;;------------------------------------------------------------------------------
;;  multiple-cursors-mode
(check-install-package 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;------------------------------------------------------------------------------
;; Setting up the path and exec-path

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/:/usr/local/bin:/Users/amin/.cabal/bin"))
(setq exec-path (append exec-path '("/Users/amin/.cabal/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
;;------------------------------------------------------------------------------
;; Detect if opam is installed

(defvar opampresent 'nil "Whether emacs is presnt or not.")

(if (executable-find "opam") (setq opampresent 't) (message "Opam not found! Opam will not be set up."))

;; set up opam if installed

(if opampresent
    (progn
      ;; Add opam emacs directory to the load-path
      (setq opam-share
	    (substring
	     (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
      (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
      ;; Add opam bin directory to path and exec-path
      (setq opam-bin
	    (substring
	     (shell-command-to-string "opam config var bin 2> /dev/null") 0 -1))
      (setenv "PATH" (concat (getenv "PATH") (concat ":" opam-bin)))
      (setq exec-path (append exec-path (cons opam-bin nil)))
      )
  ()
  )

;;------------------------------------------------------------------------------
;; ivy and counsel
(check-install-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h u") 'counsel-unicode-char)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(setq counsel-find-file-ignore-regexp "\\.vo\\|\\.aux\\|\\.glob\\|.DS_STORE")

;; Coq and Proof General
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")

(add-hook 'coq-mode-hook 'enable-agda-input)
(add-hook 'coq-mode-hook 'column-enforce-mode)

(add-hook 'coq-mode-hook 'enable-show-trailing-whitespace)

(setq coq-double-hit-enable t)

;; Load company-coq when opening Coq files
(check-install-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
;; Disable symbol prettification
(setq company-coq-disabled-features '(prettify-symbols))
;;------------------------------------------------------------------------------
;; Making a mode git-mode and a global-flyspell-mode.
;; We add enabling global-flyspell-mode (when aspell is available) at
;; git-mode-hook below. To enable git-mode for git commits use:
;;
;;           `git config --global core.editor "emacs -f git-mode"`
;;

(define-globalized-minor-mode global-flyspell-mode flyspell-mode
  (lambda () (flyspell-mode 1)))

(define-minor-mode git-mode "mode for git messages" :lighter " git-co"
  :global t)

;; Setting up the git-mode.

(defun set-column-enforce-column-for-git () (setq column-enforce-column 80))
(defun disable-ask-before-exit () (setq confirm-kill-emacs nil))

(add-hook 'git-mode-hook 'set-column-enforce-column-for-git)
(add-hook 'git-mode-hook 'global-column-enforce-mode)
(add-hook 'git-mode-hook 'global-show-trailing-whitespace)
(add-hook 'git-mode-hook 'disable-ask-before-exit)
;;------------------------------------------------------------------------------
;; Set the note mode
;; This part is best extracted in another file
(define-minor-mode note-mode "mode for note files" :lighter "notes")

(add-to-list 'auto-mode-alist '("\\.note\\'" . note-mode))

(defun set-column-enforce-column-for-notes () (setq column-enforce-column 120))

(add-hook 'note-mode-hook 'set-column-enforce-column-for-notes)
(add-hook 'note-mode-hook 'global-column-enforce-mode)
(add-hook 'note-mode-hook 'enable-show-trailing-whitespace)
(add-hook 'note-mode-hook 'enable-agda-input)
;;------------------------------------------------------------------------------
;; Enable ispell -- requires aspell to be installed.
(defun enable_spelling ()
  (progn (setq ispell-program-name "/usr/local/bin/aspell"
	       ispell-dictionary "english")
	 (add-hook 'prog-mode-hook 'flyspell-prog-mode)
	 (add-hook 'coq-mode-hook 'flyspell-prog-mode)
	 (add-hook 'git-mode-hook 'global-flyspell-mode)
	 (add-hook 'note-mode-hook 'flyspell-mode)
         (add-hook 'text-mode-hook 'flyspell-mode)
	 )
  )

(if (file-exists-p "/usr/local/bin/aspell")
    (enable_spelling)
  (message "aspell was not detected and therefore not enabled!")
  )
;; If aspell is leaded, map mouse clicks. We can't (easily) do a mouse-2 without
;; an actual mouse!
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;;------------------------------------------------------------------------------
;; Disable indention

(electric-indent-mode -1)
;; (defun do-nothing () )
;; (define-key global-map "\t" 'do-nothing)

;;------------------------------------------------------------------------------
;; agda-mode

;; Detect if agda-mode is installed

(defvar agdamodepresent 'nil "Whether agda-mode is presnt or not.")

(if (executable-find "agda-mode") (setq agdamodepresent 't) (message "agda-mode not found! agda-mode Will not be set up."))

;; set up opam if installed

(if agdamodepresent
    (load-file (let ((coding-system-for-read 'utf-8))
		 (shell-command-to-string "agda-mode locate")))
  ()
  )

;;------------------------------------------------------------------------------
;; Bind C-c C-- to toggling comments.

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )

(global-set-key (kbd "C-c C--") 'comment-or-uncomment-line-or-region)

;;------------------------------------------------------------------------------
;; configure org mode
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;;------------------------------------------------------------------------------
;; Run emacs server
(require 'server)
(if (server-running-p) () (server-start))
