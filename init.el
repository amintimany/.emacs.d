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
;;    Disable the C-z shortcut. It makes Emacs crash on OS X and
;;    I sometimes press it unintentionally
(global-unset-key (kbd "C-z"))
;;    Disable the C-v shortcut. For obvious reasons.
(global-unset-key (kbd "C-v"))
;;    Disable tool-bar
(tool-bar-mode -1)
;;-----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b")
     ("^html?$" "." "netscape %o"))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(compilation-message-face (quote default))
 '(coq-double-hit-enable t)
 '(coq-script-indent nil)
 '(custom-safe-themes
   (quote
    ("3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" default)))
 '(fci-rule-color "#49483E")
 '(global-auto-revert-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (sublime-themes paganini-theme moe-theme ample-theme markdown-mode ## monokai-theme magit fill-column-indicator company-coq column-enforce-mode auctex)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(preview-TeX-style-dir "/Users/amin/.emacs.d/elpa/auctex-11.89.7/latex" t)
 '(proof-three-window-mode-policy (quote hybrid))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (let
	       ((coq-root-directory
		 (when buffer-file-name
		   (locate-dominating-file buffer-file-name ".dir-locals.el")))
		(coq-project-find-file
		 (and
		  (boundp
		   (quote coq-project-find-file))
		  coq-project-find-file)))
	     (set
	      (make-local-variable
	       (quote tags-file-name))
	      (concat coq-root-directory "TAGS"))
	     (setq camldebug-command-name
		   (concat coq-root-directory "dev/ocamldebug-coq"))
	     (unless coq-project-find-file
	       (set
		(make-local-variable
		 (quote compile-command))
		(concat "make -C " coq-root-directory))
	       (set
		(make-local-variable
		 (quote compilation-search-path))
		(cons coq-root-directory nil)))
	     (when coq-project-find-file
	       (setq default-directory coq-root-directory)))))))
 '(show-paren-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
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
(check-install-package 'sublime-themes)
(load-theme 'junio t)
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
;; ido mode
(require 'ido)
(ido-mode t)
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
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
;;------------------------------------------------------------------------------
;; Agda input method for math/unicode input
(load-file "~/.emacs.d/agda-input.el")
(require 'agda-input)
(defun enable-agda-input () (interactive) (set-input-method "Agda"))
;; Add an alias for the command 'enable-agda-input which is now interactive
;; This means we can use M-x agd to enable Agda input method
(defalias 'agd 'enable-agda-input)
;;------------------------------------------------------------------------------
;; Setting up the path and exec-path

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/:/usr/local/bin:/Users/amin/.cabal/bin:/Users/amin/.opam/4.02.1/bin/"))
(setq exec-path (append exec-path '("/Users/amin/.opam/4.02.1/bin/")))
(setq exec-path (append exec-path '("/Users/amin/.cabal/bin")))
;;------------------------------------------------------------------------------
;; Add opam emacs directory to the load-path
(setq opam-share
  (substring
    (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;;------------------------------------------------------------------------------
;; set up for OCaml using Opam packages and more

;; The ocp-index and ocp-indent programs installed from Opam
(require 'ocp-indent)
(require 'ocp-index)

;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;; enable company for OCaml
(add-hook 'tuareg-mode-hook 'company-mode t)
(add-hook 'caml-mode-hook 'company-mode t)

;; company-ocp-index (submodule)
(load-file "~/.emacs.d/company-ocp-index/company-ocp-index.el")
(require 'company-ocp-index)
(defun enable-company-ocp-index ()
(add-to-list (make-local-variable 'company-backends) 'company-ocp-index))
(add-hook 'tuareg-mode-hook 'enable-company-ocp-index)
(add-hook 'caml-mode-hook 'enable-company-ocp-index)

;; Tuareg for OCaml
(load-file "~/.emacs.d/tuareg/tuareg-site-file.el")
;;------------------------------------------------------------------------------
;; Coq and Proof General
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")

(add-hook 'coq-mode-hook 'enable-agda-input)
(add-hook 'coq-mode-hook 'column-enforce-mode)

(add-hook 'coq-mode-hook 'enable-show-trailing-whitespace)

;; Load company-coq when opening Coq files
(check-install-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;------------------------------------------------------------------------------
;; Disable indention

(electric-indent-mode -1)
;; (defun do-nothing () )
;; (define-key global-map "\t" 'do-nothing)

;; agda-mode

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

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
;; Force emacs ask yes no question
(setq confirm-kill-emacs 'y-or-n-p)

;;------------------------------------------------------------------------------
;; Set up tex mode

;;------------------------------------------------------------------------------
;; Run emacs server
(require 'server)
(if (server-running-p) () (server-start))
