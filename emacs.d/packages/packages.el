;;; use-package example:
; (use-package foo
; :init ; Runs before loading the package. WIll always run, even if foo isn't on this system.
; :config ; Runs after.
; :bind (("M-s O" . action)
;       ("" . some-other-action))
; :commands foo-mode ; Creates autoloads for commands: defers loading until called.
; )

;; Package installation
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Keep track of whether or not we need to refresh package contents
(setq packages-installed-this-session 0)

;; Function to ensure every package in installed, and ask if it isn't.
(defun ensure-package-installed (&rest packages)
  (mapcar
   (lambda (package)
	 (if (package-installed-p package)
	     nil
	   (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	       ;; If this is the 1st install this session, update before install
	       (cond ((eq packages-installed-this-session 0)
		      (package-refresh-contents)
		      (setq packages-installed-this-session 1)
		      (package-install package))
		     (t (package-install package))
		 nil)
	     package)))
   packages))

;; List of packages to install on all systems
(setq required-packages
      '(
	iedit
	magit
	evil-magit
	magithub
	;undo-tree
	evil
	evil-leader
	powerline-evil
	monokai-theme
	challenger-deep-theme
	linum-relative
	multi-term
	neotree
	evil-numbers
	editorconfig
	company
	ivy
	flx
	flycheck
	flycheck-pos-tip
	evil-surround
	diminish
	dtrt-indent
	undohist))

;; List of optional packages
(setq optional-packages
      '(
	flymd
	markdown-mode
	latex-preview-pane
	tide
	web-mode
	racket-mode
	fuzzy
	general))


;; Check that all packages are installed
(apply 'ensure-package-installed required-packages)

;; Declare function for optional packages
(defun optional-packages-install ()
    (interactive)
  (apply 'ensure-package-installed optional-packages))


;; Activate installed packages
(package-initialize)

(require 'diminish)
(diminish 'visual-line-mode)
(diminish 'abbrev-mode)


(use-package autorevert
  :diminish auto-revert-mode)


(use-package recentf
  :config 
  (recentf-mode 1)
  (setq recentf-max-saved-items 200
    recentf-max-menu-items 15))


(use-package evil
  :config
  (evil-mode t)
  (setq evil-want-C-i-jump nil)
  (setq evil-default-state 'normal)

  ;; Move all elements of evil-emacs-state-modes to evil-motion-state-modes
  (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
  (setq evil-emacs-state-modes (list 'magit-popup-mode))
  (delete 'magit-popup-mode evil-motion-state-modes)

  ;; Delete info bindings for evil to take over
  (define-key Info-mode-map "g" nil)
  (define-key Info-mode-map "n" nil)
  (define-key Info-mode-map "p" nil)

  ;; Vim removing of windows
  (define-key evil-window-map (kbd "q") 'delete-window)
  (define-key evil-window-map (kbd "C-q") 'delete-window)

                                          ; Don't echo evil's states
  (setq evil-insert-state-message nil)
  (setq evil-visual-state-message nil)

  ;; eval the last sexp while in normal mode (include the character the cursor is currently on)
  (defun evil-eval-last-sexp ()
    (interactive)
    (evil-append 1)
    (eval-last-sexp nil)
    (evil-normal-state))

  ;; "pull" left and right with zs and ze
  (defun hscroll-cursor-left ()
    (interactive "@")
    (set-window-hscroll (selected-window) (current-column)))

  (defun hscroll-cursor-right ()
    (interactive "@")
    (set-window-hscroll (selected-window) (- (current-column) (window-width) -1)))

   ;; Horizontal scrolling
   (setq auto-hscroll-mode 't)
   (setq hscroll-margin 0
         hscroll-step 1)

  :bind (:map evil-normal-state-map
         ("zs" . hscroll-cursor-left)
         ("ze" . hscroll-cursor-right)
         ("[s" . flyspell-goto-previous-error)
         ("]s" . flyspell-goto-next-error)
         ("\C-x \C-e" . evil-eval-last-sexp)
         :map Info-mode-map
         ("g" . nil)
         ("n" . nil)
         ("p" . nil)
         :map evil-window-map
         ("q" . delete-window)
         ("C-q" . delete-window)))



(use-package evil-numbers
  ;; Increment and decrement (evil-numbers)
  :bind (("C-c C-a" . evil-numbers/inc-at-pt)
	 ("C-c C-d" . evil-numbers/dec-at-pt)))


(use-package undo-tree
  :diminish undo-tree-mode)


(use-package undohist
  :config
  ;; ;; Save undo history under .emacs.d/undohist
  (setq undohist-directory "~/.emacs.d/undohist")
  (unless (file-exists-p  "~/.emacs.d/undohist")
    (make-directory "~/.emacs.d/undohist"))

  (undohist-initialize))


(require 'init-powerline)


(use-package web-mode
  :config
  ;; 2 spaces for an indent
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; Auto-enable web-mode when opening relevent files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode)))


(use-package linum-relative
  :diminish linum-relative-mode
  :config
  (setq linum-relative-current-symbol "")
  (linum-mode)
  (linum-relative-global-mode)
  (defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))
  (advice-add #'linum-update-window :after #'linum-update-window-scale-fix))


(use-package flymd
  :config
  (setq flymd-close-buffer-delete-temp-files t))


;; Evil leader is Space
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
   "d" 'diff-buffer-with-file
   ;"b" 'buffer-menu
   "v" 'ivy-switch-buffer
   "b" 'buffer-menu
   ;"f" '(lambda ()  (interactive) (dired '"./"))
   "f" 'neotree-toggle
   "u" 'undo-tree-visualize
   "m" 'recentf-open-files
   "l" 'auto-fill-mode
   "s" '(lambda ()
  	(interactive)
  	;; use flyspell-mode when in text buffers, otherwise use flyspell-prog-mode
  	(let* ((current-mode
  	       (buffer-local-value 'major-mode (current-buffer)))
  	      (flyspell-mode-to-call
  	       (if (or (string= current-mode "text-mode") (string= current-mode "markdown-mode"))
  		   'flyspell-mode
  		 'flyspell-prog-mode)))
  	  ;; toggle the current flyspell mode, and eval the buffer if we turned it on
  	  (if flyspell-mode
  	      (funcall 'flyspell-mode '0)
  	    (funcall flyspell-mode-to-call)
  	    (flyspell-buffer))))
   ;"a" 'auto-complete-mode
   "a" 'company-mode
   "g" 'magit-status
   "M-g" 'magit-dispatch-popup
   "c" 'flycheck-mode
   ))

(if (not (eq system-type 'windows-nt))
    (lambda ()
      ((use-package magit
	 :diminish magit-auto-revert-mode
	 :config
	 (setq evil-magit-state 'normal))

       (use-package evil-magit
	 :config
         (evil-magit-init))

       (use-package magithub
         :config
         (magithub-feature-autoinject t)))))


(use-package neotree
  :config
  ; Set vi-like bindings in neotree-mode that don't conflict with evil
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)

  ;Every time when the neotree window is opened, let it find current file and jump to node.
  (setq neo-smart-open t)

  ; List of files to hide
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.class")))


;; tide/typescript

(setq typescript-indent-level 2)

;; JavaScript
(setq js-indent-level 2)


(use-package racket-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))

  ; C-w prefix in racket-REPL
  (add-hook 'racket-repl-mode-hook 'racket-repl-evil-hook)

  (defun racket-repl-evil-hook ()
    (define-key racket-repl-mode-map "\C-w" 'evil-window-map)
    (global-set-key (kbd "C-w") 'racket-repl-mode-map)))


(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))


(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode))


(use-package flx
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))


(use-package company
  :diminish company-mode)


(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
  (setq flycheck-standard-error-navigation nil)

  (global-flycheck-mode t))


(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :config 
  (dtrt-indent-mode 1))


(use-package org)

(provide 'packages)
