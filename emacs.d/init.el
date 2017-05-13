;; TODO:
;; Easier undo tree traversal
;; Replace evil-leader with general

;;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message ""	; I like things empty.
      initial-major-mode 'text-mode)	; I'm usually not writing elisp.

;; Base

(setq ring-bell-function 'ignore) ; Disable beep & flash
(blink-cursor-mode 0)

;; No scroll bar
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable toolbar
(when (display-graphic-p)
  (tool-bar-mode -1))

;; smoother scrolling
(setq scroll-margin 0
scroll-conservatively 9999
scroll-step 1)


;; Line settings and indicators
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)

;; All yes or no prompts are y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Never follow symlinks
(setq vc-follow-symlinks nil)

;;; Leave the OS clipboard alone (use evil's "+ and "* instead)
; Don't copy and paste to the clipboard
(setq select-enable-clipboard nil)
(setq x-select-enable-clipboard nil)
; Don't save to the clipboard on exit
(setq x-select-enable-clipboard-manager nil)

;; Text and Notes
(setq sentence-end-double-space nil)

;; Save minibar history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Always show matching parens
(show-paren-mode t)

;; Backups (from https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files/20824625#20824625)
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t)   ;; Backup versioned files

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backups/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; Autosave files
(setq auto-save-file-name-transforms
          `((".*" , "~/.emacs.d/backups/auto-saves" t)))

;; remember cursor position
(toggle-save-place-globally)

;;; Spelling

;; map ]s and [s to next and previously wrong word

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        ))))


;;;; Packages

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
(package-initialize)

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

(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))

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


;(use-package powerline
;  :config
;  (powerline-evil-vim-color-theme))
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
  (linum-relative-global-mode))


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
   "b" 'ivy-switch-buffer
   "C-b" 'buffer-menu
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


(use-package magit
  :diminish magit-auto-revert-mode
  :config
  (setq evil-magit-state 'normal))

(use-package evil-magit
  :config
  (evil-magit-init))

(use-package magithub
  :config
  (magithub-feature-autoinject t))


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


;;;; System-specific configs

(defun win-setup ()
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
    (setq ispell-program-name "aspell")

    (defun cmd ()
      (interactive)
	(make-comint-in-buffer "cmd" nil "cmd" nil)
	(switch-to-buffer "*cmd*")))

(defun linux-setup ())

(cond ((eq system-type 'windows-nt) (win-setup))
      ((eq system-type 'gnu/linux) (linux-setup))
      (t (message "")))


;;;; Custom
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))

;; if no custom file exists, write a default one
(unless (file-exists-p custom-file)
  (write-region "(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-evil-normal-face ((t (:background \"#859900\")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    (\"c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc\" default))))
" nil custom-file))
(load custom-file)
