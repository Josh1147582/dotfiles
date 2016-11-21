(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
	 (if (package-installed-p package)
		 nil
	   (if (y-or-n-p (format "Package %s is missing. Install it? " package))
		   (package-install package)
		 package)))
   packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
	(package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; Check that all packages are installed
(ensure-package-installed
 'iedit
 'magit
 'undo-tree
 'evil
 'evil-leader
 'evil-tabs
 'zenburn-theme
 'powerline-evil
 )

;; Evil

;; Evil leader is Space
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")

;; Evil tabs
;; (global-evil-tabs-mode t)

;; Default to evil mode
;; (require 'evil)
;; (evil-mode t)

;; Leader keybinds
;; (evil-leader/set-key
;;   "u" 'undo-tree-visualize
;;   "m" 'recentf-open-files)

;; Delete info bindings for evil to take over
;; (define-key Info-mode-map "g" nil)
;; (define-key Info-mode-map "n" nil)
;; (define-key Info-mode-map "p" nil)

;; Disable file backup
(setq make-backup-files nil)

;; Save undo history under .emacs.d/undo
(setq undo-tree-auto-save-history t
         undo-tree-history-directory-alist
         `(("." . ,(concat user-emacs-directory "undo"))))
   (unless (file-exists-p (concat user-emacs-directory "undo"))
(make-directory (concat user-emacs-directory "undo")))


;; Powerline
(require 'powerline)
(powerline-vim-theme)

;; Recent Files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; Leave the clipboard alone
(setq x-select-enable-clipboard nil)

;; All yes or no prompts are y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Never follow symlinks
(setq vc-follow-symlinks nil)

;; Text and Notes
(setq sentence-end-double-space nil)
;; Save minibar history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; TODO:
;; Go through the tutorials, skim the manuals
;; learning elisp
;; Fuzzy
;; Evil leader mode
;; Hotkey for undo tree
;; autocomplete
;; recent files
;; magit bindings




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" default)))
 '(inhibit-default-init t)
 '(inhibit-startup-buffer-menu nil)
 '(inhibit-startup-echo-area-message "josh")
 '(initial-buffer-choice t)
 '(initial-scratch-message ";; scratch buffer

")
 '(package-selected-packages
   (quote
    (evil-tabs powerline-evil zenburn-theme magit iedit evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
