;; Disable beep & flash
 (setq ring-bell-function 'ignore)

;; All yes or no prompts are y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Never follow symlinks
(setq vc-follow-symlinks nil)

;; Leave the OS clipboard alone (use evil's "+ and "* instead)
(setq x-select-enable-clipboard nil)

;; Text and Notes
(setq sentence-end-double-space nil)

;; Save minibar history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;;;; Packages

;; Package installation
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Function to ensure every package in installed, and ask if it isn't.
(defun ensure-package-installed (&rest packages)
  (mapcar
   (lambda (package)
	 (if (package-installed-p package)
		 nil
	   (if (y-or-n-p (format "Package %s is missing. Install it? " package))
		   (package-install package)
		 package)))
   packages))

;; Make sure to have downloaded archive description.
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
 'powerline-evil
 'zenburn-theme
 'auto-complete
 'fuzzy
 'general
 'relative-line-numbers
 )


;;;; Evil

;; Evil leader is Space
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Evil tabs
(global-evil-tabs-mode t)

;; Default to evil mode
(evil-mode t)

;; Leader keybinds
(evil-leader/set-key
 "u" 'undo-tree-visualize
 "m" 'recentf-open-files
 "l" 'auto-fill-mode
 "s" 'flyspell-mode)

;; Move all elements of evil-emacs-state-modes to evil-motion-state-modes
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;; Delete info bindings for evil to take over
(define-key Info-mode-map "g" nil)
(define-key Info-mode-map "n" nil)
(define-key Info-mode-map "p" nil)


;;;; Files

;; Disable file backup
(setq make-backup-files nil)

;; Instead save undo history under .emacs.d/undo
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

;; Autocomplete
(require 'auto-complete)
(ac-config-default)
(define-key ac-mode-map (kbd "TAB") 'auto-complete)
(setq ac-auto-start nil)
(global-set-key (kbd "<backtab>") 'ac-previous)

;; Spelling
;; TODO Mess with how I want spelling to be done. Maybe enable spelling on auto-fill mode?

;; map ]s and [s to next and previously wrong word
(require 'general)
(general-evil-setup)
(general-nmap "]"
	      (general-key-dispatch 'evil-change
				    "s" 'flyspell-goto-next-error
				    ))
(general-vmap "]" 'evil-change)

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
        (forward-word)))))

(general-nmap "["
	      (general-key-dispatch 'evil-change
				    "s" 'flyspell-goto-previous-error
				    ))
(general-vmap "[" 'evil-change)

;; Relative line numbers
(require 'relative-line-numbers)
(global-relative-line-numbers-mode)

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
 '(initial-scratch-message "")
 '(package-selected-packages
   (quote
    (relative-line-numbers general fuzzy auto-complete evil-tabs powerline-evil zenburn-theme magit iedit evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
