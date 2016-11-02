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

;; Assuming you wish to install "iedit" and "magit"
(ensure-package-installed 'iedit
			  'magit
			  'undo-tree
			  'evil)

;; Default to evil mode
(require 'evil)
(evil-mode t)

;; All yes or no prompts are y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable file backup
(setq make-backup-files nil)

;; Save undo history under .emacs.d/undo
(setq undo-tree-auto-save-history t
         undo-tree-history-directory-alist
         `(("." . ,(concat user-emacs-directory "undo"))))
   (unless (file-exists-p (concat user-emacs-directory "undo"))
(make-directory (concat user-emacs-directory "undo")))


;; TODO:
;; Evil leader mode
;; Hotkey for undo tree
;; autocomplete
;; recent files
;; magit bindings
;; learning elisp
