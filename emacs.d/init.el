;; TODO Move prettify lists into their own file.

;;;; Startup

;(package-initialize)

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode)

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
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

;; Always show matching parens
(show-paren-mode t)

;; Save Window layout history
(winner-mode)

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

;; Search all buffers
(defun grep-search-all-buffers (regexp)
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))

;; Tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "\"%s\" -f TAGS -e -R %s"
           tags-generator (directory-file-name dir-name))))

(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(require 'packages)

;; Buffer-based completion
(global-set-key (kbd "C-SPC") 'dabbrev-completion)


;;;; System-specific configs

(defun win-setup ()
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
    (setq ispell-program-name "aspell")

    ;; Add MinGW if it exists
    (if (file-directory-p "C:/MinGW/msys/1.0/bin")
      (setenv "PATH" (concat (getenv "PATH") "C:/MinGW/msys/1.0/bin")))
      ;; (add-to-list 'exec-path "C:/MinGW/msys/1.0/bin"))

    ;; Add tags
    (setq tags-generator (expand-file-name "ctags.exe" user-emacs-directory))

    (defun cmd ()
      (interactive)
	(make-comint-in-buffer "cmd" nil "cmd" nil)
	(switch-to-buffer "*cmd*"))

    (setq org-default-notes-file "~/../../Owncloud/org/organizer.org"))

(defun linux-setup ()
  (setq tage-generator "ctags")
  (setq org-default-notes-file "~/Owncloud/org/organizer.org"))

(cond ((eq system-type 'windows-nt) (win-setup))
      ((eq system-type 'gnu/linux) (linux-setup))
      (t (message "")))


;;;; Custom
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))

;; if no custom file exists, write a default one
(unless (file-exists-p custom-file)
  (write-region "(custom-set-faces
 '(linum-relative-current-face ((t (:inherit linum :background "dim gray" :foreground "white" :underline nil)))))
(custom-set-variables
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    (\"c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc\" default))))
" nil custom-file))
(load custom-file)
