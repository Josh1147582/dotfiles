;; Package installation

;; TODO add "]]" or some other binding for doc-view-previous/next-page
;; in doc-view-mode.

(require 'package)
;; Create the package install directory if it doesn't exist
(setq package-user-dir (format "%selpa_%s/"
                               user-emacs-directory emacs-major-version)) ; default = ~/.emacs.d/elpa/

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;;;; Required packages

(setq use-package-verbose t)

(use-package diminish
  :ensure t)

(diminish 'visual-line-mode)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package iedit
  :ensure t)

(defmacro hail (p)
  (list 'use-package p ':ensure 't))

(hail hydra)

(use-package engine-mode
  :ensure t)

(use-package evil
  :ensure t
  :init
  (defvar evil-want-integration nil)
  :config
  (evil-mode t)
  (setq evil-want-C-i-jump nil)
  (setq evil-default-state 'normal)

  ;; Move all elements of evil-emacs-state-modes to evil-motion-state-modes
  (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes)
        evil-emacs-state-modes (list 'magit-popup-mode))
  (delete 'magit-popup-mode evil-motion-state-modes)

  ;; Don't echo evil's states
  (setq evil-insert-state-message nil
        evil-visual-state-message nil)

  ;; Little words (camelCase)
  (evil-define-motion evil-little-word (count)
    :type exclusive
    (let* ((case-fold-search nil)
           (count (if count count 1)))
      (while (> count 0)
        (forward-char)
        (search-forward-regexp "[_A-Z]\\|\\W" nil t)
        (backward-char)
        (decf count))))

  ;; Don't litter registers with whitespace
  (defun destroy-whitespace--evil-delete-around (func beg end type &optional reg yh)
    (let ((clean-string (replace-regexp-in-string "[ \t\n]" "" (buffer-substring beg end))))
      (if (equal "" clean-string)
          (apply func beg end type ?_ yh)
        (apply func beg end type reg yh))))

  (advice-add 'evil-delete :around #'destroy-whitespace--evil-delete-around)

  ;; eval the last sexp while in normal mode (include the character the cursor is currently on)
  (defun evil-eval-last-sexp ()
    (interactive)
    (evil-append 1)
    (eval-last-sexp nil)
    (evil-normal-state))

  ;; select recently pasted text
  ;; from https://emacs.stackexchange.com/a/21093
  (defun my/evil-select-pasted ()
  (interactive)
  (let ((start-marker (evil-get-marker ?[))
        (end-marker (evil-get-marker ?])))
        (evil-visual-select start-marker end-marker)))

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

  (defhydra hydra-window (global-map "C-w")
    "window layout"
    ("u" winner-undo "undo")
    ("U" winner-redo "redo"))

  ;; Make K select manpage or engine-mode (m for man, g for google?)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (defhydra hydra-lookup-menu ()
    "Choose lookup"
    ("g" engine/search-google "Google" :color blue)
    ("m" evil-lookup "man" :color blue))
  (define-key evil-normal-state-map "K" 'hydra-lookup-menu/body)
  (define-key evil-visual-state-map "K" 'hydra-lookup-menu/body)

  ;; These go out here, because undefining keybinds is hard
  (define-key global-map "\C-j" nil)
  (define-key evil-normal-state-map "\C-j" 'flyspell-goto-next-error)

  (evil-define-key 'normal package-menu-mode-map "q" 'quit-window)

  :bind (:map evil-normal-state-map
              ("zs" . hscroll-cursor-left)
              ("ze" . hscroll-cursor-right)
              ("[s" . flyspell-goto-previous-error)
              ("]s" . flyspell-goto-next-error)
              ("\C-k" . flyspell-goto-previous-error)
              ("\C-x \C-e" . evil-eval-last-sexp)
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("gj" . evil-next-line)
              ("gk" . evil-previous-line)
         ; :map Info-mode-map
         ;      ("g" . nil)
         ;      ("n" . nil)
         ;      ("p" . nil)
         :map evil-window-map
              ("q" . delete-window)
              ("C-q" . delete-window)
	      ("x" . kill-buffer-and-window)
         :map Buffer-menu-mode-map
	      ("SPC" . nil)))

(use-package evil-numbers
  :ensure t
  :config
  ;; Increment and decrement (evil-numbers)
  (defhydra hydra-numbers (global-map "C-x")
    "modify numbers"
    ("a" evil-numbers/inc-at-pt "increment")
    ("x" evil-numbers/dec-at-pt "decrement")))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-enable-undo-in-region nil))

(use-package undohist
  :ensure t
  :config
  ;; Save undo history under .emacs.d/undohist
  (setq undohist-directory "~/.emacs.d/undohist")
  (unless (file-exists-p  "~/.emacs.d/undohist")
    (make-directory "~/.emacs.d/undohist"))

  (undohist-initialize))

(use-package powerline-evil
  :ensure t
  :config
  (defun powerline-center-evil-theme ()
    "Setup a mode-line with major, evil, and minor modes centered."
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face1 (if active 'powerline-active1 'powerline-inactive1))
			    (face2 (if active 'powerline-active2 'powerline-inactive2))
			    (separator-left (intern (format "powerline-%s-%s"
							    (powerline-current-separator)
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     (powerline-current-separator)
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (powerline-raw "%*" mode-line 'l)
				       (powerline-buffer-id mode-line-buffer-id 'l)
				       (powerline-raw " " mode-line)
				       (funcall separator-left mode-line face1)
				       (powerline-narrow face1 'l)
				       (powerline-vc face1)))
			    (rhs (list (funcall separator-right face1 mode-line)
				       (powerline-raw mode-line-misc-info mode-line 'r)
					;(powerline-raw global-mode-string face1 'r)
				       (powerline-raw "%2l" mode-line 'r)
				       (powerline-raw ":" mode-line)
				       (powerline-raw "%2c" mode-line 'r)
					;(powerline-raw " ")
					;(powerline-raw "%6p" mode-line 'r)
				       (powerline-hud face2 face1)))
			    (center (append (list (powerline-raw " " face1)
						  (funcall separator-left face1 face2)
						  (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
						    (powerline-raw erc-modified-channels-object face2 'l))
						  (powerline-major-mode face2 'l)
						  (powerline-process face2)
						  (powerline-raw " " face2))
					    (if (split-string (format-mode-line minor-mode-alist))
						(append (if evil-mode
							    (list (funcall separator-right face2 face1)
								  (powerline-raw evil-mode-line-tag face1 'l)
								  (powerline-raw " " face1)
								  (funcall separator-left face1 face2)))
							(list (powerline-minor-modes face2 'l)
							      (powerline-raw " " face2)
							      (funcall separator-right face2 face1)))
					      (list (powerline-raw evil-mode-line-tag face2)
						    (funcall separator-right face2 face1))))))
		       (concat (powerline-render lhs)
			       (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			       (powerline-render center)
			       (powerline-fill face1 (powerline-width rhs))
			       (powerline-render rhs)))))))
  (defun powerline-evil-vim-theme ()
    "Powerline's Vim-like mode-line with evil state at the beginning."
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face1 (if active 'powerline-active1 'powerline-inactive1))
			    (face2 (if active 'powerline-active2 'powerline-inactive2))
			    (separator-left (intern (format "powerline-%s-%s"
							    (powerline-current-separator)
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     (powerline-current-separator)
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (if evil-mode
					   (powerline-raw (powerline-evil-tag) mode-line))
				       (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
				       (powerline-raw "[" mode-line 'l)
				       (powerline-major-mode mode-line)
				       (powerline-process mode-line)
				       (powerline-raw "]" mode-line)
				       (when (buffer-modified-p)
					 (powerline-raw "[+]" mode-line))
				       (when buffer-read-only
					 (powerline-raw "[RO]" mode-line))
				       ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
				       (when (and (boundp 'which-func-mode) which-func-mode)
					 (powerline-raw which-func-format nil 'l))
				       (when (boundp 'erc-modified-channels-object)
					 (powerline-raw erc-modified-channels-object face1 'l))
				       (powerline-raw "[" mode-line 'l)
				       (powerline-minor-modes mode-line) (powerline-raw "%n" mode-line)
				       (powerline-raw "]" mode-line)
				       (when (and vc-mode buffer-file-name)
					 (let ((backend (vc-backend buffer-file-name)))
					   (when backend
					     (concat (powerline-raw "[" mode-line 'l)
						     (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
						     (powerline-raw "]" mode-line)))))))
			    (rhs (list (powerline-raw mode-line-misc-info mode-line 'r)
				       (powerline-raw global-mode-string mode-line 'r)
				       (powerline-raw "%l," mode-line 'l)
				       (powerline-raw (format-mode-line '(10 "%c")))
				       (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r))))
		       (concat (powerline-render lhs)
			       (powerline-fill mode-line (powerline-width rhs))
			       (powerline-render rhs)))))))

  (if (or (display-graphic-p) (daemonp))
      (powerline-center-evil-theme)
    (powerline-evil-vim-theme)))

(use-package linum-relative
  :ensure t
  :diminish linum-relative-mode
  :config
  (setq linum-relative-current-symbol "")
  (if (not (or (display-graphic-p) (daemonp)))
      (setq linum-relative-format "%3s "))

  (add-hook 'prog-mode-hook 'linum-relative-mode)

  (defun linum-update-window-scale-fix (win)
    "fix linum for scaled text"
    (set-window-margins
     win
     (ceiling (* (if (boundp 'text-scale-mode-step)
                     (expt text-scale-mode-step
                           text-scale-mode-amount) 1)
                 (if (car (window-margins))
                     (car (window-margins)) 1)
                 ))))

  (advice-add #'linum-update-window
              :after #'linum-update-window-scale-fix))

(use-package bind-map
  :ensure t
  :after evil
  :after evil-numbers
  :config
  (bind-map
   my-base-leader-map
   :keys ("M-m")
   :evil-keys ("SPC")
   :evil-states (normal motion visual)
   :bindings
   ("d" 'diff-buffer-with-file
    "b" 'buffer-menu
    "f" 'treemacs-toggle
    "u" 'undo-tree-visualize
    "l" 'auto-fill-mode
    "s" 'flyspell-toggle-correct-mode
    "a" 'company-mode
    "g" 'magit-status
    "c" 'flycheck-mode
    "w" '(lambda () (interactive)
	   ;; "writing" mode
	   (variable-pitch-mode)
	   (visual-line-mode))
    "p" 'my/evil-select-pasted
    "/" 'swiper
    "v" 'ivy-switch-buffer
    "n" 'hydra-numbers/body
    ;; TODO find a better way to display this
    ;; TODO add button to kill this: maybe k1, k2, etc.?
    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "5" 'eyebrowse-switch-to-window-config-5
    "6" 'eyebrowse-switch-to-window-config-6
    "7" 'eyebrowse-switch-to-window-config-7
    "8" 'eyebrowse-switch-to-window-config-8
    "9" 'eyebrowse-switch-to-window-config-9))

  (bind-map
    my-elisp-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :major-modes (emacs-lisp-mode)
    :bindings
    ("el" 'evil-eval-last-sexp
     "er" 'eval-region
     "eb" 'eval-buffer)))

(use-package treemacs
  :ensure t
  :bind (:map treemacs-mode-map
	      ("." . treemacs-toggle-show-dotfiles)))

(use-package treemacs-evil
  :after treemacs
  :ensure t)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t))

(use-package flx
  :ensure t
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'prog-mode-hook 'company-mode)
)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(idle-change new-line save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package dtrt-indent
  :ensure t
  :config
  (setq global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode 1))

(use-package org
  :commands org-mode
  :after bind-map
  :ensure t
  :config
  (setq org-log-done 'time)
  (defun org->odt->pdf ()
    "Someday I'll learn how to properly format the LaTeX to PDF output."
    (interactive)
    (org-odt-export-to-odt)
    (shell-command
     (concat
      "libreoffice --headless --convert-to pdf \"" (file-name-sans-extension (buffer-name)) ".odt\""
      " && echo Done")))
  (setq org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6" :rules "all" :frame "border"))

  (setq org-latex-minted-options
    '("breaklines"))

  (defun org-variable-toggle-latex-fragment ()
    "Toggle LaTeX fragment, taking into account the current zoom size of the buffer."
    (interactive)
    (if (boundp 'text-scale-mode-amount)
	(let ((org-format-latex-options (plist-put org-format-latex-options :scale text-scale-mode-amount)))
	  (org-toggle-latex-fragment))
      (org-toggle-latex-fragment)))
  (face-attribute 'default :font)

  ;; I might experiemnt with gnuplot and notes in the future.
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((gnuplot . t)))
  ;; (global-set-key (kbd "C-c c") 'org-capture)

  (bind-map
   my-org-map
   :keys ("M-m")
   :evil-keys ("SPC")
   :major-modes (org-mode)
   :bindings
   (;"ol" 'org-toggle-latex-fragment
    "ol" 'org-variable-toggle-latex-fragment
    "ot" 'org-timeline
    "oa" 'org-archive-subtree
    "od" 'org-deadline
    "os" 'org-schedule
    "op" 'org-priority
    "t" 'org-todo)))

(use-package org-agenda
  :commands org-agenda org-timeline
  :after org
  :after evil
  :config
  ;; Rip org-timeline
  (defun org-timeline ()
    (interactive)
      (let ((org-agenda-custom-commands
	'(("z" "" agenda ""
	   ((org-agenda-span 'year)
	    ;; (org-agenda-time-grid nil)
	    (org-agenda-show-all-dates nil)
	    ;; (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
	    (org-deadline-warning-days 0))))))

	(org-agenda nil "z" 'buffer)))
  ;; Not sure if this can be placed in a :bind statement
  (evil-define-key 'motion org-agenda-mode-map (kbd "RET") '(lambda () (interactive) (org-agenda-switch-to t)))
  (setq org-agenda-files (quote ("~/Owncloud/org/organizer.org"))))

(use-package org-preview-html
  :commands org-preview-html/preview
  :after org
  :ensure t)

(use-package evil-ediff
  :ensure t
  :config
  (add-hook 'ediff-load-hook 'evil-ediff-init))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
  (setq rainbow-identifiers-faces-to-override
        '(
          font-lock-constant-face
          font-lock-type-face
          font-lock-function-name-face
          font-lock-variable-name-face
          font-lock-keyword-face)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-evil-keys)
  (setq eyebrowse-new-workspace t))

(use-package solarized-theme
  :ensure t)

(use-package solaire-mode
  :ensure t
  :config
  ;; highlight the minibuffer when it is activated
  (set-face-attribute 'solaire-minibuffer-face nil :inherit 'solaire-default-face :background "gainsboro")
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))

(use-package evil-goggles
  :ensure t
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; TODO Finish up special rules for windows like help and repl windows.
;; Where they're positioned, whether or not they're focused, etc.
(use-package shackle
  :ensure t
  :init
  (shackle-mode)
  (setq shackle-rules '(("*Python*" :align t :size 0.2)
			("*Help*" :align t :size 0.4 :select t)
			("\\`\\*intero:.*:repl\\*\\'" :regexp t :align t :size 0.4))))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
  ;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom)
  (defvar i 1)
  (while (< i 10)
    (let ((cell (cons (cons (number-to-string i) nil) '(lambda (cs) t))))
      (add-to-list 'which-key-replacement-alist cell))
    (setq i (+ i 1)))
  (makunbound 'i)

  (which-key-add-key-based-replacements
  "SPC d" "Diff buffer w/ file"))

;; OS specific
(use-package magit
  :commands magit-status
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :defer t
  :diminish magit-auto-revert-mode)

(use-package evil-magit
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :config
  (evil-magit-init))

(use-package multi-term
  :if (not (eq system-type 'windows-nt))
  :ensure t)

(use-package esup
  :commands esup
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; global-prettify-symbols doesn't play nice on Windows
(if (not (eq system-type 'windows-nt))
    (global-prettify-symbols-mode))

(require 'prettify-custom-symbols)

;;;; Optional packages

(use-package flymd
  :config
  (setq flymd-close-buffer-delete-temp-files t))

(use-package web-mode
  :config
  ;; 2 spaces for an indent
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2
          web-mode-enable-auto-closing t
          web-mode-enable-auto-pairing t)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; Auto-enable web-mode when opening relevent files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode)))

(setq js-indent-level 2)

(use-package tide
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package racket-mode
  :mode "\\.scm\\'"
  :config
  ;; C-w prefix in racket-REPL
  (add-hook 'racket-repl-mode-hook 'racket-repl-evil-hook)

  (defun racket-repl-evil-hook ()
    (define-key racket-repl-mode-map "\C-w" 'evil-window-map)
    (global-set-key (kbd "C-w") 'racket-repl-mode-map)))

(use-package intero
  :commands intero-mode
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (bind-map
   my-haskell-map
   :keys ("M-m")
   :evil-keys ("SPC")
   :major-modes (haskell-mode)
   :bindings
   ("l" 'intero-repl-load
    "r" 'intero-repl)))

(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package latex-preview-pane
  :commands latex-preview-pane-mode)

(use-package slime
  :after bind-key
  :commands slime slime-mode
  :init
  (setq auto-mode-alist (cons '("\\.cl$" . common-lisp-mode) auto-mode-alist))
  (add-hook 'lisp-mode-hook 'slime-mode)
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup)

  (defun evil-slime-eval-last-expression ()
    (interactive)
    (evil-append 1)
    (slime-eval-last-expression)
    (evil-normal-state))

  (bind-map
   my-slime-map
   :keys ("M-m")
   :evil-keys ("SPC")
   :major-modes (lisp-mode)
   :bindings
   ("el" 'evil-slime-eval-last-expression
    "er" 'slime-eval-region
    "eb" 'slime-compile-and-load-file)))

(use-package slime-company
  :after slime)

;; TODO learn/configure auctex
(use-package auctex
  :defer t)

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

;; List of optional packages
(defvar optional-packages
      '(
        flymd
        web-mode
        tide
        racket-mode
        intero
        emojify
        latex-preview-pane
	slime
	slime-company
        markdown-mode
	auctex
	company-auctex
        atomic-chrome
        ))

(defvar packages-installed-this-session nil)
(defun ensure-package-installed (prompt package)
  "Ensure a package is installed, and (optionally) ask if it isn't."
  (if (not (package-installed-p package))
      (if (or prompt (y-or-n-p (format "Package %s is missing. Install it? " package)))
	  ;; If this is the 1st install this session, update before install
	  (cond ((not packages-installed-this-session)
		 (package-refresh-contents)
		 (setq packages-installed-this-session t)
		 (package-install package))
		(t (package-install package))
		nil)
	package)))

(defun optional-packages-install ()
  "Ask to install any optional packages."
  (interactive)
  (mapcar (lambda (package) (ensure-package-installed nil package)) optional-packages))


;;;; Builtin configs

(defvar gdb-many-windows t)

(setq tramp-syntax (quote default))

(setq prolog-program-name "swipl")
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(bind-map
  my-prolog-map
  :keys ("M-m")
  :evil-keys ("SPC")
  :major-modes (prolog-mode)
  :bindings
  ("l" 'prolog-consult-buffer))

(use-package doc-view
  :defer t
  :config
  (evil-define-key 'motion doc-view-mode-map
    (kbd "k") 'doc-view-previous-line-or-previous-page
    (kbd "j") 'doc-view-next-line-or-next-page
    (kbd "C-b") 'doc-view-previous-page
    (kbd "C-f") 'doc-view-next-page)
)


(use-package flyspell
  :config
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
              (setq arg 0))))))

  (defun flyspell-toggle-correct-mode ()
    "Decide whether to use flyspell-mode or flyspell-prog-mode, then properly toggle."
    (interactive)
    ;; use flyspell-mode when in text buffers
    ;; otherwise use flyspell-prog-mode
    (let* ((current-mode
            (buffer-local-value 'major-mode (current-buffer)))
           (flyspell-mode-to-call
            (if (or (string= current-mode "text-mode") (string= current-mode "markdown-mode"))
                'flyspell-mode
              'flyspell-prog-mode)))
      ;; toggle the current flyspell mode, and
      ;; eval the buffer if we turned it on
      (if flyspell-mode
          (funcall 'flyspell-mode '0)
        (funcall flyspell-mode-to-call)
        (flyspell-buffer))))

  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package hideshow
  :config
  (add-hook 'prog-mode-hook
            'hs-minor-mode)
  (add-hook 'hs-minor-mode-hook
            (lambda ()
              (diminish 'hs-minor-mode))))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (add-to-list 'recentf-exclude ".*.emacs\\.d/elpa.*"))

(use-package dired
  :bind (:map dired-mode-map
	      ("SPC" . nil)))



(provide 'packages)
