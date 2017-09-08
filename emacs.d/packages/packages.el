;;; use-package example:
;; (use-package foo
;; :init ; Runs before loading the package. WIll always run, even if foo isn't on this system.
;; :config ; Runs after.
;; :bind (("M-s O" . action)
;;       ("" . some-other-action))
;; :commands foo-mode ; Creates autoloads for commands: defers loading until called.
;; )

;; Package installation

(require 'package)
;; Create the package install directory if it doesn't exist
(setq package-user-dir (format "%selpa_%s/"
                               user-emacs-directory emacs-major-version)) ; default = ~/.emacs.d/elpa/
(package-initialize)


(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))



;;;; Required packages

(use-package diminish
  :ensure t)

(diminish 'visual-line-mode)
(diminish 'abbrev-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package bind-key
  :ensure t)

(use-package iedit
  :ensure t)

(use-package hydra
  :ensure t)

(use-package engine-mode
  :ensure t)

(use-package evil
  :ensure t
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
              ("C-q" . delete-window)
         :map evil-operator-state-map
              ("lw" . evil-little-word)))

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
  :diminish undo-tree-mode)

(use-package undohist
  :ensure t
  :config
  ;; Save undo history under .emacs.d/undohist
  (setq undohist-directory "~/.emacs.d/undohist")
  (unless (file-exists-p  "~/.emacs.d/undohist")
    (make-directory "~/.emacs.d/undohist"))

  (undohist-initialize))

(use-package powerline
  :ensure t
  :config
  (powerline-evil-vim-theme))

(use-package powerline-evil
  :ensure t)

(set-face-background 'powerline-evil-normal-face "#859900")

(use-package linum-relative
  :ensure t
  :diminish linum-relative-mode
  :config
  (setq linum-relative-current-symbol "")
  (linum-mode)
  (linum-relative-global-mode)
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

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "d" 'diff-buffer-with-file
    "v" 'ivy-switch-buffer
    "b" 'buffer-menu
    "f" 'neotree-toggle
    "u" 'undo-tree-visualize
    "l" 'auto-fill-mode
    "s" 'flyspell-toggle-correct-mode
    "a" 'company-mode
    "g" 'magit-status
    "M-g" 'magit-dispatch-popup
    "c" 'flycheck-mode
    "w" '(lambda () (interactive)
           ;; "writing" mode
           (variable-pitch-mode)
           (visual-line-mode)
           (flyspell-toggle-correct-mode))
    ))

(use-package neotree
  :ensure t
  :config
  ;; Set vi-like bindings in neotree-mode that don't conflict with evil
  (evil-define-key 'normal neotree-mode-map
    (kbd "q") 'neotree-hide
    (kbd "RET") 'neotree-enter
    (kbd "h") 'neotree-hidden-file-toggle
    (kbd "r") 'neotree-refresh)

  ;; Every time when the neotree window is opened, let it find current file and jump to node.
  (setq neo-smart-open t)

  ;; List of files to hide
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.class")))

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
  :diminish company-mode)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
  (setq flycheck-standard-error-navigation nil)

  (global-flycheck-mode t))

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
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-mode 1))

(use-package org
  :ensure t
  :config
  (setq org-log-done 'time)
  )

;; Specified to get (org-timeline)
(use-package org-agenda
  :after org)

(use-package org-preview-html
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

(use-package monokai-theme
  :ensure t)

;; OS specific
(use-package magit
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :diminish magit-auto-revert-mode)

(use-package evil-magit
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :demand
  :config
  (evil-magit-init))

(use-package magithub
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :demand
  (magithub-feature-autoinject t))

(use-package multi-term
  :if (not (eq system-type 'windows-nt))
  :ensure t)


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

(use-package js
  :config
  (setq js-indent-level 2))

(use-package tide
  :config
  (setq typescript-indent-level 2))

(use-package racket-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))

  ;; C-w prefix in racket-REPL
  (add-hook 'racket-repl-mode-hook 'racket-repl-evil-hook)

  (defun racket-repl-evil-hook ()
    (define-key racket-repl-mode-map "\C-w" 'evil-window-map)
    (global-set-key (kbd "C-w") 'racket-repl-mode-map)))

(use-package haskell-mode
  :config
  (setq haskell-interactive-popup-errors nil)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-p") 'haskell-process-reload))

(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package latex-preview-pane)

;; List of optional packages
(defvar optional-packages
      '(
        flymd
        markdown-mode
        latex-preview-pane
        tide
        web-mode
        racket-mode
        haskell-mode
        realgud
        emojify
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


(global-prettify-symbols-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  (append
                   '(
                     ("->" . ?→)
                     ("lambda" . ?λ)
                     ("->" . ?→)
                     ("<=" . ?≤)
                     (">=" . ?≥)
                     ("!=" . ?≠)) prettify-symbols-alist))))

;; List from https://github.com/cpitclaudel/.emacs.d/blob/master/lisp/prettify-alists/haskell-prettify.el
(add-hook 'haskell-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
		    (append
		     '(;; Double-struck letters
		       ("|A|" . ?𝔸)
		       ("|B|" . ?𝔹)
		       ("|C|" . ?ℂ)
		       ("|D|" . ?𝔻)
		       ("|E|" . ?𝔼)
		       ("|F|" . ?𝔽)
		       ("|G|" . ?𝔾)
		       ("|H|" . ?ℍ)
		       ("|I|" . ?𝕀)
		       ("|J|" . ?𝕁)
		       ("|K|" . ?𝕂)
		       ("|L|" . ?𝕃)
		       ("|M|" . ?𝕄)
		       ("|N|" . ?ℕ)
		       ("|O|" . ?𝕆)
		       ("|P|" . ?ℙ)
		       ("|Q|" . ?ℚ)
		       ("|R|" . ?ℝ)
		       ("|S|" . ?𝕊)
		       ("|T|" . ?𝕋)
		       ("|U|" . ?𝕌)
		       ("|V|" . ?𝕍)
		       ("|W|" . ?𝕎)
		       ("|X|" . ?𝕏)
		       ("|Y|" . ?𝕐)
		       ("|Z|" . ?ℤ)
		       ("|gamma|" . ?ℽ)
		       ("|Gamma|" . ?ℾ)
		       ("|pi|" . ?ℼ)
		       ("|Pi|" . ?ℿ)

		       ;; Types
		       ("::" . ?∷)

		       ;; Quantifiers
		       ("forall" . ?∀)
		       ("exists" . ?∃)

		       ;; Arrows
		       ("-->" . ?⟶)
		       ("<-" . ?←)
		       ("<--" . ?⟵)
		       ("<->" . ?↔)
		       ("<-->" . ?⟷)

		       ("=>" . ?⇒)
		       ("==>" . ?⟹)
		       ("<==" . ?⟸)
		       ("<=>" . ?⇔)
		       ("<==>" . ?⟺)

		       ("|->" . ?↦)
		       ("|-->" . ?⟼)
		       ("<-|" . ?↤)
		       ("<--|" . ?⟻)

		       ("|=>" . ?⤇)
		       ("|==>" . ?⟾)
		       ("<=|" . ?⤆)
		       ("<==|" . ?⟽)

		       ("~>" . ?⇝)
		       ("<~" . ?⇜)

		       (">->" . ?↣)
		       ("<-<" . ?↢)
		       ("->>" . ?↠)
		       ("<<-" . ?↞)

		       (">->>" . ?⤖)
		       ("<<-<" . ?⬻)

		       ("<|-" . ?⇽)
		       ("-|>" . ?⇾)
		       ("<|-|>" . ?⇿)

		       ("<-/-" . ?↚)
		       ("-/->" . ?↛)

		       ("<-|-" . ?⇷)
		       ("-|->" . ?⇸)
		       ("<-|->" . ?⇹)

		       ("<-||-" . ?⇺)
		       ("-||->" . ?⇻)
		       ("<-||->" . ?⇼)

		       ("-o->" . ?⇴)
		       ("<-o-" . ?⬰)

		       ;; Boolean operators
		       ("not" . ?¬)
		       ("&&" . ?∧)
		       ("||" . ?∨)

		       ;; Relational operators
		       ("==" . ?≡)
		       ("/=" . ?≠)
		       ("<=" . ?≤)
		       (">=" . ?≥)
		       ("/<" . ?≮)
		       ("/>" . ?≯)

		       ;; Containers / Collections
		       ("++" . ?⧺)
		       ("+++" . ?⧻)
		       ("|||" . ?⫴)
		       ("empty" . ?∅)
		       ("elem" . ?∈)
		       ("notElem" . ?∉)
		       ("member" . ?∈)
		       ("notMember" . ?∉)
		       ("union" . ?∪)
		       ("intersection" . ?∩)
		       ("isSubsetOf" . ?⊆)
                       ("isProperSubsetOf" . ?⊂)

                       ;; Other
                       ("<<" . ?≪)
                       (">>" . ?≫)
                       ("<<<" . ?⋘)
                       (">>>" . ?⋙)
                       ("<|" . ?⊲)
                       ("|>" . ?⊳)
                       ("><" . ?⋈)
                       ("mempty" . ?∅)
                       ("mappend" . ?⊕)
                       ("<*>" . ?⊛)
                       ("undefined" . ?⊥)
                       (":=" . ?≔)
                       ("=:" . ?≕)
                       ("=def" . ?≝)
                       ("=?" . ?≟)
                       ("..." . ?…)) prettify-symbols-alist))))

(setq python--prettify-symbols-alist
   '(("def" .      #x2131)
     ("not" .      #x2757)
     ("return" .   #x27fc)
     ("yield" .    #x27fb)
     ("or" . ?∨)
     ("and" . ?∧)
     ("None" . ?⊥)
     ("set()" . ?∅)
     ("not in" . ?∉)
     ("in" . ?∈)
     ("is not" . ?≢)
     ("is" . ?≡)))

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
        (flyspell-buffer)))))

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
        recentf-max-menu-items 15))

(provide 'packages)
