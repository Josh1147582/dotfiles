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

(provide 'prettify-custom-symbols)
