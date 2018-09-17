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

(provide 'prettify-custom-symbols)
