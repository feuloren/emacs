;; First of all we need MELPA

(require 'package)
;(add-to-list 'package-archives
;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; Then enable the proxy if needed


;; And download all the packages !
(defvar ft/needed-packages
 '(
   ;; Essential
   company-mode projectile undo-tree god-mode
   ;; Flycheck
   flycheck flycheck-tip
   ;; Web
   php-mode emmet-mode web-mode rainbow-mode
   php-boris php-eldoc js2-mode json-mode
   ;; Lisp
   paredit rainbow-delimiters
   ;; Python
   anaconda-mode company-anaconda ein
   ;; Clojure
   clojure-mode cider clojure-cheatsheet clj-refactor cljsbuild-mode flycheck-clojure
   ;; Misc
   highlight-symbol multiple-cursors
   rebox2 smooth-scrolling hl-line
   visible-mark ace-jump-mode
   visual-regexp-steroids ag
   relative-line-numbers smart-mode-line
   ;; ido
   smex ido-ubiquitous flx-ido
   ;; Dired
   dired-subtree dired-details
   ;; Org
   htmlize
   ;; VC
   magit
   ;; Theme
   monokai-theme
   ;; Libs
   s dash
   ;; Android
   android-mode
   ;; Scala
   sbt scala-mode2 ensime
   ;; Common Lisp
   slime slime-company
   ;; Haskell
   haskell-mode flycheck-haskell
   ;; Literate http requests
   restclient company-restclient
   ))

(defun install-packages (packages)
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (package-download-transaction (package-compute-transaction
				 () (delq nil (mapcar (lambda (elt)
							(when (member (car elt) packages)
							  (cadr elt)))
						      package-archive-contents)))))

(install-packages ft/needed-packages)
