;; First of all we need MELPA

(require 'package)
;(add-to-list 'package-archives
;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; Then enable the proxy if needed


;; And download all the packages !
(package-download-transaction
 '(
   ;; Essential
   company-mode projectile undo-tree
   ;; Helm
   helm helm-projectile helm-ack
   ;; Web
   php-mode emmet-mode web-mode rainbow-mode
   php-boris php-eldoc
   ;; Lisp
   paredit rainbow-delimiters
   ;; Python
   anaconda-mode
   ;; Clojure
   clojure-mode cider clojure-cheatsheet clj-refactor cljsbuild-mode
   ;; Misc
   highlight-symbol multiple-cursors
   rebox2 smooth-scrolling hl-line
   key-chord dired-details visible-mark
   visual-regexp-steroids
   ;; Org
   htmlize
   ;; Flycheck
   flycheck flycheck-pyflakes
   ;; VC
   magit
   ;; Theme
   monokai-theme
   ;; Libs
   s dash
   ;; Android
   android-mode
   ;; Scala
   sbt scala-mode2
   ))
