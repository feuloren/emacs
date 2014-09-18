; First of all we need MELPA

(require 'package)
;(add-to-list 'package-archives
;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.milkbox.net/packages/") t)


; Then enable the proxy if needed


; And download all the packages !
(package-download-transaction
 '(
   ; Essential
   company-mode projectile undo-tree
   ; Helm
   helm helm-projectile
   ; Web
   php-mode emmet-mode web-mode rainbow-mode
   ; Lisp
   paredit rainbow-delimiters
   ; Python
   jedit
   ; Other
   highlight-symbol multiple-cursors
   rebox2 smooth-scroll hl-line
   ))
