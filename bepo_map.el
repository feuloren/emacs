(setq lexical-binding t)

(defun define-keys (map &rest keys)
  (let ((saved-key))
    (dolist (key-or-fun keys)
      (cond (saved-key
	     (funcall #'define-key map (kbd saved-key) key-or-fun)
	     (setq saved-key nil))
	    (t
	     (setq saved-key key-or-fun))))))

(require 'god-mode)
(define-keys god-local-mode-map
  ;; Move
  "t" #'backward-char
  "d" #'previous-line
  "s" #'next-line
  "r" #'forward-char
  "T" #'backward-word
  "R" #'forward-word
  "þ" #'beginning-of-line
  "®" #'end-of-line
  "<" #'beginning-of-buffer
  ">" #'end-of-buffer
  "ß" #'ace-jump-mode
  "a" #'isearch-forward
  "A" #'isearch-backward
  "L" #'er/expand-region
  ;; Edit
  "ù" #'query-replace
  "i" #'ft/delete-backward-char
  "e" #'ft/backward-kill-word
  "E" #'ft/kill-word
  "€" #'ft/kill-region
  "à" #'kill-ring-save
  "J" #'join-to-next-line
  "K" #'kill-whole-line
  ;; Buffers
  "," nil
  ", c" #'ido-switch-buffer
  ", t" #'previous-buffer
  ", r" #'next-buffer
  ;; Multiple cursors
  "m" #'mc/mark-next-like-this
  "M" #'mc/skip-to-next-like-this
  "v" #'mark-next-like-symbol-under-cursor
  "V" #'skip-to-next-like-symbol-under-cursor 
  ;; Projectile
  "p" nil
  "p n" #'projectile-find-file
  "p d" #'projectile-dired
  "p g" #'projectile-ag
  "p s" #'projectile-save-project-buffers
  "p t" #'projectile-switch-project
  ;; Misc
  "?" #'help-around-cursor
  "*" #'ft/go-to-definition
  "'" #'transient-mark-mode
  "q" #'ft/god-q
  ;; Org
  "l" nil
  "l l" #'org-capture
  "l i" #'org-clock-in
  "l o" #'org-agenda-clock-out
  "l t" #'org-time-stamp-inactive
  ;; Undo-tree
  "/" #'undo-tree-undo
  "+" #'undo-tree-redo
  ;; Yasnippets
  "f" #'ft/yas-insert-snippet
  ;; Paredit
  "|" #'paredit-split-sexp
  "b" nil
  "b t" #'paredit-forward-slurp-sexp
  "b d" #'paredit-splice-sexp-killing-backward
  "b r" #'paredit-forward-barf-sexp
  ;; Bookmark
  "b b" #'bookmark-set
  "b l" #'bookmark-bmenu-list
  ;; Windows
  "«" #'split-window-vertically
  "»" #'split-window-horizontally
  "$" #'delete-window
  "\"" #'delete-other-windows
)
(global-set-key (kbd "C-x C-n") #'ido-find-file)

;; Paredit requires its own commands to be called in order
;; to keep the ast valid
(require 'subr-x)
(defmacro ft/paredit-alternative (fun paredit-alternative)
  `(defun ,(intern (string-join (list "ft/" (symbol-name fun)))) ()
     (interactive)
     (if (bound-and-true-p paredit-mode)
	 (call-interactively ',paredit-alternative)
       (call-interactively ',fun))))

(ft/paredit-alternative delete-backward-char paredit-backward-delete)
(ft/paredit-alternative paredit-backward-kill-word backward-kill-word)
(ft/paredit-alternative paredit-forward-kill-word kill-word)
(ft/paredit-alternative paredit-kill-region kill-region)

;; I prefer to join the next line to the current line, not the way
;; emacs does it.
;; If possible this command should even join lines according to what
;; semantically makes sense
(defun join-to-next-line ()
  (interactive)
  (join-line -1))

(defun ft/god-q (arg)
  "I want q in god-mode to do the regular 'closing' action in read-only buffers
uq invokes q-other-window"
  (interactive "P")
  (if (null arg)
      (progn
	(if (or (bound-and-true-p buffer-read-only) (eq major-mode 'Custom-mode))
	    (progn
	      (god-mode-all)
	      (call-interactively (key-binding "q"))
	      (god-mode-all))
	  (call-interactively 'quoted-insert)))
    (call-interactively 'q-other-window)))

;; Turn off god-mode after inserting the skeleton of a snippet so I
;; can type right away
(defun ft/yas-insert-snippet ()
  (interactive)
  (when (bound-and-true-p god-global-mode)
    (god-mode-all))
  (call-interactively 'yas-insert-snippet))

;; Fix company and ido maps to have bépo-friendly bindings
(require 'company)
(define-keys company-active-map
  "C-t" #'company-select-previous-or-abort
  "C-r" #'company-select-next-or-abort)

(require 'ido)
(add-hook 'ido-setup-hook #'ido-add-bepo-bindings)
(defun ido-add-bepo-bindings ()
  (define-keys
    ido-completion-map
    (kbd "C-t") #'ido-prev-match
    (kbd "C-r") #'ido-next-match))


(provide 'bepo_map)
