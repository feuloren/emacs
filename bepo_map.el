;;; bepo_map.el --- Keymap for a bépo keyboard
;;; Code:

(setq lexical-binding t)

;;; Commentary:
;; 

(require 'hydra)
(defhydra hydra-projectile (:exit t :hint nil)
  "
^Open^                 ^Operate^
_n_: find file         _g_: search
_t_: switch project    _s_: save all
_d_: dired             _c_: compile
"
  ("n" projectile-find-file)
  ("d" projectile-dired)
  ("g" projectile-ag)
  ("s" projectile-save-project-buffers)
  ("t" projectile-switch-project)
  ("c" projectile-compile-project))

(defhydra hydra-org (:exit t :hint nil)
  "
^Clock^    ^Subtree^     ^Insert^               ^Capture^
_i_: in    _r_: refile   _t_: timestamp         _z_
_o_: out   _a_: archive  _T_: active timestamp
"
  ("z" org-capture)
  ("w" org-capture)
  ("i" org-clock-in)
  ("o" org-agenda-clock-out)
  ("t" org-time-stamp-inactive)
  ("T" org-time-stamp)
  ("r" org-refile)
  ("a" org-archive-subtree))

(defhydra hydra-paredit (:exit t :hint nil)
  "
^Slurp^        ^Barf^         ^Splice^         ^Split^  ^Join^
_t_: forward   _r_: forward   _d_: Kill back   _|_      _j_
                              _e_: Raise
"
  ("|" paredit-split-sexp)
  ("t" paredit-forward-slurp-sexp)
  ("d" paredit-splice-sexp-killing-backward)
  ("r" paredit-forward-barf-sexp)
  ("e" paredit-raise-sexp)
  ("j" paredit-join-sexps))

(defhydra hydra-fly* (:hint nil)
 "
^Check^                ^Spell^
_i_: goto next error   _s_: goto next error
_e_: list errors       _t_: correct word
                     _d_: change dictionnary
                     _l_: toggle flyspell
                     _v_: check buffer
"
 ("s" flyspell-goto-next-error) ;; not good bindings
 ("t" ispell-word)
 ("d" ispell-change-dictionary)
 ("l" flyspell-mode)
 ("i" flycheck-next-error)
 ("e" flycheck-list-errors)
 ("v" flyspell-buffer))

(defhydra hdyra-magit (:exit t :hint nil)
  "
^Status^               ^File^
_c_: current project   _b_: blame
_t_: choose project"
  ("c" magit-status)
  ("t" ft/magit-projectile)
  ("b" magit-blame))

(defun ft/magit-projectile ()
  (interactive)
  (let ((projectile-switch-project-action #'magit-status))
    (call-interactively #'projectile-switch-project)))

(defun define-keys (map &rest keys)
  "Defines multiple keybinding in the same MAP at once.
Optional argument KEYS tr."
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
  "v" #'avy-goto-char
  "a" #'swiper
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
  ";" #'endless/comment-line
  ;; Buffers
  "," #'ido-switch-buffer
  ;; Multiple cursors
  "m" #'mc/mark-next-like-this
  "M" #'mc/skip-to-next-like-this
  "ß" #'mark-next-like-symbol-under-cursor
  "ẞ" #'skip-to-next-like-symbol-under-cursor
  ;; Projectile
  "p" #'hydra-projectile/body
  ;; Misc
  "'" #'transient-mark-mode
  "q" #'ft/god-q
  ;; Org
  "l" #'other-window
  ;; Undo-tree
  "/" #'undo-tree-undo
  "+" #'undo-tree-redo
  ;; Yasnippets
  "f" #'ft/yas-insert-snippet
  "F" #'aya-expand ; really cool but need some fixes for god-mode
		   ; (deactivate when we enter the snippet, reactivate
		   ; when we completed everything)
  "M-f" #'aya-create
  ;; Paredit
  "|" #'paredit-split-sexp
  "b" #'hydra-paredit/body
  ;; Windows
  "«" #'split-window-vertically
  "»" #'split-window-horizontally
  "$" #'delete-window
  "\"" #'delete-other-windows
  ;; Fly*
  "n" #'hydra-fly*/body

  "." #'smex
  ;; Org
  "z" #'hydra-org/body
  "w" #'hydra-org/body
  ;; Magit
  "é" #'hdyra-magit/body
)
(global-set-key (kbd "C-x C-n") #'ido-find-file)

;; Paredit requires its own commands to be called in order
;; to keep the ast valid
(require 'subr-x)
(defmacro ft/paredit-alternative (fun paredit-alternative)
  "Wrap delete commands and invoke paredit alternative when paredit mode is on."
  `(defun ,(intern (string-join (list "ft/" (symbol-name fun)))) ()
     (interactive)
     (if (bound-and-true-p paredit-mode)
	 (call-interactively ',paredit-alternative)
       (call-interactively ',fun))))

(ft/paredit-alternative delete-backward-char paredit-backward-delete)
(ft/paredit-alternative backward-kill-word paredit-backward-kill-word)
(ft/paredit-alternative kill-word paredit-forward-kill-word)
(ft/paredit-alternative kill-region paredit-kill-region)

;; I prefer to join the next line to the current line, not the way
;; emacs does it.
;; If possible this command should even join lines according to what
;; semantically makes sense
(defun join-to-next-line ()
"Join the next line to the current line."
  (interactive)
  (join-line -1))

(defun ft/god-q (arg)
  "I want q in god-mode to do the regular 'closing' action in read-only buffers.
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

(require 'yasnippet)
(require 'auto-yasnippet)
(defun ft/yas-insert-snippet ()
  "Turn off `god-mode' after inserting the skeleton of a snippet so I can type right away."
  (interactive)
  (when (bound-and-true-p god-global-mode)
    (god-mode-all)) ;; not good, we should deactivate god-mode only
  ;; when a snippet is inserted
  (call-interactively 'yas-insert-snippet))

;; Fix company and ido maps to have bépo-friendly bindings
(require 'company)
(define-keys company-active-map
  "C-t" #'company-select-previous-or-abort
  "C-r" #'company-select-next-or-abort)

(require 'ido)
(add-hook 'ido-setup-hook #'ido-add-bepo-bindings)
(defun ido-add-bepo-bindings ()
  "Fix ido keybindings.
ido keymap is dynamic so this function is called every time ido is called."
  (define-keys
    ido-completion-map
    (kbd "C-t") #'ido-prev-match
    (kbd "C-r") #'ido-next-match))

(require 'ivy)
(define-keys ivy-minibuffer-map
  "C-t" #'ivy-previous-line
  "C-r" #'ivy-next-line)

(defconst bepo-digits-replace-alist
  '(("\"" . 1)
    ("«" . 2)
    ("»" . 3)
    ("(" . 4)
    (")" . 5)
    ("@" . 6)
    ("+" . 7)
    ("-" . 8)
    ("/" . 9)
    ("*" . 0)))

(defun ft/bepo-friendly-goto-line ()
  "Not yet."
  (interactive))
;;  "On a bépo keyboard one has to hold shift to input digits.
;;This inconvenient for fast input so we'll read any char and substitute the chars produced
;;by non-shifted digit keys with their associated digit")

(provide 'bepo_map)

;;; bepo_map.el ends here
