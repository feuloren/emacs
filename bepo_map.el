(setq lexical-binding t)

(defun define-keys (map &rest keys)
  (let ((saved-key))
    (dolist (key-or-fun keys)
      (cond (saved-key
	     (funcall 'define-key map (kbd saved-key) key-or-fun)
	     (setq saved-key nil))
	    (t
	     (setq saved-key key-or-fun))))))

(require 'god-mode)
(define-keys god-local-mode-map
  ;; Move
  "t" 'backward-char
  "s" 'previous-line
  "r" 'next-line
  "n" 'forward-char
  "T" 'backward-word
  "N" 'forward-word
  "S" 'beginning-of-line
  "R" 'end-of-line
  "ß" 'beginning-of-buffer
  "®" 'end-of-buffer
  "þ" 'ace-jump-mode
  "a" 'isearch-forward
  ;; Edit
  "ù" 'query-replace
  "i" 'delete-backward-char
  "e" 'backward-kill-word
  "€" 'kill-region
  "à" 'kill-ring-save
  "J" 'join-line
  ;; Buffers
  "," nil
  ", c" 'ido-switch-buffer
  ", t" 'previous-buffer
  ", n" 'next-buffer
  ;; Multiple cursors
  "v" 'mc/mark-next-like-this
  "V" 'mc/skip-to-next-like-this
  "d" 'mark-next-like-symbol-under-cursor
  "D" 'skip-to-next-like-symbol-under-cursor 
  ;; Projectile
  "p" nil
  "p n" 'projectile-find-file
  "p d" 'projectile-dired
  "p g" 'projectile-ag
  "p s" 'projectile-save-project-buffers
  "p t" 'projectile-switch-project
  ;; Misc
  "?" 'help-around-cursor
  "'" 'transient-mark-mode
  ;; Org
  "l" nil
  "l l" 'org-capture
  "l i" 'org-clock-in
  "l o" 'org-agenda-clock-out
  )
(global-set-key (kbd "C-x C-n") 'ido-find-file)

(require 'company)
(define-keys company-active-map
  "C-t" 'company-select-previous-or-abort
  "C-n" 'company-select-next-or-abort)

(provide 'bepo_map)
