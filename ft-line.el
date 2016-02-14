(require 'powerline)

(defface ft/god-mode-active-face '((t (:background "DarkOliveGreen3" :foreground "black" :inherit mode-line)))
  "Face used when god mode is active and in current buffer."
  :group 'powerline)

(defface ft/god-mode-inactive-face '((t (:background "grey22" :inherit mode-line)))
  "Face used when god mode is inactive or in non-current buffer."
  :group 'powerline)

(defun ft/powerline-theme ()
  "Setup Florent's custom modeline."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (god-face (if (and god-local-mode active) 'ft/god-mode-active-face 'ft/god-mode-inactive-face))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%4l" god-face 'l)
				     (powerline-raw ":" god-face 'l)
				     (powerline-raw "%3c" god-face 'r)
                                     (powerline-raw "" nil)
                                     (funcall separator-left god-face nil)
                                     (powerline-buffer-id nil 'l)
                                     (powerline-raw "%*" nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-raw " |" nil)
                                     (powerline-major-mode nil 'l)
                                     (powerline-raw " " nil)
                                     (funcall separator-left mode-line face1)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs 'nil))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 0)))))))

(provide 'ft-line)
