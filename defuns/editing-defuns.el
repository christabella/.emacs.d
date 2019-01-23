;; https://github.com/cjohansen/.emacs.d/blob/master/defuns/editing-defuns.el

;; kill region if active, otherwise kill backward word
(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
