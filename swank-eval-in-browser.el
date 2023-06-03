;; -*- lexical-binding: t; -*-

(defun my/slime-eval-in-browser ()
  (interactive)
  (save-some-buffers t)
  (slime-eval-async
      `(cl-user::eval-in-browser ,(slime-defun-at-point))
    (lambda (result)
      (message "==> %s" (car (read-from-string (cdr (assoc 'value result))))))))

(global-set-key (kbd "H-q") 'my/slime-eval-in-browser)
(global-set-key (kbd "M-S-<return>") 'my/slime-eval-in-browser)

(defun my/slime-eval-in-browser-and-redraw ()
  (interactive)
  (save-some-buffers t)
  (slime-eval-async
      `(cl-user::eval-in-browser ,(slime-defun-at-point) t)
    (lambda (result)
      (message "==> %s" (car (read-from-string (cdr (assoc 'value result)))))))
  (run-with-timer 0.5 nil 'my/update-css))

(global-set-key (kbd "H-p") 'my/slime-eval-in-browser-and-redraw)
(global-set-key (kbd "M-<return>") 'my/slime-eval-in-browser-and-redraw)

(defun my/update-css ()
  (with-current-buffer (find-file-noselect (format "%spublic/app.css" (projectile-acquire-root)))
    (skewer-css-eval-buffer)
    (kill-buffer)))
