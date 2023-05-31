;; -*- lexical-binding: t; -*-

(defun paul/slime-eval-in-browser ()
  (interactive)
  (slime-eval-async
      `(cl-user::eval-in-browser ,(slime-defun-at-point))
    (lambda (result)
      (message "==> %s" (car (read-from-string (cdr (assoc 'value result))))))))

(global-set-key (kbd "H-q") 'paul/slime-eval-in-browser)

(defun paul/slime-eval-in-browser-and-redraw ()
  (interactive)
  (slime-eval-async
      `(cl-user::eval-in-browser ,(slime-defun-at-point) t)
    (lambda (result)
      (message "==> %s" (car (read-from-string (cdr (assoc 'value result))))))))

(global-set-key (kbd "H-p") 'paul/slime-eval-in-browser-and-redraw)
(global-set-key (kbd "M-<return>") 'paul/slime-eval-in-browser-and-redraw)
