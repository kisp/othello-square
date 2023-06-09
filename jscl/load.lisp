(in-package :cl-user)

(load "jscl.lisp")

(defun compile-application ()
  (jscl:compile-application
   (list "../clog.lisp"
         "../mini-fiveam.lisp"
         "../utils.lisp"
         "../othello.lisp"
         "../m-macro.lisp"
         "../myapp.lisp")
   "../public/myapp.js"))

(define-symbol-macro cc (compile-application))

(defun bootstrap-and-compile-application ()
  (jscl:bootstrap)
  (compile-application))

#+swank
(bootstrap-and-compile-application)

#+swank
(load "../swank-eval-in-browser.lisp")

#+swank
(swank:eval-in-emacs '(load (format "%sswank-eval-in-browser.el" (projectile-acquire-root))))

#+swank
(swank:eval-in-emacs '(find-file-noselect (format "%spublic/app.js" (projectile-acquire-root))))

#+swank
(swank:eval-in-emacs '(let ((httpd-host "0.0.0.0")) (httpd-start)))
