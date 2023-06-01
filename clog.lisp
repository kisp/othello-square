(defpackage :clog
  (:use :cl)
  (:export #:clog
           #:clog-lisp
           #:cwarn
           #:cwarn-lisp
           #:cerror
           #:cerror-lisp))

(in-package :clog)

(defun clog (&rest args)
  (apply #j:console:log args))

(defun clog-lisp (&rest args)
  (apply #'clog (mapcar #'prin1-to-string args)))

(defun cwarn (&rest args)
  (apply #j:console:warn args))

(defun cwarn-lisp (&rest args)
  (apply #'cwarn (mapcar #'prin1-to-string args)))

(defun cerror (&rest args)
  (apply #j:console:error args))

(defun cerror-lisp (&rest args)
  (apply #'cerror (mapcar #'prin1-to-string args)))
