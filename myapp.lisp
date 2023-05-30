(defpackage :myapp
  (:use :cl :m-macro))

(in-package :myapp)

(defvar *nickname* nil)

(defun plist2object (plist)
  (let ((obj (jscl::new)))
    (loop
      for (key value) on plist by #'cddr
      do (jscl::oset value obj (string-downcase key)))
    obj))

(defun m (&rest args)
  (let ((n (length args)))
    (cond
      ((<= n 1)
       (apply (jscl::js-inline "m") args))
      ((listp (second args))
       (destructuring-bind (tag attributes &rest rest) args
         (apply (jscl::js-inline "m")
                tag
                (plist2object attributes)
                rest)))
      (t
       (apply (jscl::js-inline "m") args)))))

(defun m-render (elt hs)
  (funcall (jscl::js-inline "m.render") elt hs))

(defun m-mount (elt component)
  (funcall (jscl::js-inline "m.mount") elt component))

(defun app ()
  (plist2object
   (list
    :view
    (lambda (&rest args)
      (m "div"
         (m "h1" "Othello Square")
         (m "h2" "Please login with your nickname")
         (m "label" (list :for "nickname") "Nickname")
         (m "input" (list :id "nickname"))
         (m "button"
            (list
             :onclick
             (lambda (event)
               (let ((elt (jscl::js-inline "document.getElementById('nickname')")))
                 (setq *nickname* (jscl::oget elt "value")))))
            "Login")
         (m "div#message" (format nil "Welcome, ~A!" *nickname*)))))))

(defun myinit ()
  (let ((elt (jscl::js-inline "document.getElementById('app')")))
    ;; (jscl::oset "foo" elt "innerHTML")
    (m-mount elt (app))))

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun get-bool-from-js (x)
  (ecase x
    (1 (if (jscl::js-inline "true")
           1
           2))
    (2 (if (jscl::js-inline "false")
           1
           2))))

(defun max-via-js-inline (a b)
  (let ((js-func (jscl::js-inline "Math.max")))
    (funcall js-func a b)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun transform-case (string)
    (cond
      ((notany #'lower-case-p string)
       (string-downcase string))
      ((notany #'upper-case-p string)
       (string-upcase string))
      (t string))))

(defmacro obj-literal (&rest plist)
  (let ((obj (gensym "OBJ")))
    `(let ((,obj (jscl::new)))
       ,@(loop
           for (key value) on plist by #'cddr
           collect `(jscl::oset ,value ,obj ,(transform-case (string key))))
       ,obj)))

(defun exports-for-js ()
  (#j:console:log "exports-for-js called")
  (obj-literal
   :myinit #'myinit
   :fact #'fact
   :|getBoolFromJs| #'get-bool-from-js
   :|maxViaJsInline| #'max-via-js-inline))
