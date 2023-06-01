(defpackage :myapp
  (:use :cl :m-macro :utils))

(in-package :myapp)

(defvar *ws* nil)

(defvar *nickname* nil)
(defvar *welcome-message* nil)
(defvar *other-users* nil)

(defun format-message-as-json (message)
  (#j:JSON:stringify
   (apply
    #'jscl::make-new
    #j:Array
    (jscl::lisp-to-js
     (substitute #\_ #\-
                 (string-downcase (car message))))
    (mapcar #'jscl::lisp-to-js (cdr message)))))

(defun handle-message (message)
  (case (car message)
    (:please-tell-me-who-you-are
     (websocket-send
      *ws*
      (format-message-as-json `(:login ,*nickname*))))
    (:logged-in
     (setq *nickname* (get-nickname-from-input-field)
           *welcome-message* (second message))
     (websocket-send
      *ws*
      (format-message-as-json `(:get-list-of-users))))
    (:users-present
     (setq *other-users*
           (map 'list #'jscl::js-to-lisp (second message))))
    (:user-entered
     (push (second message) *other-users*))
    (t
     (error "Don't know how to handle-message: ~S"
            message)))
  (m-redraw))

(defun transform-message-head (message)
  (when message
    (cons (intern (substitute #\- #\_
                              (string-upcase (car message)))
                  "KEYWORD")
          (cdr message))))

(defun parsed-json-message-to-lisp (json)
  (let ((array (jscl::js-to-lisp json)))
    (assert (arrayp array))
    (transform-message-head
     (map 'list #'jscl::js-to-lisp array))))

(defun open-websocket-with-handlers ()
  (open-websocket (ws-url)
                  :log-each-event nil
                  :on-message
                  (lambda (event)
                    (handle-message
                     (parsed-json-message-to-lisp
                      (#j:JSON:parse
                       (jscl::oget event "data")))))))

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

(defun m-redraw ()
  (funcall (jscl::js-inline "m.redraw")))

(defun get-nickname-from-input-field ()
  (let* ((elt (jscl::js-inline "document.getElementById('nickname')"))
         (nickname (jscl::oget elt "value")))
    nickname))

(defun handle-login-submit (event)
  ((jscl::oget event "preventDefault"))
  (let ((nickname (get-nickname-from-input-field)))
    (unless (or (equal nickname "")
                (every (lambda (char) (char= char #\space)) nickname))
      (setq *nickname* nickname)
      (setq *ws* (open-websocket-with-handlers)))))

(defun app-aux (&rest args)
  (plist2object
   (list
    :view
    (lambda (&rest args)
      (m "div"
         (m "h1" (m "a" (list :href "/") "Othello Square"))
         (unless *welcome-message*
           (jscl::make-new #j:Array
                           (m "h2" "Please login with your nickname")
                           (m "form"
                              (list :onsubmit #'handle-login-submit)
                              (m "label" (list :for "nickname") "Nickname")
                              (m "input" (list :id "nickname"))
                              (m "button" "Login"))))
         (when *nickname*
           (m "div#message"
              (list :data-testid "message")
              ;; (format nil "Welcome, ~A!" *nickname*)
              *welcome-message*))
         (when *other-users*
           (m "div"
              (m "h2" "Online users")
              (apply #'m
                     "ol"
                     (mapcar (lambda (user)
                               (m "li"
                                  (list :id (format nil "user_~A" user))
                                  user))
                             *other-users*)))))))))

(defun app ()
  (plist2object
   (list
    :view
    (lambda (&rest args)
      (m (symbol-function 'app-aux))))))

(defun mount-app ()
  (let ((elt (jscl::js-inline "document.getElementById('app')")))
    ;; (jscl::oset "foo" elt "innerHTML")
    (m-mount elt (app))))

(defun exports-for-js ()
  (#j:console:log "exports-for-js called")
  (obj-literal
   :|runAllTestsJest| #'mini-fiveam:run-all-tests-jest
   :|mountApp| #'mount-app
   :fact #'fact
   :|getBoolFromJs| #'get-bool-from-js
   :|maxViaJsInline| #'max-via-js-inline))
