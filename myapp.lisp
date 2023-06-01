(defpackage :myapp
  (:use :cl :m-macro :utils))

(in-package :myapp)

(defvar *ws* nil)

(defvar *nickname* nil)
(defvar *welcome-message* nil)
(defvar *other-users* nil)

(defun send! (message)
  (websocket-send
   *ws*
   (format-message-as-json message)))

(defun handle-message (message)
  (case (car message)
    (:please-tell-me-who-you-are
     (send! `(:login ,*nickname*)))
    (:logged-in
     (setq *nickname* (get-nickname-from-input-field)
           *welcome-message* (second message))
     (send! `(:get-list-of-users)))
    (:users-present
     (setq *other-users*
           (map 'list #'jscl::js-to-lisp (second message))))
    (:user-entered
     (push (second message) *other-users*))
    (t
     (error "Don't know how to handle-message: ~S"
            message)))
  (m-redraw))

(defun open-websocket-with-handlers ()
  (open-websocket (ws-url)
                  :log-each-event nil
                  :on-message
                  (lambda (event)
                    (handle-message
                     (parse-json-message
                      (jscl::oget event "data"))))))

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
           (js-array*
            (m "h2" "Please login with your nickname")
            (m "form"
               (list :onsubmit #'handle-login-submit)
               (m "label" (list :for "nickname") "Nickname")
               (m "input" (list :id "nickname"))
               (m "button" "Login"))))
         (when *nickname*
           (m "div#message"
              (list :data-testid "message")
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
    (m-mount elt (app))))

(defun exports-for-js ()
  (#j:console:log "exports-for-js called")
  (obj-literal
   :|runAllTestsJest| #'mini-fiveam:run-all-tests-jest
   :|mountApp| #'mount-app
   :fact #'fact
   :|getBoolFromJs| #'get-bool-from-js
   :|maxViaJsInline| #'max-via-js-inline))
