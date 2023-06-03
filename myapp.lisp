(defpackage :myapp
  (:use :cl :m-macro :utils))

(in-package :myapp)

(defvar *ws* nil)

(defvar *nickname* nil)
(defvar *welcome-message* nil)
(defvar *other-users* nil)
(defvar *pending-invite* nil)
(defvar *game* nil)

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
    (:game-invitation-from
     (setq *pending-invite* (second message)))
    (:game-start-with
     ;; TODO: (second message) is the other user
     (setq *game* t))
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

(defun invite-for-game-handler (invitee)
  (lambda (event)
    ((jscl::oget event "preventDefault"))
    (send! `(:invite-for-game ,invitee))))

(defun handle-game-invitation-accept (event)
  ((jscl::oget event "preventDefault"))
  (send! `(:accept-game-invitation ,*pending-invite*)))

(defun app-aux (&rest args)
  (plist2object
   (list
    :view
    (lambda (&rest args)
      (m "div"
         (m "h1" (list :class "text-2xl text-pink-500")
            (m "a" (list :href "/") "Othello Square"))
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
                                  user
                                  (m "button"
                                     (list :onclick (invite-for-game-handler user))
                                     "Invite for game")))
                             *other-users*))))
         (when *pending-invite*
           (m "div#game_invitation"
              (m "h2"
                 (format nil "~A invites you for a game!" *pending-invite*)
                 (m "button"
                    (list :onclick #'handle-game-invitation-accept)
                    "Accept"))))
         (when *game*
           (m "div#board"
              "Game board")))))))

(defun app ()
  (plist2object
   (list
    :view
    (lambda (&rest args)
      (m (symbol-function 'app-aux))))))

(defvar *app-mounted* nil)

(defun mount-app ()
  ;; you can run this from the browser console:
  ;; import("./myapp.js").then(x => x.default.exports_for_js().mountApp())
  (when *app-mounted*
    (unmount-app))
  (let ((elt (jscl::js-inline "document.getElementById('app')")))
    (m-mount elt #'app)
    (setq *app-mounted* t)))

(defun unmount-app ()
  (let ((elt (jscl::js-inline "document.getElementById('app')")))
    (m-mount elt (jscl::js-inline "null"))
    (jscl::oset "" elt "innerHTML")))

(defun exports-for-js ()
  (#j:console:log "exports-for-js called")
  (obj-literal
   :|runAllTestsJest| #'mini-fiveam:run-all-tests-jest
   :|mountApp| #'mount-app
   :|unmountApp| #'unmount-app
   :fact #'fact
   :|getBoolFromJs| #'get-bool-from-js
   :|maxViaJsInline| #'max-via-js-inline))
