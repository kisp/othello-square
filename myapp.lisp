(defpackage :myapp
  (:use :cl :m-macro :utils))

(in-package :myapp)

(defvar *ws* nil)

(defvar *nickname* nil)
(defvar *login-success* nil)
(defvar *welcome-message* nil)
(defvar *other-users* nil)
(defvar *pending-invite* nil)
(defvar *game* nil)
(defvar *game-opponent* nil)
(defvar *game-first-player* nil)

(defun send! (message)
  (websocket-send
   *ws*
   (format-message-as-json message)))

(defun handle-message (message)
  (case (car message)
    (:please-tell-me-who-you-are
     (send! `(:login ,*nickname*)))
    (:logged-in
     (setq *login-success* t
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
     (setq *game* t
           *game-opponent* (second message)
           *game-first-player* (third message)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         clog-handler                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clog-handler (message)
  (lambda (event)
    (clog::clog message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              mc                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mc (name-and-args &body body)
  (destructuring-bind (name &rest args)
      (if (consp name-and-args) name-and-args (list name-and-args))
    `(m (function ,name) (list ,@args) ,@body)))

(defmacro ms (name-and-args &body body)
  (destructuring-bind (name &rest args)
      (if (consp name-and-args) name-and-args (list name-and-args))
    `(m ,(string-downcase name) (list ,@args) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       define-component                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-component* (name args state &body body)
  `(defun ,name (initial-vnode)
     (let ,state
       (plist2object
        (list
         :view
         (lambda (vnode)
           (let ((children (jscl::oget vnode "children"))
                 ,@(mapcar
                     (lambda (arg)
                       (destructuring-bind (name default)
                           (if (consp arg) arg (list arg nil))
                         `(,name (or (jscl::oget vnode "attrs" ,(string-downcase name))
                                     ,default))))
                     args))
             ;; if children are empty, it will be #()
             (when (zerop (length children))
               (setq children nil))
             ,@body)))))))

(defmacro define-component (name args &body body)
  `(define-component* ,name ,args ()
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            counter                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component* counter ((button-text "Click me") children-fn) ((count 0))
  (m "div.text-4xl"
     (if children-fn
         (funcall children-fn count)
         (or children
             (m "div" count)))
     (m "button" (list :onclick (lambda (event) (incf count)))
        button-text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        initial-wrapper                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component initial-wrapper ()
  (ms (:div :class "bg-gray-400 min-h-screen")
      children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        main-container                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component main-container ()
  (ms (:main :class "container mx-auto px-4 pb-8")
      children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            navbar                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component navbar ()
  (ms (:header :class "text-white text-xl md:text-2xl bg-pink-700 p-2 shadow-lg flex items-center")
      (ms :h1 (ms (:a :href "/") "Othello Square"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        default-layout                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component default-layout ()
  (mc initial-wrapper
    (mc navbar)
    (mc main-container
      (ms :div
          (when *welcome-message*
            (m "div#message"
               (list :data-testid "message")
               *welcome-message*))
          (when *pending-invite*
            (m "div#game_invitation"
               (m "h2"
                  (format nil "~A invites you for a game!" *pending-invite*)
                  (m "button"
                     (list :onclick #'handle-game-invitation-accept)
                     "Accept"))))
          children))))

(defmacro define-page (name args &body body)
  `(define-component ,name ,args
     (mc default-layout
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          login-form                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component login-form ()
  (ms :div
      (ms :h2 "Please login with your nickname")
      (ms (:form :onsubmit #'handle-login-submit)
          (ms (:label :for "nickname") "Nickname")
          (ms (:input :id "nickname"))
          (ms :button "Login"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          login-page                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-page login-page ()
  (mc login-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          users-page                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-page users-page ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         game-message                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component game-message ()
  (ms (:div :id "game_message"
            :class "border-2 border-black rounded bg-gray-100 p-4 my-4 text-center md:text-xl")
      (if (equal *nickname* *game-first-player*)
          "It's your turn"
          (format nil "Waiting for ~a's turn" *game-opponent*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             square                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component empty-square (legal-move-indicator)
  (ms (:div :class "border border-black flex items-center justify-center")
      (when legal-move-indicator
        (ms (:div :class "border border-black rounded-full w-5/6 h-5/6")))))

(define-component black-square ()
  (ms (:div :class "border border-black flex items-center justify-center")
      (ms (:div :class "bp w-5/6 h-5/6"))))

(define-component white-square ()
  (ms (:div :class "border border-black flex items-center justify-center")
      (ms (:div :class "wp w-5/6 h-5/6"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             board                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component board ()
  (let* ((player othello::+black+)
         (board (othello::initial-board))
         (legal-moves (othello::legal-moves player board)))
    (ms (:div :class "border border-black bg-green-600 aspect-square grid grid-cols-8 max-h-screen")
        (apply #'js-array
               (othello::map-board-squares
                (lambda (row col square)
                  (case (othello::bref board square)
                    ;; othello::+empty+
                    (0 (mc (empty-square :legal-move-indicator (member square legal-moves))))
                    ;; othello::+black+
                    (1 (mc black-square))
                    ;; othello::+white+
                    (2 (mc white-square)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           game-page                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-page game-page ()
  (m "div#board"
     (mc game-message)
     (mc board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         page-switcher                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component page-switcher ()
  (cond
    ((not *login-success*)
     (mc login-page))
    ((not *game*)
     (mc users-page))
    (*game*
     (mc game-page))
    (t (error "don't know which page to show"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              app                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component app-aux ()
  (mc page-switcher))

(define-component app ()
  (mc app-aux))

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
