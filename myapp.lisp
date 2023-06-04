(defpackage :myapp
  (:use :cl :m-macro :utils))

(in-package :myapp)

(defvar *ws* nil)

(defvar *nickname* nil)
(defvar *login-success* nil)
(defvar *welcome-message* nil)
(defvar *other-users* nil)
(defvar *pending-invite* nil)
(defvar *game-board* nil)
(defvar *game-first-player-nickname* nil)
(defvar *game-opponent-nickname* nil)
(defvar *game-current-player* nil)
(defvar *game-my-color* nil)

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
     (setq *game-board* (othello::initial-board)
           *game-opponent-nickname* (second message)
           *game-first-player-nickname* (third message)
           *game-current-player* othello::+black+
           *game-my-color* (if (equal *game-first-player-nickname* *nickname*)
                               othello::+black+
                               othello::+white+)))
    (:move-to
     (destructuring-bind (square) (cdr message)
       (handle-opponent-move square)))
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

(defun move-to-handler (square)
  (lambda (event)
    ((jscl::oget event "preventDefault"))
    (when (othello::legal-p square *game-current-player* *game-board*)
      (othello::make-move square *game-current-player* *game-board*)
      (setf *game-current-player* (othello::opponent *game-current-player*))
      (send! `(:move-to ,square)))))

(defun handle-opponent-move (square)
  (when (othello::legal-p square *game-current-player* *game-board*)
    (othello::make-move square *game-current-player* *game-board*)
    (setf *game-current-player* (othello::opponent *game-current-player*))))

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
  (ms (:main :class "container mx-auto px-4 pt-14")
      children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            navbar                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component navbar ()
  (ms (:header :class "fixed w-full text-white text-xl md:text-2xl bg-pink-700 p-2 shadow-lg flex items-center")
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
  ;; (ms (:div :class "flex justify-center mt-[23vh]")
  ;;     (ms (:div
  ;;          :class "border-2 border-black rounded bg-gray-100 p-4 my-4 text-center md:text-xl")
  ;;         (ms :h2 "Please login")
  ;;         (ms (:form :onsubmit #'handle-login-submit)
  ;;             (ms (:label :for "nickname") "Nickname")
  ;;             (ms (:input :id "nickname"))
  ;;             (ms :button "Login"))))
  ;; (ms (:div :class "bg-white p-12")
  ;;     (ms (:h2 :class "text-base font-semibold leading-7 text-gray-900")
  ;;         "Please login")
  ;;     (ms (:div :class "mt-6 grid grid-cols-1 gap-x-6 gap-y-8 sm:grid-cols-6")
  ;;         (ms (:div :class "col-span-full")
  ;;             (ms (:label :class "block text-sm font-medium leading-6 text-gray-900")
  ;;                 "Nickname")
  ;;             (ms (:div :class "mt-2")
  ;;                 (ms (:input :class "block outline-none w-full rounded-md border-0 py-1.5 px-2 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-pink-600 sm:text-sm sm:leading-6"))))
  ;;         (ms (:div :class "mt-6 flex items-center justify-end gap-x-6")
  ;;             (ms (:button :class "cursor-pointer rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600")
  ;;                 "Login"))))
  (ms (:div :class "flex justify-center mt-[20vh]")
      (ms (:div :class "border-2 border-black rounded bg-gray-100 p-4 my-4 md:text-xl")
          (ms (:h2 :class "text-base font-semibold leading-7 text-gray-900 text-center")
              "Please login")
          (ms (:form :onsubmit #'handle-login-submit)
              (ms (:div :class "mt-6 grid grid-cols-1 gap-x-6 gap-y-8 sm:grid-cols-6")
                  (ms (:div :class "col-span-full")
                      (ms (:label
                           :for "nickname"
                           :class "block text-sm font-medium leading-6 text-gray-900 text-center")
                          "Nickname")
                      (ms (:div :class "mt-2")
                          (ms (:input
                               :id "nickname"
                               :class "block outline-none w-full rounded-md border-0 py-1.5 px-2 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-pink-600 sm:text-sm sm:leading-6"))))
                  (ms (:div :class "mt-2 flex items-center justify-center gap-x-6")
                      (ms (:button :class "cursor-pointer rounded-md bg-pink-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-pink-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-pink-600")
                          "Login")))))))

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
      (if (eql *game-current-player* *game-my-color*)
          "It's your turn"
          (format nil "Waiting for ~a's turn" *game-opponent-nickname*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             square                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component square-container (square)
  (ms (:button
       :id (format nil "square_~A" square)
       :class "border border-black flex items-center justify-center"
       :onclick (move-to-handler square))
      children))

(define-component empty-square (square legal-move-indicator)
  (mc (square-container :square square)
    (if legal-move-indicator
        (ms (:div :class "lm border border-black rounded-full w-5/6 h-5/6"))
        (ms (:div :class "ee")))))

(define-component black-square (square)
  (mc (square-container :square square)
    (ms (:div :class "bp w-5/6 h-5/6"))))

(define-component white-square (square)
  (mc (square-container :square square)
    (ms (:div :class "wp w-5/6 h-5/6"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             board                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component board ()
  (let ((legal-moves (othello::legal-moves *game-current-player* *game-board*)))
    (ms (:div :class "border border-black bg-green-600 aspect-square grid grid-cols-8 max-h-screen")
        (apply #'js-array
               (othello::map-board-squares
                (lambda (row col square)
                  (case (othello::bref *game-board* square)
                    ;; othello::+empty+
                    (0 (mc (empty-square
                            :square square
                            :legal-move-indicator
                            (and (eql *game-current-player*
                                      *game-my-color*)
                                 (member square legal-moves)))))
                    ;; othello::+black+
                    (1 (mc (black-square :square square)))
                    ;; othello::+white+
                    (2 (mc (white-square :square square))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           game-page                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-page game-page ()
  (m "div#board"
     (list :data-pieces-balance
           (format nil "~A/~A"
                   (count othello::+black+ *game-board*)
                   (count othello::+white+ *game-board*)))
     (mc game-message)
     (mc board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         page-switcher                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component page-switcher ()
  (cond
    ((not *login-success*)
     (mc login-page))
    ((not *game-board*)
     (mc users-page))
    (*game-board*
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
