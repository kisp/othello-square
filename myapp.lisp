(defpackage :myapp
  (:use :cl :m-macro :utils))

(in-package :myapp)

(defvar *ws* nil)

(defvar *toast-messages* nil)
(defvar *nickname* nil)
(defvar *login-success* nil)
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
     (add-toast-message (second message) :delay 1)
     (setq *login-success* t)
     (send! `(:get-list-of-users)))
    (:users-present
     (setq *other-users*
           (map 'list #'jscl::js-to-lisp (second message))))
    (:user-entered
     (add-toast-message (format nil "~A came online" (second message)))
     (push (second message) *other-users*))
    (:game-invitation-from
     (setq *pending-invite* (second message))
     (add-toast-message
      (lambda (remove-message-thunk)
        (ms (:div :class "game-invitation"
                  :data-invitator *pending-invite*)
            (ms (:h2 :class "text-xl mb-4")
                (format nil "~A invites you for a game!" *pending-invite*))
            (ms (:button :class "px-2 py-2 bg-pink-600 hover:bg-pink-700 text-white w-full"
                         :onclick (lambda (event)
                                    (funcall remove-message-thunk)
                                    (handle-game-invitation-accept event)))
                "Accept")))
      :no-auto-close t))
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
                  :log-each-event t
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
;;;                        toast messages                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-toast-message (text &optional (id (princ-to-string (random 100000))))
  (list :id id :text text))

(defun add-toast-message (text &key delay no-auto-close)
  (labels ((do-it ()
             (let* ((message-id (princ-to-string (random 100000)))
                    (remove-message-thunk (lambda () (remove-toast-message-by-id message-id)))
                    (message-text (if (functionp text)
                                      (funcall text remove-message-thunk)
                                      text))
                    (message (make-toast-message message-text message-id)))
               (setf *toast-messages*
                     (append *toast-messages* (list message)))
               (unless no-auto-close
                 (m-set-timeout 3 remove-message-thunk)))))
    (if (null delay)
        (do-it)
        (m-set-timeout delay #'do-it))))

(defun remove-toast-message-by-id (message-id)
  (setq *toast-messages*
        (remove-if (lambda (x)
                     (equal (getf x :id) message-id))
                   *toast-messages*)))

(defun remove-toast-message (message)
  (remove-toast-message-by-id (getf message :id)))

(defun toast-message-close-handler (message)
  (lambda (event)
    (remove-toast-message message)))

(defun toast-message-dom-id (message)
  (format nil "toast_~A" (getf message :id)))

(define-component toast-messages (messages)
  (ms (:DIV :CLASS "fixed right-4 top-4")
      (map 'vector
           (lambda (message)
             (mc (toast-message
                  :key (getf message :id)
                  :message message
                  :onbeforeremove
                  (lambda (vnode)
                    ((jscl::oget vnode "dom" "classList" "add") "fade-out")
                    (jscl::make-new #j:Promise
                                    (lambda (resolve)
                                      ((jscl::oget vnode "dom" "addEventListener")
                                       "animationend" resolve)))))))
           messages)))

(define-component toast-message-icon ()
  (ms (:DIV :CLASS
            "inline-flex items-center justify-center flex-shrink-0 w-8 h-8 text-pink-500 bg-pink-100 rounded-lg dark:bg-blue-800 dark:text-blue-200")
      (ms (:SVG :ARIA-HIDDEN "true" :CLASS "w-5 h-5" :FILL "currentColor" :VIEWBOX "0 0 20 20")
          (ms (:PATH :FILL-RULE "evenodd"
                     :D "M12.395 2.553a1 1 0 00-1.45-.385c-.345.23-.614.558-.822.88-.214.33-.403.713-.57 1.116-.334.804-.614 1.768-.84 2.734a31.365 31.365 0 00-.613 3.58 2.64 2.64 0 01-.945-1.067c-.328-.68-.398-1.534-.398-2.654A1 1 0 005.05 6.05 6.981 6.981 0 003 11a7 7 0 1011.95-4.95c-.592-.591-.98-.985-1.348-1.467-.363-.476-.724-1.063-1.207-2.03zM12.12 15.12A3 3 0 017 13s.879.5 2.5.5c0-1 .5-4 1.25-4.5.5 1 .786 1.293 1.371 1.879A2.99 2.99 0 0113 13a2.99 2.99 0 01-.879 2.121z"
                     :CLIP-RULE "evenodd")))
      (ms (:SPAN :CLASS "sr-only")
          "Fire icon")))

(define-component toast-message-close-button (message)
  (let ((dom-id (toast-message-dom-id message)))
    (ms (:BUTTON :TYPE "button"
                 :CLASS "ml-auto -mx-1.5 -my-1.5 bg-gray-100 text-gray-400 hover:text-gray-900 rounded-lg focus:ring-2 focus:ring-gray-300 p-1.5 hover:bg-gray-300 inline-flex h-8 w-8 dark:text-gray-500 dark:hover:text-white dark:bg-gray-800 dark:hover:bg-gray-700"
                 :DATA-DISMISS-TARGET (format nil "#~A" dom-id)
                 :ARIA-LABEL "Close"
                 :onclick (toast-message-close-handler message))
        (ms (:SPAN :CLASS "sr-only")
            "Close")
        (ms (:SVG :ARIA-HIDDEN "true" :CLASS "w-5 h-5" :FILL "currentColor" :VIEWBOX
                  "0 0 20 20")

            (ms (:PATH :FILL-RULE "evenodd" :D
                       "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                       :CLIP-RULE "evenodd"))))))

(define-component toast-message (message)
  (let ((dom-id (toast-message-dom-id message)))
    (ms (:DIV
         :ID dom-id
         :CLASS "toast-message fade-in mb-4 flex w-full max-w-xs p-4 text-black bg-gray-100 rounded-lg shadow dark:text-gray-400 dark:bg-gray-800"
         :ROLE "alert")
        (mc toast-message-icon)
        (ms (:DIV :CLASS "ml-3 text-xl font-normal")
            (getf message :text))
        (mc (toast-message-close-button :message message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        default-layout                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-component default-layout ()
  (mc initial-wrapper
      (mc navbar)
      (mc (toast-messages :messages *toast-messages*))
      (mc main-container
          (ms :div
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
