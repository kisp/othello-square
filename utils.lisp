(defpackage :utils
  (:use :cl :mini-fiveam)
  (:export #:remove-last-char
           #:add-event-listener
           #:ws-url
           #:open-websocket
           #:websocket-send
           #:close-websocket
           #:obj-literal
           #:fact
           #:get-bool-from-js
           #:max-via-js-inline))

(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       remove-last-char                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-last-char (string)
  (subseq string 0 (1- (length string))))

(deftest remove-last-char.1
  (is (equal "fo" (remove-last-char "foo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      add-event-listener                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-event-listener (obj event fn)
  ((jscl::oget obj "addEventListener") event fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            ws-url                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ws-url (&optional (protocol #j:document:location:protocol) (host #j:document:location:host))
  (format nil
          "~A://~A/cable"
          (ecase (intern (string-upcase (remove-last-char protocol)) "KEYWORD")
            (:http "ws")
            (:https "wss"))
          host))

(deftest ws-url.1
  (is (equal "ws://example.com:3030/cable"
             (ws-url "http:" "example.com:3030")))
  (is (equal "wss://example.com:3030/cable"
             (ws-url "https:" "example.com:3030")))
  (is (equal "wss://othello-square.fly.dev/cable"
             (ws-url "https:" "othello-square.fly.dev"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           websocket                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-websocket (url &key on-close on-error on-message on-open log-each-event)
  (let ((events '("close" "error" "message" "open"))
        (ws (jscl::make-new #j:WebSocket (jscl::lisp-to-js url))))
    (when log-each-event
      (let ((handler (lambda (event)
                       (#j:console:log event))))
        (loop for event in events
              when handler
                do (add-event-listener ws event handler))))
    (loop for event in events
          for handler in (list on-close on-error on-message on-open)
          when handler
            do (add-event-listener ws event handler))
    ws))

(defun websocket-send (ws message)
  ((jscl::oget ws "send")
   (jscl::lisp-to-js
    (if (stringp message)
        message
        (prin1-to-string message)))))

(defun close-websocket (ws)
  ((jscl::oget ws "close")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          obj-literal                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             other                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
