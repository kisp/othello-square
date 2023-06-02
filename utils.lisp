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
           #:max-via-js-inline
           #:format-message-as-json
           #:parse-json-message
           #:plist2object
           #:m
           #:m-render
           #:m-mount
           #:m-redraw
           #:js-array
           #:js-array*
           #:range))

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

(deftest obj-literal.1
  (is (equal "{\"foo\":1,\"bar\":2}" (#j:JSON:stringify (obj-literal :foo 1 :bar 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         plist2object                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plist2object (plist)
  (let ((obj (jscl::new)))
    (loop
      for (key value) on plist by #'cddr
      do (jscl::oset value obj (string-downcase key)))
    obj))

(deftest plist2object.1
  (is (equal "{\"foo\":1,\"bar\":2}" (#j:JSON:stringify (plist2object '(:foo 1 :bar 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           js-array                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun js-array (&rest args)
  (apply #'jscl::make-new #j:Array args))

(defmacro js-array* (&rest args)
  `(jscl::make-new #j:Array ,@args))

(deftest js-array.1
  (is (equal "[1,2,3]" (#j:JSON:stringify (js-array 1 2 3))))
  (is (equal "[1,2,3]" (#j:JSON:stringify (js-array* 1 2 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    format-message-as-json                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-message-as-json (message)
  (#j:JSON:stringify
   (apply
    #'js-array
    (jscl::lisp-to-js
     (substitute #\_ #\-
                 (string-downcase (car message))))
    (mapcar #'jscl::lisp-to-js (cdr message)))))

(deftest format-message-as-json.1
  (is (equal "[\"foo\",\"bar\"]" (format-message-as-json '(:foo "bar"))))
  (is (equal "[\"foo_bar\",123]" (format-message-as-json '(:foo-bar 123))))
  (is (equal "[\"foo\",1,2,3]" (format-message-as-json '(:foo 1 2 3))))
  (is (equal "[\"foo\"]" (format-message-as-json '(:foo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  parse-json-message                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform-message-head (message)
  (when message
    (cons (intern (substitute #\- #\_
                              (string-upcase (car message)))
                  "KEYWORD")
          (cdr message))))

(defun parse-json-message (string)
  (let ((array (jscl::js-to-lisp (#j:JSON:parse string))))
    (assert (arrayp array))
    (transform-message-head
     (map 'list #'jscl::js-to-lisp array))))

(deftest parse-json-message.1
  (labels ((roundtrip (x)
             (parse-json-message
              (format-message-as-json x))))
    (dolist (message '((:foo "bar")
                       (:foo-bar 123)
                       (:foo 1 2 3)
                       (:foo)))
      (is (equal message (roundtrip message))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            mithril                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             range                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (from to)
  (loop for i from from to to collect i))

(deftest range.1
  (is (equal '(1 2 3) (range 1 3)))
  (is (equal '() (range 10 1))))

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
