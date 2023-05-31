(in-package :cl-user)

(defun eval-in-browser (string &optional redraw)
  (labels ((from-string (string)
             (swank::with-buffer-syntax ()
               (let ((*read-suppress* nil))
                 ;; (values (read-from-string string))
                 (jscl::ls-read-from-string string))))
           (emit-code (out)
             (format out "(function(values, internals) {~%")
             (format out "~A" (compile-arg))
             (format out "})(jscl.internals.pv,jscl.internals);"))
           (compile-arg ()
             (if redraw
                 (compile-form
                  `(prog1
                       (jscl::lisp-to-js
                        (prin1-to-string
                         ,(from-string string)))
                     (jscl::js-inline "window.m.redraw()"))
                  nil t)
                 (compile-form
                  `(jscl::lisp-to-js
                    (prin1-to-string
                     ,(from-string string)))
                  nil t)))
           (compile-form (form &optional multiple-value-p return-p)
             (jscl::with-compilation-environment
               (jscl::compile-toplevel form multiple-value-p return-p)))
           (eval-in-browser ()
             (let ((code
                     (with-output-to-string (out)
                       (emit-code out))))
               (print code)
               (SWANK:EVAL-IN-EMACS
                `(skewer-eval-synchronously ,code)))))
    (eval-in-browser)))

;; (eval-in-browser "(print 1)")
