(defpackage :mini-fiveam
  (:use :cl :clog)
  (:export #:deftest
           #:is
           #:run-all-tests
           #:run-all-tests-jest))

(in-package :mini-fiveam)

(defvar *test-names* nil)
(defvar *test-lambdas* (make-hash-table))

(defmacro deftest (name &body body)
  `(progn
     (push ',name *test-names*)
     (setf (gethash ',name *test-lambdas*)
           (lambda (pass)
             ,@body))))

(defmacro is (form)
  (unless (consp form)
    (error "Expected cons: ~S" form))
  (destructuring-bind (eql expected expr)
      form
    `(let ((result ,expr)
           (expected-value ,expected))
       (if (,eql expected-value result)
           (funcall pass)
           (let ((message
                   (format nil "Expected ~S to eval to ~S, but it was ~S"
                           ',expr ',expected result)))
             (throw 'failure message))))))

(defun run-all-tests ()
  (let ((failures)
        (checks 0)
        (pass 0)
        (fail 0)
        (erred 0))
    (labels ((pass ()
               (incf checks)
               (incf pass))
             (fail (test reason)
               (incf checks)
               (incf fail)
               (push (list :failure test reason) failures))
             (error* (test condition)
               (incf checks)
               (incf erred)
               (push (list :error test condition) failures)))
      (dolist (test-name (reverse *test-names*))
        (let ((test-report-message
                (with-output-to-string (report-message)
                  (format report-message "~A: " test-name)
                  (flet ((pass ()
                           (format report-message ".")
                           (pass))
                         (fail (test reason)
                           (format report-message "f")
                           (fail test reason))
                         (error* (test condition)
                           (format report-message "X")
                           (error* test condition)))
                    (handler-case
                        (let (finished)
                          (let ((reason
                                  (catch 'failure
                                    (funcall (gethash test-name *test-lambdas*) #'pass)
                                    (setq finished t))))
                            (unless finished
                              (fail test-name reason))))
                      (error (c)
                        (error* test-name c)))))))
          (clog test-report-message)))
      (clog (format nil "Did ~A checks." checks))
      (clog (format nil "Pass: ~A, Fail: ~A, Erred: ~A" pass fail erred))
      (dolist (failure (reverse failures))
        (destructuring-bind (kind test-name reason) failure
          (funcall (ecase kind (:failure #'cwarn) (:error #'cerror))
                   (format nil "~A in ~A:"
                           (ecase kind (:failure "Failure") (:error "Error"))
                           test-name))
          (ecase kind
            (:failure (clog (format nil "~A" reason)))
            (:error
             (let* ((condition reason)
                    (message
                      (typecase condition
                        (simple-error
                         (format nil
                                 (simple-condition-format-control condition)
                                 (simple-condition-format-arguments condition)))
                        (type-error
                         (format nil
                                 "Type error. ~a does not designate a ~a"
                                 (type-error-datum condition)
                                 (type-error-expected-type condition))))))
               (clog message))))))
      (values (and (zerop fail) (zerop erred))
              checks pass fail erred
              failures))))

(defun run-all-tests-jest ()
  (multiple-value-bind (success
                        checks pass fail erred
                        failures)
      (run-all-tests)
    (jscl::make-new
     #j:Array
     (jscl::lisp-to-js
      success)
     (jscl::lisp-to-js
      (format nil "Did ~A checks.~%Pass: ~A, Fail: ~A, Erred: ~A"
              checks
              pass fail erred)))))
