(defpackage :utils
  (:use :cl :mini-fiveam)
  (:export #:remove-last-char))

(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       remove-last-char                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-last-char (string)
  (subseq string 0 (1- (length string))))

(deftest remove-last-char.1
  (is (equal "fo" (remove-last-char "foo"))))
