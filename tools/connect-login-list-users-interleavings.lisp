(in-package :cl-user)

;;; steps
(defun connect (user)
  (lambda ()
    (format t "When ~S has opened a connection to the game server~%"
            (string-downcase user))))

(defun login (user)
  (lambda ()
    (format t "When ~S logs in with her username~%"
            (string-downcase user))))

(defun get-users (user)
  (lambda ()
    (format t "When ~S asks the server to get a list of users~%"
            (string-downcase user))))

;;; before
(defun no-users-connected ()
  (format t "Given no users are connected to the game server~%"))

;;; after
(defun user-can-see-other-user (user other-user)
  (format t "Then ~S can see that ~S is there~%"
          (string-downcase user)
          (string-downcase other-user)))

(defun print-scenarios (number-of-users)
  (check-type number-of-users (member 2 3))
  (let ((users (ecase number-of-users
                 (2 '(:bob :jane))
                 (3 '(:bob :jane :peter))))
        (markers (ecase number-of-users
                   (2 '(("b1" "b2" "b3")
                        ("jA" "jB" "jC")))
                   (3 '(("b1" "b2" "b3")
                        ("jA" "jB" "jC")
                        ("pI" "pII" "pIII"))))))
    (interleavings:map-interleavings
     (lambda (x)
       (let ((xs (mapcar #'first x))
             (fns (mapcar #'second x)))
         (format t "  Scenario: ~A~%" xs)
         (format t "    ")
         (no-users-connected)
         (dolist (fn fns)
           (format t "    ")
           (funcall fn))
         (dolist (user users)
           (dolist (other-user users)
             (unless (eql user other-user)
               (format t "    ")
               (user-can-see-other-user user other-user))))
         (terpri)))
     (let ((steps (list #'connect #'login #'get-users)))
       ;; (list (mapcar #'list '("b1" "b2" "b3")
       ;;               (mapcar (lambda (x) (funcall x :bob))
       ;;                       steps))
       ;;       (mapcar #'list '("jA" "jB" "jC")
       ;;               (mapcar (lambda (x) (funcall x :jane))
       ;;                       steps)))
       (mapcar
        (lambda (user marker)
          (mapcar #'list marker
                  (mapcar (lambda (x) (funcall x user))
                          steps)))
        users markers)))))
