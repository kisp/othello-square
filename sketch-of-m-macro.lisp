(defun transform-attribute-value (x)
  (cond
    ((stringp x) x)
    ((atom x) (princ-to-string x))
    (t x)))

(defun parse (x)
  (let ((tag (pop x))
        attributes
        rest)
    (loop
      with r = x
      for (key value) on x by #'cddr
      while (keywordp key)
      do (push key attributes)
      do (push (transform-attribute-value value)
               attributes)
      do (progn (pop r) (pop r))
      finally (setq rest r))
    (list* tag
           (nreverse attributes)
           rest)))

(testing "parse"
  (ok (equal '(:p nil "foo")
             (parse '(:p "foo"))))
  (ok (equal '(:p nil "foo" "bar")
             (parse '(:p "foo" "bar"))))
  (ok (equal '(:p (:id "5"))
             (parse '(:p :id 5))))
  (ok (equal '(:p (:id "5" :class "foo"))
             (parse '(:p :id 5 :class "foo")))))

(defun trans (x)
  (cond ((and (consp x) (keywordp (car x)))
         (destructuring-bind (tag attributes &rest args)
             (parse x)
           `(m ,(string-downcase tag)
               ,@(when attributes
                   `((list ,@attributes)))
               ,@(mapcar #'trans args))))
        ((consp x) x)
        ((stringp x) x)
        (t (princ-to-string x))))

(testing "trans"
  (ok (equal '(m "p" "foo")
             (trans '(:p "foo"))))
  (ok (equal '(m "p" "foo" "bar")
             (trans '(:p "foo" "bar"))))
  (ok (equal '(m
               "p"
               (list :class "cool")
               "hello")
             (trans '(:p
                      :class "cool"
                      "hello")))))

(testing "trans 2"
  (ok (equal '(m "div"
               (m "p" "foo")
               (m "p" "bar"))
             (trans '(:div
                      (:p "foo")
                      (:p "bar")))))
  (ok (equal '(m "main"
               (m "div" "foo"
                (m "p" "bar")))
             (trans '(:main
                      (:div "foo"
                       (:p "bar")))))))

(testing "trans 3"
  (ok (equal '(m "div"
               (mybutton 1 2 3))
             (trans '(:div
                      (mybutton 1 2 3))))))

(testing "trans 4"
  (ok (equal '(m "div" "1")
             (trans '(:div 1))))
  (ok (equal '(m "div" (list :id "5"))
             (trans '(:div :id 5))))
  (ok (equal '(m "div" (list :id (uuid)))
             (trans '(:div :id (uuid))))))
