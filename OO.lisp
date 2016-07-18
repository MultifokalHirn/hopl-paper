
(defun send-message (obj message &rest args)
  (apply (gethash message obj) args))

(defparameter obj (make-hash-table))

(setf (gethash 'attribute obj) "myAttribute")

(setf (gethash 'method1 obj) (lambda (self) (format t "A method has been called.")))

(setf (gethash 'method2 obj) (lambda (self) (format t "Look, I have an attribute with the value ~d!" (gethash 'attribute self))))

;-------------------------------------

(format t "Reading attribute:~%")
(format t (gethash 'attribute obj))
(format t "~%~%Calling method1:~%")
(send-message obj 'method1 obj)
(format t "~%~%Calling method2:~%")
(send-message obj 'method2 obj)
