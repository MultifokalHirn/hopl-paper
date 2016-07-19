(defclass person ()
  ((name :accessor person-name
         :initarg :name)
   (age :accessor person-age ;is the person-* accessor not kind of stupid?
        :initarg :age)
   (address :accessor person-address
         :initarg :address)
   (registration-date :accessor person-registration-date
         :initarg :registration-date)))

(defun make-person (name age adress registration-date)
  (make-instance 'person :name name :age age :address address
     :registration-date registration-date))


(defclass staffmember (person)
  ((bank-details :accessor staffmember-bank-details
            :initarg :bank-details)))

(defun make-staffmember (name age subject adress registration-date bank-details)
  (make-instance 'staffmember :name name :age age :address address
     :registration-date registration-date :bank-details bank-details))

(defclass student (person)
 ((ects-points :accessor student-ects-points
           :initarg :ects-points)))

(defun make-student (name age subject adress registration-date ects-points)
 (make-instance 'student :name name :age age :address address
    :registration-date registration-date :ects-points ects-points))

(defgeneric who-am-i (person)
      (:documentation "Returns data on all attributes."))

(defmethod who-am-i ((p person))
    (format t "My name is ~d and I am ~d years old.~%" (person-name p) (person-age p)))


(defmethod who-am-i ((p teacher))
    (format t "My name is ~d and I am ~d years old.~%" (person-name p) (person-age p))
    (format t "I am a teacher and my subject is ~d.~%" (teacher-subject p)))

(defmethod who-am-i :around ((p athletic-teacher))
    (format t "Oh, hi!~%" )
    (let ((result (call-next-method)))
      (format t "Gotta sprint back to class, bye-bye!~%")
      result))

;-------------------------------------

(defvar *Bill* (make-person 'bill 21 "randomstreet" ))
(defvar *Johann* (make-staffmember 'johann 42 "funstreet" ))
(defvar *Susi* (make-athletic-teacher 'susi 30 'tennis 'geography))

(format t "~%")
(who-am-i *Bill*)
(format t "~%--------------~%~%")
(who-am-i *Johann*)
(format t "~%--------------~%~%")
(who-am-i *Susi*)
(format t "~%")
