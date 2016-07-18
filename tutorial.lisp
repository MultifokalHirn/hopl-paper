(defclass person ()
  ((name :accessor person-name
         :initform 'susi
         :initarg :name)
   (age :accessor person-age
        :initform 42
        :initarg :age)))

(defun make-person (name age)
  (make-instance 'person :name name :age age))


(defclass teacher (person)
  ((subject :accessor teacher-subject
            :initarg :subject)))

(defun make-teacher (name age subject)
  (make-instance 'teacher :name name :age age :subject subject))


(defclass athlete (person)
  ((sport :accessor athlete-sport
            :initarg :sport)))

(defun make-athlete (name age sport)
  (make-instance 'athlete :name name :age age :sport sport))


(defclass athletic-teacher (teacher athlete) () )

(defun make-athletic-teacher (name age sport subject)
  (make-instance 'athletic-teacher :name name :age age :sport sport :subject subject))


(defgeneric who-am-i (person)
      (:documentation "Makes the person describe themselves."))

(defmethod who-am-i ((p person))
    (format t "My name is ~d and I am ~d years old.~%" (person-name p) (person-age p)))

(defmethod who-am-i ((p teacher))
    (format t "My name is ~d and I am ~d years old.~%" (person-name p) (person-age p))
    (format t "I am a teacher and my subject is ~d.~%" (teacher-subject p)))

(defmethod who-am-i :after ((p teacher))
    (format t "I am a teacher and my subject is ~d.~%" (teacher-subject p)))

(defmethod who-am-i :after ((p athlete))
    (format t "I am an athlete and my sport is ~d.~%" (athlete-sport p)))

(defmethod who-am-i :around ((p athletic-teacher))
    (format t "Oh, hi!~%" )
    (let ((result (call-next-method)))
      (format t "Gotta sprint back to class, bye-bye!~%")
      result))

;-------------------------------------

(defvar *Bill* (make-person 'bill 21))
(defvar *Johann* (make-teacher 'johann 42 'maths))
(defvar *Susi* (make-athletic-teacher 'susi 30 'tennis 'geography))

(format t "~%")
(who-am-i *Bill*)
(format t "~%--------------~%~%")
(who-am-i *Johann*)
(format t "~%--------------~%~%")
(who-am-i *Susi*)
(format t "~%")
