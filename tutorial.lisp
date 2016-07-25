(defclass person ()
  ((name :accessor person-name
         :initarg :name)
   (date-of-birth :accessor person-date-of-birth ;is the person-* accessor not kind of stupid?
        :initarg :date-of-birth)
   (address :accessor person-address
         :initarg :address)
   (registration-date :accessor person-registration-date
         :initarg :registration-date)))

(defun make-person (name date-of-birth address registration-date)
  (make-instance 'person :name name :date-of-birth date-of-birth :address address
     :registration-date registration-date))


(defclass staffmember (person)
  ((bank-details :accessor staffmember-bank-details
            :initarg :bank-details)))

(defun make-staffmember (name date-of-birth address registration-date bank-details)
  (make-instance 'staffmember :name name :date-of-birth date-of-birth :address address
     :registration-date registration-date :bank-details bank-details))

(defclass student (person)
 ((ects-points :accessor student-ects-points
           :initarg :ects-points)))

(defun make-student (name date-of-birth address registration-date ects-points)
 (make-instance 'student :name name :date-of-birth date-of-birth :address address
    :registration-date registration-date :ects-points ects-points))

(defgeneric get-info (person)
      (:documentation "Returns data on all attributes."))

(defmethod get-info ((p person))
    (format t "Name: ~d ~% Occupation: ~d ~% Date of Birth: ~d ~% Adress: ~d ~% Date of Registration: ~d ~%" (person-name p) (class-of p) (person-date-of-birth p) (person-address p) (person-registration-date p)))



;-------------------------------------

(defvar *Bill* (make-person 'bill 21 "randomstreet" 21222009))
(defvar *Johann* (make-staffmember 'johann 42 "funstreet" 21222011 "IBAN:1234"))
(defvar *Susi* (make-student 'susi 30 "funstreet" 21222011 0))

(format t "~%")
(get-info *Bill*)
(format t "~%--------------~%~%")
(get-info *Johann*)
(format t "~%--------------~%~%")
(get-info *Susi*)
(format t "~%")
