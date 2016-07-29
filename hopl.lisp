;;;----------------------------------------
;;;The Framework as described in Section 4
;;;-------------------------------------

(defclass person ()
  ((name :accessor name
         :initarg :name)
   (age :accessor age
        :initarg :age)
   (address :accessor address
         :initarg :address)
   (registration-date :accessor registration-date
         :initarg :registration-date)))


(defclass student (person)
 ((ects-points :accessor ects-points
           :initarg :ects-points)))

(defun make-student (name age address registration-date ects-points)
 (make-instance 'student :name name :age age :address address
    :registration-date registration-date :ects-points ects-points))


(defclass staffmember (person)
  ((bank-details :accessor bank-details
            :initarg :bank-details)))


(defclass professor (staffmember)
  ((chair :accessor chair
            :initarg :chair)))

(defun make-professor (name age address registration-date bank-details chair)
  (make-instance 'professor :name name :age age :address address
    :registration-date registration-date :bank-details bank-details :chair chair))

(defclass chairman (staffmember)
  ((share :accessor share
            :initarg :share)))

(defun make-chairman (name age address registration-date bank-details share)
  (make-instance 'chairman :name name :age age :address address
    :registration-date registration-date :bank-details bank-details :share share))

(defclass professor-chairman (professor chairman) () )

(defun make-professor-chairman (name age address registration-date bank-details chair share)
  (make-instance 'professor-chairman :name name :age age :address address
    :registration-date registration-date :bank-details bank-details :chair chair :share share))


(defgeneric get-info (person)
      (:documentation "Returns data on all attributes."))

(defmethod get-info ((p person))
    (format t "Name: ~d ~% Occupation: ~d ~% Age: ~d ~% Adress: ~d ~% Date of Registration: ~d ~% "
         (name p) (class-of p) (age p) (address p) (registration-date p)))

(defmethod get-info :after ((s student))
    (format t "ECTS Points: ~d ~% " (ects-points s)))

(defmethod get-info :after ((s staffmember))
    (format t "Bank Details: ~d ~% " (bank-details s)))

(defmethod get-info :after ((p professor))
    (format t "Chair: ~d ~% " (chair p)))

(defmethod get-info :after ((c chairman))
    (format t "Share: ~d ~% " (share c)))



;;;-------------------------------------
;;; Examples for testing
;;;-------------------------------------

(defvar *Max* (make-student "Max Mustermann" 22 "Beispiel Strasse 1" 21222011 0))

(defvar *Maria* (make-professor-chairman "Maria Musterfrau" 52 "Schoene Strasse 2" 21222010 "IBAN:1234" "Software Architecture" "50%"))

(get-info *Max*)
(format t "~%--------------~%~%")
(get-info *Maria*)
