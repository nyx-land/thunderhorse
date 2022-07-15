(in-package :thunderhorse)

(defmethod print-object ((object paragraph) stream)
  (let ((preview nil))
    (if (> (length (body object)) 200)
        (setf preview (format nil "~a...[truncated string of ~a length]"
                              (subseq (body object) 0 200)
                              (length (body object))))
        (setf preview (format nil "~a" (body object))))
    (write-string "#<PARAGRAPH " stream)
    (write-string preview stream)
    (write-string " >" stream)))

(defmethod print-object ((object markup) stream)
  (if (slot-boundp object 'text)
      (format stream "#<~a ~a>"
              (class-name (class-of object))
              (text object))
      (print-unreadable-object (object stream :type t :identity t))))
