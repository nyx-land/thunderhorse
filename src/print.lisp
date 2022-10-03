(in-package :thunderhorse)

(defmethod print-object ((object heading) stream)
  (if (slot-boundp object 'title)
      (format stream "#<~a ~s>"
              (class-name (class-of object))
              (title object))))

(defmethod print-object ((object section) stream)
  (let ((preview nil))
    (if (> (length (body object)) 200)
        (setf preview (format nil "~a...[truncated string of ~a length]"
                              (subseq (body object) 0 200)
                              (length (body object))))
        (setf preview (format nil "~a" (body object))))
    (if (zerop (length (body object)))
        (print-unreadable-object (object stream :type t :identity t))
        (format stream "#<~a ~s>"
                (class-name (class-of object))
                preview))))

(defmethod print-object ((object markup) stream)
  (if (slot-boundp object 'text)
      (format stream "#<~a ~s>"
              (class-name (class-of object))
              (text object))
      (print-unreadable-object (object stream :type t :identity t))))

(defmethod print-object ((object link) stream)
  (if (and (slot-boundp object 'text)
           (slot-boundp object 'href))
      (format stream "#<~a ~s (~a)>"
              (class-name (class-of object))
              (text object)
              (href object))
      (print-unreadable-object (object stream :type t :identity t))))
