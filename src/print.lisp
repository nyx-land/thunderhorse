(in-package :thunderhorse)

(defmethod print-object ((object heading) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'title)
      (format stream "~a" (title object)))))

(defmethod print-object ((object section) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (and (slot-boundp object 'body)
               (not (zerop (length (body object)))))
      (let ((preview nil))
        (if (> (length (body object)) 200)
            (setf preview (format nil "~a...[truncated string of ~a length]"
                                  (subseq (body object) 0 200)
                                  (length (body object))))
            (setf preview (format nil "~a" (body object))))
        (format stream "~s" preview)))))

(defmethod print-object ((object markup) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'text)
      (format stream "~a" (text object)))))

(defmethod print-object ((object link) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (and (slot-boundp object 'text)
               (slot-boundp object 'href))
      (format stream "~a (~a)"
              (text object)
              (href object)))))
