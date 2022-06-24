(defparameter *test-file* #P"~/docs/org-sync/blogs/unlife.org ")

(defparameter *dispatch* '((#\* . heading)))

(defclass doc ()
  ((headings
    :initarg :headings
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor headings)))

(defclass doc-raw ()
  ((file  :initarg :file                                 :accessor file)
   (str   :initarg :str                                  :accessor str)
   (pos   :initarg :pos                                  :accessor pos)
   (node  :initarg :node                                 :accessor node)
   (final :initarg :final :initform (make-instance 'doc) :accessor final)))

(defclass heading ()
  ((depth      :initarg :depth      :initform 1   :accessor depth)
   (todo       :initarg :todo       :initform nil :accessor todo)
   (title      :initarg :title      :initform nil :accessor title)
   (properties :initarg :properties :initform nil :accessor properties)
   (parent     :initarg :parent     :initform nil :accessor parent)
   (children   :initarg :children   :initform nil :accessor children)
   (paragraphs :initarg :paragraphs :initform nil :accessor paragraphs)))

(defclass property ()
  ((entries :initarg :entries :initform nil :accessor entries)))

(defclass paragraph () ())

(defgeneric parse (obj doc)
  (:documentation "The parser method"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heading methods

(defmethod parse :after ((obj heading) (doc doc-raw))
  (parse (str doc) doc))

(defun heading-title (h s)
  (loop for c = (read-char s nil :eof)
        for n = (peek-char nil s nil :eof)
        for v = (make-array 0 :adjustable t :fill-pointer 0)
        until (eq c #\Newline)
        if (and (char= c #\:)
                (char/= n #\space))
          do (heading-tag h s v)
        else
          do (vector-push-extend c v)))

(defun heading-todo (h s)
  (let ((input
          (loop for c = (read-char s nil :eof)
                for v = (make-array 0 :adjustable t :fill-pointer 0)
                until (eq c #\Space)
                do (vector-push-extend c v)
                finally (return v))))
    (if (every #'upper-case-p input)
        (setf (todo h) (concatenate 'string input))
        (setf (title h) input)))
  (heading-title h s))

(defun heading-depth (h s)
  (loop for c = (read-char s nil :eof)
        until (eq c #\Space)
        do (incf (depth h)))
  (heading-todo h s))

(defun heading-parser (h s)
  (heading-depth h s))

(defmethod parse ((obj heading) (doc doc-raw))
  (with-slots (final) doc
    (vector-push-extend obj (headings final)))
  (heading-parser obj (str doc))
  (loop for c = (read-char (str doc) nil :eof)
        until (eq c #\Space)
          do (incf (depth obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; entrypoint and return methods

(defmethod parse ((obj stream) (doc doc-raw))
  (loop for c = (read-char obj nil :eof)
        until (eq c :eof)
        when (assoc c *dispatch*)
          do (parse (make-instance (cdr (assoc c *dispatch*)))
                    doc)
        finally (return (final doc))))

(defmethod parse ((obj string) doc)
  (declare (ignore doc))
  (let ((stream (make-string-input-stream obj)))
    (parse stream (make-instance
                   'doc-raw
                   :str stream))))

(defmethod parse ((obj pathname) doc)
  (declare (ignore doc))
  (with-open-file (stream obj :element-type 'character)
    (parse stream (make-instance
                   'doc-raw
                   :file obj
                   :str stream))))
