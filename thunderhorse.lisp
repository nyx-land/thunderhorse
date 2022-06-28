(defparameter *test-file* #P"~/docs/org-sync/blogs/unlife.org ")
(defparameter *test-str* (format nil "* foo~%** bar"))
(defparameter *dispatch* '((#\* . heading)))

(defclass doc ()
  ((headings
    :initarg :headings
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor headings)
   (todo-vals
    :initarg :todo-vals
    :initform '("TODO" "DONE")
    :accessor todo-vals)
   (priority-vals
    :initarg :priority-vals
    :initform (mapcar (lambda (x) (format nil "[#~a]" x))
                      '("A" "B" "C"))
    :accessor priority-vals)
   (todos
    :initarg :todos
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor todos)
   (tags
    :initarg :tags
    :initform (make-hash-table :test #'equalp)
    :accessor tags)))

(defclass doc-raw ()
  ((file  :initarg :file                                 :accessor file)
   (str   :initarg :str                                  :accessor str)
   (pos   :initarg :pos   :initform nil                  :accessor pos)
   (node  :initarg :node  :initform nil                  :accessor node)
   (final :initarg :final :initform (make-instance 'doc) :accessor final)))

(defclass tag ()
  ((name  :initarg :name  :accessor name)
   (index :initarg :index :accessor index)))

(defclass head-meta ()
  ((state  :initarg :state  :accessor state)
   (parent :initarg :parent :accessor parent)))

(defclass todo (head-meta)
  ((priority
    :initarg :priority :accessor priority)))

(defclass section ()
  ((body :initarg :body :accessor body)))

(defclass heading ()
  ((depth
    :initarg :depth
    :initform 1
    :accessor depth)
   (todo
    :initarg :todo
    :initform nil :accessor todo)
   (title
    :initarg :title
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor title)
   (properties
    :initarg :properties
    :initform nil
    :accessor properties)
   (parent
    :initarg :parent
    :initform nil
    :accessor parent)
   (children
    :initarg :children
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor children)
   (paragraphs
    :initarg :paragraphs
    :initform nil
    :accessor paragraphs)
   (tags
    :initarg :tags
    :initform nil
    :accessor tags)))

(defclass drawer ()
  ((entries :initarg :entries :initform nil :accessor entries)
   (parent  :initarg :parent :accessor parent)))

(defclass propdrawer (drawer) ())

(defclass paragraph () ())

(defgeneric parse (obj doc)
  (:documentation "The parser method"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heading methods

(defun heading-parent (h pos doc)
  (cond
    ((= 1 (depth h))
     (setf (parent h) doc))
    ((> (depth h) (depth pos))
     (vector-push-extend h (children pos)))
    ((> (depth pos) (depth h))
     (vector-push-extend (children pos) h))))

(defgeneric parse-heading (obj doc)
  (:documentation "Special parser for heading"))

(defmethod parse-heading :after ((obj heading) (doc doc-raw))
  (with-slots (node) doc
    (unless (eq obj node)
      (setf node obj))))

(defmethod parse-heading ((obj stream) (doc doc-raw)))

(defmethod parse-heading ((obj todo) (doc doc-raw))
  (with-slots (parent) obj
    (with-slots (final) doc
      (let ((todo? (remove-if-not
                    (lambda (x) (search x (title parent)))
                    (todo-vals final))))
        (when todo?
          (setf (todo parent) obj)
          (setf (title parent)
                (string-trim (car todo?) (title parent)))
          (vector-push-extend obj (todos final)))))))

(defmethod parse-heading ((obj heading) (doc doc-raw))
  (parse-heading (make-instance 'todo :parent obj) doc)
  ;;(with-slots (title depth todo) obj
  ;;  (let ((todo? (or (search "TODO" title)
  ;;                   (search "DONE" title))))
  ;;    (if todo?
  ;;        (progn
  ;;          (setf )))))

  ;;(with-slots (str final node) doc
  ;;  (heading-depth obj str)
  ;;  (if (null node)
  ;;      (progn
  ;;        (setf node obj)
  ;;        (setf (parent obj) final)
  ;;        (vector-push-extend obj (headings final)))
  ;;      (heading-parent obj node final))
  ;;  (parse-heading str doc))
  )

(defmethod parse :after ((obj heading) (doc doc-raw))
  (parse (str doc) doc))

(defmethod parse ((obj heading) (doc doc-raw))
  (with-slots (title depth) obj
    (let* ((line (read-line (str doc) nil :eof))
           (head (search "* " line)))
      (if head
          (progn
            (setf depth (1+ head))
            (setf title (subseq line (1+ (1+ depth))))
            (parse-heading obj doc))
          (parse (make-instance 'section :body line)
                 doc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; entrypoint and return methods

(defmethod parse ((obj stream) (doc doc-raw))
  (loop for c = (read-char obj nil :eof)
        until (eq c :eof)
        when (assoc c *dispatch*)
          do (setf (pos doc) (file-position obj))
             (parse (make-instance 'heading)
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
