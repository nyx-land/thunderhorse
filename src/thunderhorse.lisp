(in-package :thunderhorse)

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
  ((body
    :initarg :body
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor body)
   (parent
    :initarg :parent
    :accessor parent)))

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

(defclass paragraph (section)
  ((body
    :initform (make-string 0))))

(defgeneric parse (obj doc)
  (:documentation "The parser method"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lesser element methods

(defgeneric parse-lesser (obj doc)
  (:documentation "Special parser for lesser elements"))

(defmethod parse-lesser ((obj paragraph) (doc doc-raw))
  (princ obj))

(defmethod parse-lesser ((obj section) (doc doc-raw))
  (pg-split obj (vector-pop (body obj)))
  (loop for p across (body obj)
        do (parse-lesser p doc)))

(defun heading-conditions (stream c n h)
  (or (and (char= c #\newline)
           (char= n #\*)
           (char= h #\space))
      (and (= 1 (file-position stream))
           (char= c #\*)
           (char= n #\space))))

(defmethod parse ((obj section) (doc doc-raw))
  (with-slots (parent body) obj
    (loop for c = (read-char (str doc) nil :eof)
          for n = (peek-char nil (str doc) nil :eof)
          for h = (peek-nth-chars 2 (str doc) nil :eof)
          until (or (eq c :eof)
                    (heading-conditions (str doc) c n h))
          collect c into sections
          finally (vector-push-extend (concatenate 'string sections)
                                      (body obj))
                  (parse-lesser obj doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; greater element methods

(defmethod parse ((obj drawer) (doc doc-raw))
  (loop for line = (read-line (str doc) nil :eof)
        for ksearch = (search ": " line)
        for k = (subseq line 0 ksearch)
        for v = (subseq line (1+ ksearch))
        until (search ":END:" line :test #'char-equal)
        collect (cons k v) into e
        finally (push e (entries obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heading methods

(defgeneric parse-heading (obj doc)
  (:documentation "Special parser for heading"))

(defmethod parse-heading ((obj tag) (doc doc))
  (push obj (tags (cdr (car (index obj)))))
  (with-slots (name) obj
    (let ((tag? (gethash name (tags doc)))
          (head (car (car (index obj)))))
      (if tag?
          (when (null (assoc head (index tag?) :test #'equalp))
            (push (car (index obj)) (index tag?)))
          (setf (gethash name (tags doc)) obj)))))

(defun tag-split (tags heading doc)
  (with-slots (title) heading
    (loop for i = 0 then (incf j)
          as j = (position #\: tags :start i)
          unless (equalp (subseq tags i j) "")
            collect (parse-heading
                     (make-instance
                      'tag
                      :name (subseq tags i j)
                      :index (list (cons title heading)))
                     doc)
          while j)))

(defun parse-heading-tags (title heading doc)
  (let* ((tag-index
           (loop for c across (reverse title)
                 for n = (1- (length title)) then (decf n)
                 until (char= c #\space)
                 finally (return (1+ n))))
         (tags (subseq title tag-index)))
    (setf (title heading) (subseq title 0 tag-index))
    (tag-split tags heading doc)))

(defun parse-heading-priority (todo title doc)
  (let ((priority? (remove-if-not
                    (lambda (x) (search x title))
                    (priority-vals doc))))
    (when priority?
      (setf (priority todo)
            (aref (car priority?) 2))
      (setf (title (parent todo))
            (string-trim (format nil " ~a" (car priority?)) title)))))

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
          (setf (state obj) (car todo?))
          (vector-push-extend obj (todos final))
          (parse-heading-priority obj (title parent) final))))))

(defun parse-heading-parent (head doc)
  (with-slots (node final) doc
    (unless (eq node head)
      (cond
        ((= 1 (depth head))
         (setf (parent head) final)
         (vector-push-extend head (headings final)))
        ((> (depth head) (depth node))
         (vector-push-extend head (children node)))
        ((> (depth node) (depth head))
         (vector-push-extend (children node) head)
         (setf (parent head) node))))))

(defmethod parse-heading :before ((obj heading) (doc doc-raw))
  (with-slots (node final) doc
    (when (null node)
      (setf node obj)
      (setf (parent obj) final)
      (vector-push-extend obj (headings final)))))

(defmethod parse-heading :after ((obj heading) (doc doc-raw))
  (with-slots (node) doc
    (unless (eq obj node)
      (setf node obj))))

(defmethod parse-heading ((obj heading) (doc doc-raw))
  (parse-heading-parent obj doc)
  (parse-heading (make-instance 'todo :parent obj) doc)
  (with-slots (title) obj
    (when (char= (aref title (1- (length title))) #\:)
      (parse-heading-tags title obj (final doc))))
  (parse (make-instance 'section :parent obj) doc))

(defmethod parse ((obj heading) (doc doc-raw))
  (with-slots (title depth) obj
    (let* ((line (read-line (str doc) nil :eof))
           (head (search "* " line)))
      (cond ((char= #\space (aref line 0))
             (setf title (subseq line 1))
             (parse-heading obj doc))
            (head
             (setf depth (1+ (1+ head)))
             (setf title (subseq line depth))
             (parse-heading obj doc))
            (t (parse (make-instance 'section :body line)
                      doc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; entrypoint and return methods

(defmethod parse ((obj stream) (doc doc-raw))
  (loop for c = (read-char obj nil :eof)
        for n = (peek-char nil obj nil :eof)
        until (eq c :eof)
        do (cond ((char= c #\*)
                  (setf (pos doc) (file-position obj))
                  (parse (make-instance 'heading)
                         doc))
                 ((and (char= c #\#)
                       (char= n #\+))
                  (parse (make-instance 'doc-opt)
                         doc))
                 (t (parse (make-instance 'section)
                           doc)))
        finally (return (values doc (final doc)))))

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
