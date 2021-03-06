(in-package :thunderhorse)

(defun peek-nth-chars (num &optional (stream *standard-input*)
                             (eof-error t) eof-value)
  (let* ((chars (loop repeat num
                      for c = (read-char stream eof-error eof-value)
                      collect c))
         (npeek (car (last chars))))
    (loop for c in (reverse chars)
          unless (eq c :eof)
            do (unread-char c stream))
    npeek))

(defun pg-split (section string)
  (let* ((newlines (search (format nil "~%~%") string))
         (chunk (make-instance 'paragraph
                               :body (subseq string 0 newlines)
                               :parent (parent section))))
    (when newlines
      (vector-push-extend chunk (body section))
      (pg-split section (subseq string (1+ (1+ newlines)))))
    (unless newlines
      (vector-push-extend (make-instance
                           'paragraph
                           :body string
                           :parent (parent section))
                          (body section)))))
