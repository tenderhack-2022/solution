(uiop:define-package #:app/kpgz
  (:use #:cl)
  (:import-from #:montezuma
                #:add-document-to-index)
  (:import-from #:serapeum
                #:fmt))
(in-package #:app/kpgz)


(defvar *index* nil)


(defun load-kpgz ()
  (with-open-file (s "~/kpgz.csv")
    (read-line s nil nil)
    (loop with index = (make-instance 'montezuma:index)
          for line in (uiop:slurp-stream-lines s)
          for parsed = (str:split ";" line)
          for code = (first parsed)
          for title = (second parsed)
          do (add-document-to-index index
                                    (list (cons "code" code)
                                          (cons "title" title)))
          finally (return (setf *index*
                                index)))))


(defun search-kpgz (query)
  (unless *index*
    (load-kpgz))

  (uiop:while-collecting (collect-document)
    (flet ((process (doc-id score)
             (declare (ignore score))
             (let* ((doc (montezuma:get-document *index* doc-id)))
               (collect-document
                (list (montezuma:document-value doc "code")
                      (montezuma:document-value doc "title"))))))
      (montezuma:search-each *index*
                             (fmt "title:~A" query)
                             #'process))))
