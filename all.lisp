(uiop:define-package #:all/all
  (:use #:cl)
  (:import-from #:app/server)
  (:export
   #:start-all))
(in-package #:all/all)


(defun start-all ()
  (app/server::start)
  (values))
