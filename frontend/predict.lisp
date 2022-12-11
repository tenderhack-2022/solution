(uiop:define-package #:app/predict
  (:use #:cl)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:serapeum
                #:dict))
(in-package #:app/predict)


(defun predict (name kpgz-codes max-price)
  (let* ((data (dict "name" name
                     "kpgz" (etypecase kpgz-codes
                              (list (str:join ";" kpgz-codes))
                              (string kpgz-codes))
                     "region" "Москва"
                     "nmck" max-price
                     "date" "2022-07-27 15:48:12.147"
                     "stakes" 10
                     "inn" "8a2f56591b85b74bb7cf813c9cb570a1"))
         (response (dex:post "http://51.250.82.124:8000/predict/"
                             :headers '((:content-type . "application/json"))
                             :content (with-output-to-string* ()
                                        (yason:encode data))
                             :read-timeout 60))
         (parsed (yason:parse response)))
    (list (gethash "price" parsed)
          (gethash "competitors" parsed))))
