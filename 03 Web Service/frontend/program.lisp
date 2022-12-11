(uiop:define-package #:app/program
  (:use #:cl)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:cl-cookie)
  (:import-from #:local-time
                #:unix-to-timestamp
                #:timestamp))
(in-package #:app/program)


;; curl 'https://tv.yandex.ru/api/213' \
;;   -H 'Cookie: yandexuid=2496322901669495048; yuidss=2496322901669495048; is_gdpr=0; is_gdpr_b=CIy8DhDXlwEoAg==; _yasc=HT6z8bSGxymqNx08FGbAerfz1Bjx2i/P2ypMX30cygHmPRdw7MlJc9E68rMboQ==; i=dq6N/deeUusvTm7issbaVTELzWH2tBie+vWd/alhVfAKCIIeNKapoVq/41YlWUpp0sClcCVayX6CALFmEAOcoNwdfg4=' \
;;   -H 'X-Requested-With: XMLHttpRequest' \
;;   -H 'X-TV-SK: e40d23bed00e873c701579c9660905316f8c0929:1669495059476' \
;;   --compressed

;; X-TV-SK:
;;  window.__INITIAL_SK__ = {"key":"83bbd91e6e03ddc7705672dc90d79310302884ec:1669495376914","expire":1669581776914};

;; Это не работает - яндекс редиректит на Captche
;; (defun get-yandex-program ()
;;   (let* ((cookies (cl-cookie:make-cookie-jar))
;;          ;; (index-page (dex:get "https://tv.yandex.ru/" :cookie-jar cookies))
;;          )
;;     (dex:get "https://tv.yandex.ru/" :cookie-jar cookies
;;                                      :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.5112.114 YaBrowser/22.9.1.1146 Yowser/2.5 Safari/537.36")))
;;     ;; index-page
;;     ;; (dex:get "https://tv.yandex.ru/api/213" :cookie-jar cookies
;;     ;;                                         :headers '(("X-TV-SK" . "e40d23bed00e873c701579c9660905316f8c0929:1669495059476")
;;     ;;                                                    ("X-Requested-With" . "XMLHttpRequest")))
;;     ))


(defclass program ()
  ((begin :initarg :begin
          :type timestamp
          :reader program-begin)
   (end :initarg :end
        :type timestamp
        :reader program-end)
   (title :initarg :title
          :type string
          :reader program-title)))


(defmethod print-object ((obj program) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "[~A -> ~A] ~A"
            (program-begin obj)
            (program-end obj)
            (program-title obj))))


(defcached (get-program :timeout (* 15 60)) ()
  (let* ((response (dex:get "https://stream.1tv.ru/api/schedule.json"))
         (data (yason:parse response))
         (channel (gethash "channel" data))
         (schedule (gethash "schedule" channel))
         (program (gethash "program" schedule)))
    (loop for item in program
          collect (make-instance 'program
                                 :begin (unix-to-timestamp (gethash "begin" item))
                                 :end (unix-to-timestamp (gethash "end" item))
                                 :title (gethash "title" item)))))
