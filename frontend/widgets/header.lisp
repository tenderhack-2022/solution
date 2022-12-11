(uiop:define-package #:app/widgets/header
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  ;; (:import-from #:app/widgets/login
  ;;               #:get-username)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:passport/client
                #:make-passport)
  (:export
   #:make-page-with-header))
(in-package #:app/widgets/header)


(defwidget page-with-header ()
  ((content :initarg :content
            :reader content)))


(defun make-page-with-header (content)
  (make-instance 'page-with-header :content content))


(defmethod render ((widget page-with-header))
  (flet ((logout (&rest rest)
           (declare (ignore rest))
           (reblocks/session:reset)
           (reblocks/response:redirect "/logout/"))
         (login (&rest rest)
           (declare (ignore rest))
           (reblocks/response:redirect "/login/")))
    (let* ((api (passport/client::connect
                 (make-passport)
                 (get-user-token)))
           (profile (ignore-errors
                     (passport/client::my-profile api)))
           (avatar-url (when profile
                         (passport/client::user-avatar-url profile))))
      (reblocks/html:with-html
        (:header
         ;; TODO: replace with custom style
         ;; (:link :href "https://ideahunt.ru/static/css/main.a005c85d.css"
         ;;        :rel "stylesheet")
         (:div :class "navbar"
               (:img :src "https://zakupki.mos.ru/static/media/pp_logo_ny.94891f12246e7741c4a10dba12921d6b.svg")
               (:span :class "navbar-main-logo"
                      (:span :class "first" "Tender")
                      (:span :class "second" "Lead"))))

        (:div :class "page-content"
              (render (content widget)))))))



(defmethod get-dependencies ((widget page-with-header))
  (list
   (reblocks-lass:make-dependency
     `(.page-with-header
       :display flex
       :flex-direction column
       :align-items center

       ((:and input :disabled)
         :background "rgb(232, 238, 246)")

       (.navbar
        :display flex
        :justify-content space-between
        :margin-top 1rem
        :margin-bottom 2rem
        :padding-left 4rem
        :padding-right 4rem
        (span.navbar-main-logo :font-size 2rem
                               :font-weight bold
                               :color darkgray
                               (.first :color red)
                               (.second :color "#315593"
                                        :margin-left -0.2em)))

       ((:or .navbar-user-icon
             .login-link)
        :margin-left 3em)
       
       (.page-content
        :width 80%
        :margin-left auto
        :margin-right auto)
       
       (header
        :width 100%
        (.main-menu :display flex
                    :align-items center
                    (a :margin-right 1rem))
        
        (input
         :margin 0))))))
