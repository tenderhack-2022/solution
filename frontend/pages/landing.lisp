(uiop:define-package #:app/pages/landing
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/utils
                #:format-time
                #:get-user-token)
  (:import-from #:app/program
                #:program-end
                #:program-title
                #:program
                #:program-begin)
  (:import-from #:local-time
                #:timestamp<
                #:format-timestring
                #:+iso-8601-time-format+
                #:now)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-lass)
  (:import-from #:chat/client
                #:make-chat-api)
  (:import-from #:local-time-duration
                #:duration
                #:timestamp-duration-)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:serapeum
                #:parse-float
                #:fmt)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:reblocks-typeahead
                #:typeahead-widget)
  (:import-from #:app/kpgz
                #:search-kpgz))
(in-package #:app/pages/landing)


(defvar *program-to-chat*
  (make-hash-table :test 'equal))


(defwidget landing-page ()
  ((kpgz-items :initform nil
               :accessor kpgz-items)
   (max-total-price :initform 0
                    :accessor max-total-price)
   (results :initform nil
            :accessor prediction-results)))

(defwidget kpgz-input (typeahead-widget)
  ())


(defwidget kpgz-input-item ()
  ((code :initarg :code
         :initform nil
         :reader item-code)
   (name :initarg :name
         :initform nil
         :reader item-name)
   (parent-widget :initarg :parent
                  :reader parent-widget)))


(defwidget kpgz-item ()
  ((parent-widget :initarg :parent
                  :initform nil
                  :reader kpgz-parent)
   (kgpz-input :initform (make-instance 'kpgz-input)
               :reader kpgz-input)
   (code :initform ""
         :accessor kpgz-code)
   (title :initform ""
          :accessor kpgz-title)
   (amount :initform 0
           :accessor kpgz-amount)
   (price :initform 0
          :accessor kpgz-price)))


(defmethod render ((widget kpgz-input-item))
  (with-html
    (:span (fmt "~A: ~A"
                (item-code widget)
                (item-name widget)))))


(defmethod reblocks-typeahead:execute-query ((widget kpgz-input) query)
  (loop for (code title) in (search-kpgz query)
        collect (make-instance 'kpgz-input-item :code code :name title  :parent widget)))


(defmethod reblocks-typeahead:on-select ((item kpgz-input-item))
  (when (parent-widget item)
    (let ((typeahead-widget (parent-widget item)))
      (setf (reblocks-typeahead:input-value typeahead-widget)
            (fmt "~A: ~A"
                 (item-code item)
                 (item-name item)))
      (reblocks/widget:update typeahead-widget)
      (reblocks-typeahead:hide-results typeahead-widget))))


(defun make-landing-page ()
  (make-instance 'landing-page))


(defun make-kpgz (parent)
  (make-instance 'kpgz-item
                 :parent parent))


(defmethod render ((widget landing-page))
  (with-html
    (flet ((add-kpgz (&rest args)
             (declare (ignore args))
             (log:info "Adding kpgz item")
             (setf (kpgz-items widget)
                   (append (kpgz-items widget)
                           (list (make-kpgz widget))))
             (reblocks/widget:update widget)))
      (:div :class "form"
             (:label :for "date" "Дата проведения:")
             (:input :type "text"
                     :id "date"
                     :name "date"
                     :value (local-time:format-timestring nil (local-time:today)
                                                          :format local-time:+iso-8601-date-format+)
                     :disabled "true")
             (:input :type "text"
                     :name "title"
                     :placeholder "Наименование Котировочной Сессии")

             (when (kpgz-items widget)
               (:div :class "kpgz-items"
                     (loop for kpgz-widget in (kpgz-items widget)
                           do (render kpgz-widget))))

             (with-html-form (:post #'add-kpgz)
               (:input :type "submit"
                       :class "button success"
                       :value "Добавить позицию"))

             (:p :class "starting-price total-price"
                 "НАЧАЛЬНАЯ ЦЕНА"
                 (:span (fmt "~,2F ₽ ~A"
                             (max-total-price widget)
                             (cond
                               ((< (max-total-price widget)
                                   1000)
                                "🙂")
                               ((< (max-total-price widget)
                                   5000)
                                "😛")
                               ((< (max-total-price widget)
                                   10000)
                                "😭")
                               (t
                                "😱")))))

             (when (prediction-results widget)
               (:div :class "graph"
                     (:img :src "https://love-me-tender-love-me-sweet.40ants.com/images/left.png")
                     (:div :class "predictions"
                           (loop with len = (length (prediction-results widget))
                                 for (price competitors) in (reverse
                                                             (prediction-results widget))
                                 for idx upfrom 0
                                 for last-item = (= idx (1- len)) 
                                 do (let ((price (min price 1.0)))
                                      (:div :class (if last-item
                                                       "prediction"
                                                       "prediction last-item")
                                            (:div :class "prosadka"
                                                  :style (fmt "height: ~,4F%" (* price 100)))
                                            (:div :class "price"
                                                  :style (fmt "height: ~,4F%" (* (- 1.0 price)
                                                                                 100)))
                                            (:div :class "competitors"
                                                  competitors))))
                           (:span :class "last-price"
                                  (fmt "~,2F%"
                                       (* 100 (first (first (prediction-results widget)))))))
                     (:img :src "https://love-me-tender-love-me-sweet.40ants.com/images/right.png"))
               ;; (:div :class "results"
               ;;       (:p :class "total-price"
               ;;           "Вероятная итоговая цена:"
               ;;           (:span (first (prediction-results widget))))
               ;;       (:p :class "total-price"
               ;;           "Вероятное количество участников:"
               ;;           (:span (second (prediction-results widget)))))
               )))))


(defun remove-pkgz-item (landing-widget
                         kpgz-widget)
  (setf (kpgz-items landing-widget)
        (remove kpgz-widget (kpgz-items landing-widget)))
  (reblocks/widget:update landing-widget))


(defun parse-number (text)
  (when text
    (unless (string-equal text "")
      (or (ignore-errors (parse-float text))
          (ignore-errors (parse-integer text))
          0))))


(defun recalculate-totals (widget)
  (check-type widget landing-page)
  
  (loop for item in (kpgz-items widget)
        summing (* (kpgz-amount item)
                   (kpgz-price item)) into total
        collect (kpgz-code item) into codes
        finally (when (not (= (max-total-price widget) total))
                  (setf (max-total-price widget)
                        total)

                  (when (and (prediction-results widget)
                             (not (listp (first (prediction-results widget)))))
                    (setf (prediction-results widget)
                          (list (prediction-results widget))))

                  (push (app/predict::predict "Какая-то сессия" codes total)
                        (prediction-results widget))
                    
                  (reblocks/widget:update widget))))


(defmethod render ((widget kpgz-item))
  (with-html
    (flet ((update (&key items-count item-price &allow-other-keys)
             (log:info "Updating kpgz" items-count item-price)
             (let ((landing (kpgz-parent widget))
                   (items-count (parse-number items-count))
                   (item-price (parse-number item-price)))
               (when items-count
                 (setf (kpgz-amount widget) items-count))
               (when item-price
                 (setf (kpgz-price widget) item-price))
               (reblocks/widget:update widget)
               (recalculate-totals landing)))
           (remove-kpgz (&rest args)
             (log:info "Removing" args)
             (let ((landing (kpgz-parent widget)))
               (remove-pkgz-item (kpgz-parent widget)
                                 widget)
               (recalculate-totals landing))))

      (:label "ОКПД2")
      (:input :type "text"
              :name "okpd"
              :placeholder "ОКПД2")
      
      (:label "КПГЗ") 
      (render (kpgz-input widget))

      (with-html-form (:post #'update)
        (let ((update-action (reblocks/actions:make-js-form-action #'update)))
          (:label "Количество")
          (:input :type "number"
                  :min "0"
                  :name "items-count"
                  :onblur update-action
                  :placeholder "Кол-во"
                  :value (kpgz-amount widget))
          (:label "Цена за единицу, ₽")
          (:input :type "number"
                  :min "0"
                  :name "item-price"
                  :onblur update-action
                  :placeholder "Цена за единицу, ₽"
                  :value (kpgz-price widget))
        
          (when (and (kpgz-amount widget)
                     (kpgz-price widget))

            (:div :class "total-price"
                  "Сумма"
                  (:span (fmt "~,2F ₽" (* (kpgz-amount widget)
                                          (kpgz-price widget))))))))
      
      (with-html-form (:post #'remove-kpgz)
        (:input :type "submit"
                :class "button warning"
                :value "Удалить")))))


(defmethod get-dependencies ((widget landing-page))
  (list* (reblocks-lass:make-dependency
           `(.landing-page
             ((:and .button .warning)
              :background-color "rgba(0, 0, 0, 0)"
              :border 2px solid "rgb(38, 75, 130)"
              :margin-bottom 0
              :margin-top 1rem)
             ((:and .button .warning :hover)
              :background-color "rgb(38, 75, 130)"
              :color white)
             
             ((:and .button .success)
              :background-color "rgba(0, 0, 0, 0)"
              :border 2px solid "rgb(185, 56, 45)"
              :border-radius 5px)
             ((:and .button .success :hover)
              :background-color "rgb(185, 56, 45)"
              :color white)
             
             (.total-price
              :font-size 1.7rem
              (span :margin-left 0.5rem
                    :color "rgb(38, 75, 130)"
                    :font-weight 700))
             (.starting-price
              :background-color "rgb(232, 238, 246)"
              :border 1px solid "rgb(209, 214, 221)"
              :border-radius 5px
              :padding 1rem
              :margin-left -1rem
              :margin-right 2rem
              :margin-bottom 1rem
              (span :color "rgb(38, 75, 130)"
                    :font-weight bold))
             (.kpgz-items
              :display flex
              :flex-wrap wrap
              (.kpgz-item
               :background-color "rgb(232, 238, 246)"
               :border 1px solid "rgb(209, 214, 221)"
               :border-radius 5px
               :padding 1rem
               :margin-left -1rem
               :margin-right 2rem
               :margin-bottom 1rem
               :max-width 30%
               ))

             (.graph
              :display flex
              :flex-direction row
              :justify-content space-between
              :align-items center
              :margin-bottom 10rem
              :margin-left -3rem

              (img :height 7rem)
              
              (.predictions
               :flex-direction row
               :gap 1rem
               :display flex
               :flex-direction row
               :gap 1rem
               (.prediction
                :width 2rem
                :height 10rem
                :opacity 100%
                (.prosadka
                 :background-color "rgb(185, 56, 45)"
                 :border-top-left-radius 10px)
                (.price
                 :background-color "rgb(47, 74, 126)"
                 :border-bottom-right-radius 10px)
                (.competitors
                 :text-align center
                 :background-color "rgb(232, 238, 246)"))
               ((:and .prediction .last-item)
                :opacity 50%))
              (.last-price
               :font-size 1.5rem))))
         (call-next-method)))
