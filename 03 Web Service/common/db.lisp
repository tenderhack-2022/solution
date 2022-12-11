(uiop:define-package #:common/db
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:dbi)
  (:import-from #:dbi.error)
  (:import-from #:cl-postgres)
  ;; To prevent Mito from trying to load driver on first connect.
  ;; Sometimes this can cause errors if DBD get's updated by some
  ;; project's check
  (:import-from #:dbd.postgres)
  (:import-from #:mito
                #:object-id
                #:select-dao)
  (:import-from #:ironclad
                #:octets-to-integer
                #:hmac-digest
                #:make-hmac
                #:ascii-string-to-byte-array)
  (:import-from #:secret-values
                #:ensure-value-revealed)
  (:import-from #:alexandria
                #:make-keyword
                #:length=
                #:remove-from-plistf
                #:with-gensyms)
  (:import-from #:sxql
                #:order-by
                #:where)
  (:import-from #:serapeum
                #:fmt)
  (:export #:with-transaction
           #:with-connection
           #:connect-toplevel
           #:with-lock
           #:unable-to-aquire-lock
           #:get-lock-name
           #:sql-fetch-all
           #:get-lock
           #:execute
           #:connect-toplevel-in-dev
           #:lock-timeout
           #:map-by-id))
(in-package #:common/db)


(defun get-db-host ()
  (or (uiop:getenv "DB_HOST")
      "192.168.0.2"))

(defun get-db-port ()
  (parse-integer
   (or (uiop:getenv "DB_PORT")
       "5432")))

(defun get-db-name ()
  (or (uiop:getenv "DB_NAME")
      "postgres"))

(defun get-db-user ()
  (or (uiop:getenv "DB_USER")
      "root"))

(defun get-db-pass ()
  (uiop:getenv "DB_PASSWORD"))


;; TODO: разобраться почему при использовании pool,
;; запросы через раз зависают. Пока не кэшируем:
(defparameter *cached-default* nil)


(define-condition connection-error (error)
  ((message :initarg :message
            :reader error-message))
  (:report (lambda (condition stream)
             (format stream "~A"
                     (error-message condition)))))


(declaim (notinline inner-connect))

(defun inner-connect (&key host database-name username password
                           (port 5432)
                           (cached *cached-default*)
                           (use-ssl :no))
  "This function is used to leave a trace in the backtrace and let
   logger know which arguments are secret."
  
  (funcall (if cached
               'cl-dbi:connect-cached
               'cl-dbi:connect)
           :postgres
           :host host
           :port port
           :database-name database-name
           :username username
           :password (ensure-value-revealed password)
           :use-ssl use-ssl))


(defun connect (&key host database-name username password
                     port
                     (cached *cached-default*)
                     (use-ssl :no))
  (inner-connect :host (or host
                           (get-db-host))
                 :port (or port
                           (get-db-port))
                 :database-name (or database-name
                                    (get-db-name))
                 :username (or username
                               (get-db-user))
                 :password (or password
                               (get-db-pass))
                 :cached cached
                 :use-ssl use-ssl))


(defun connect-toplevel ()
  (setf mito:*connection* (connect :cached nil)))


(defun connect-toplevel-in-dev ()
  (setf mito:*connection*
        (cl-dbi:connect :postgres
                        :host (get-db-host)
                        :port 5432
                        :database-name (get-db-name)
                        :username "ultralisp"
                        :password "ultralisp")))


(defun call-with-transaction (func)
  (cl-dbi:with-transaction mito:*connection*
    (funcall func)))


(defmacro with-transaction (&body body)
  (with-gensyms (transactional-func)
    `(flet ((,transactional-func ()
              ,@body))
       (declare (dynamic-extent #',transactional-func))
       (call-with-transaction #',transactional-func))))


(defvar *was-cached*)


(defun call-with-connection (func &rest connect-options &key (cached *cached-default*) &allow-other-keys)
  (when (and cached
             (boundp '*was-cached*)
             (not *was-cached*))
    (error 'connection-error
           :message "Unable to get cached connection inside a block with non-cached connection."))

  (let* ((*was-cached* cached)
         ;; (cl-postgres:*ssl-key-file* )
         ;; 
         ;; (cl-postgres:*ssl-root-ca-file*
         ;;  ;; cl-postgres:*ssl-certificate-file*
         ;;  "/home/art/.postgresql/root.crt")
         (schema (prog1 (getf connect-options :schema)
                   (remove-from-plistf connect-options :schema)))
         (mito:*connection*
           ;; In cached mode we will reuse current connect.
           ;; This way, nested WITH-CONNECTION calls will
           ;; reuse the same connection and postgres savepoints.
           (cond ((and *was-cached*
                       mito:*connection*)
                  mito:*connection*)
                 (t
                  (apply #'connect
                         connect-options)))))
    (when schema
      (execute (format nil "set search_path TO ~A,public"
                       (string-downcase schema))))
    
    (unwind-protect
         (call-with-transaction func)
      (unless cached
        ;; We don't want to close nested cached connections
        ;; because they should be closed only on upper level
        ;; Here is a state table showing in which cases connect
        ;; will be closed:
        ;; | top connect | nested connect | close top | close nested |
        ;; | cached?     | cached?        | connect?  | connect?     |
        ;; |-------------+----------------+-----------+--------------|
        ;; | nil         | nil            | t         | t            |
        ;; | nil         | t              | t         | t            |
        ;; | t           | t              | t         | nil          |
        (cl-dbi:disconnect mito:*connection*)))))


(defmacro with-connection ((&rest connect-options) &body body)
  "Establish a new connection and start transaction"
  (with-gensyms (connected-func)
    `(flet ((,connected-func ()
              ,@body))
       (declare (dynamic-extent #',connected-func))
       (call-with-connection #',connected-func ,@connect-options))))


(defun make-hash-for-lock-name (name)
  ;; TODO: store all names in some global
  ;;       map {hash -> name} so that we'll be enable
  ;;       to make reverse transformation and know
  ;;       which locks are held on database.
  ;;       Also, we need to add a function which
  ;;       fetches all advisory locks from pg_locks table
  ;;       and returns a list of names.
  ;;       (All of this should be made threasafe of cause.)
  (let* ((bytes (ascii-string-to-byte-array name))
         (hmac (make-hmac bytes :sha256))
         (digest (hmac-digest hmac))
         (num-bits-in-result 63)
         (result (octets-to-integer digest
                                    :n-bits num-bits-in-result)))
    result))


(defun execute (sql &rest params)
  (cl-dbi:execute (cl-dbi:prepare mito:*connection* sql) params))


(defun sql-fetch-all (sql &rest params)
  (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare mito:*connection* sql) params)))


(define-condition unable-to-aquire-lock (simple-error)
  ((lock-name :initarg :lock-name
              :reader get-lock-name
              :documentation "Human readable lock name, passed to `with-lock' macro.")
   (key :initarg :key
        :reader get-key
        :documentation "An integer, representing lock in a Postgres database."))
  (:documentation "Signaled if some thread was unable to get a lock on a database.")
  (:report (lambda (condition stream)
             (format stream
                     "Unable to aquire lock: name=~A key=~A"
                     (ignore-errors
                      (get-lock-name condition))
                     (ignore-errors
                      (get-key condition))))))


(define-condition lock-timeout (unable-to-aquire-lock)
  ((timeout :initarg :timeout
            :reader get-timeout
            :documentation "An integer, a number a milliseconds."))
  (:report
   (lambda (condition stream)
     (format stream
             "Lock timeout: name=~A key=~A timeout=~A"
             (ignore-errors (get-lock-name condition))
             (ignore-errors (get-key condition))
             (ignore-errors (get-timeout condition)))))
  (:documentation "Raised when you are trying to get lock to was unable to do this during current lock_timeout."))


(defun try-to-get-lock (lock-name &key (signal-on-failure t))
  (unless (cl-dbi:in-transaction mito:*connection*)
    (error "To get a lock, you need to start a transaction."))
  
  (let* ((key (make-hash-for-lock-name lock-name))
         (rows (sql-fetch-all "SELECT pg_try_advisory_xact_lock(?) as locked" key))
         (locked? (getf (first rows)
                        :|locked|) ))
    (unless locked?
      (log:warn "Unable to get lock" lock-name)
      (when signal-on-failure
        (error 'unable-to-aquire-lock
               :lock-name lock-name
               :key key)))
    locked?))


(defun get-lock (lock-name &key (timeout 3))
  ""
  (unless (cl-dbi:in-transaction mito:*connection*)
    (error "To get a lock, you need to start a transaction."))
  
  (let ((key (make-hash-for-lock-name lock-name)))
    (when timeout
      (check-type timeout integer)
      (execute (format nil "SET lock_timeout = ~A" (* timeout 1000))))
    
    (handler-bind ((cl-dbi:<dbi-database-error>
                     ;; If we were unable to acquire lock because
                     ;; of timeout, we need to throw a special
                     ;; condition which can be catched by the caller
                     (lambda (condition)
                       (with-slots ((code dbi.error::error-code)) condition
                         ;; lock_not_available
                         (when (string-equal code "55P03")
                           (error 'lock-timeout
                                  :lock-name lock-name
                                  :key key
                                  :timeout timeout))))))
      (execute "SELECT pg_advisory_xact_lock(?)" key))))


(defmacro with-lock ((name &key (block t) (timeout 3) (signal-on-failure t)) &body body)
  (if block
      `(block with-lock
         (handler-bind ((lock-timeout (lambda (c)
                                        (declare (ignorable c))
                                        (unless ,signal-on-failure
                                          (return-from with-lock))))))
         (get-lock ,name :timeout ,timeout)
         (log:debug "Lock aquired:" ,name mito:*connection*)
         ,@body)
      `(when (try-to-get-lock ,name :signal-on-failure ,signal-on-failure)
         (log:debug "Lock aquired:" ,name mito:*connection*)
         ,@body)))


(defun make-list-placeholders (list)
  "Given a list of items, returns a string like \"(?,?,?)\"
   where number of questionmarks corresponds to number of list items."
  (format nil "(~{~A~^,~})"
          (loop repeat (length list)
                collect "?")))



;; Workaround для проблемы, которую я описал тут:
;; https://github.com/fukamachi/mito/issues/120
;; Закомментил потому что работает плохо, лучше явно указать mito:dao-class
;; для класса view
;; (defmethod initialize-instance :around ((class mito:dao-table-view) &rest initargs
;;                                         &key direct-superclasses &allow-other-keys)
;;   (unless (mito.util:contains-class-or-subclasses 'mito:dao-class direct-superclasses)
;;     (push (find-class 'mito:dao-class) (getf initargs :direct-superclasses)))
;;   (apply #'call-next-method class initargs))


(defun select-dao-by-ids (class-name ids)
  (let* ((class (find-class class-name))
         (pk-name (let ((value (mito.class:table-primary-key class)))
                    (unless (length= 1 value)
                      (error "PK should have only one column, to make select-dao-by-ids work. ~S has ~S."
                             class-name value))
                    (first value)))
         (columns (mito.class:table-column-slots class))
         (pk-column
           (loop for column in columns
                 for column-name = (closer-mop:slot-definition-name column)
                 thereis (and
                          (string-equal column-name
                                        pk-name)
                          column)))
         (pk-type (when pk-column
                    (mito.class:table-column-type pk-column))))
    (values
     (if ids
         (select-dao class-name
           (where (:in (make-keyword pk-name) ids))
           ;; Чтобы сохранился порядок элементов, такой же как в ids:
           (order-by (:raw (fmt "array_position(array[~{~A~^, ~}]::~A[], ~A)"
                                ids
                                (cond
                                  ((string-equal pk-type "bigserial")
                                   "bigint")
                                  ((string-equal pk-type "serial")
                                   "integer")
                                  (t pk-type))
                                pk-name))))
         #()))))


(defun map-by-id (dao-objects)
  (loop with result = (make-hash-table)
        for obj in dao-objects
        do (setf (gethash (object-id obj) result)
                 obj)
        finally (return result)))
