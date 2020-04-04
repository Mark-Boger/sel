;;;; sel.lisp

(in-package #:sel)

(defvar *webdriver-default-host* "http://127.0.0.1")
(defvar *webdriver-default-port* 4444)

(defclass session ()
  ((%id :initarg :id
        :reader id
        :initform ""
        :type (simple-array character *))
   (%capabilities :initarg :capabilities
                  :reader capabilities
                  :initform (make-hash-table)
                  :type hash-table)
   (%host :initarg :host
          :reader server-host
          :type (simple-array character *))
   (%port :initarg :port
          :reader server-port
          :type integer)
   (%req :initarg :req
         :reader session-req
         :type (simple-array character *))
   (%status :initarg :status
            :initform nil
            :reader open-p)))

(defmacro with-http-stream ((stream url &rest options) &body body)
  `(let ((,stream (http-request ,url ,@options :want-stream t)))
     (setf (flexi-streams:flexi-stream-external-format ,stream) :utf-8)
     ,@body))

(defmacro with-results ((results url &rest options) &body body)
  (a:with-gensyms (stream)
    `(let (,results)
       (with-http-stream (,stream ,url ,@options)
         (setf ,results (cdr
                         (assoc "value" (yason:parse ,stream :object-as :alist)
                                :test #'string=))))
       ,@body)))

(defmacro with-session ((session) &body body)
  `(let ((,session (make-session)))
     (unwind-protect
          ,@body
       (destroy ,session))))

(defun make-session (&key (host *webdriver-default-host*) (port *webdriver-default-port*))
  (with-results (vals (format nil "~A:~A/session" host port) 
                      :method :post
                      :content-type "application/json"
                      ;;; TODO: make this dynamic
                      :content "{\"capabilities\" : { \"alwaysMatch\" : {\"browserName\" : \"firefox\"}}}")
    (make-instance 'session :id (cdr (assoc "sessionId" vals :test #'string=))
                            :capabilities (cdr (assoc "capabilities" vals :test #'string=))
                            :host host
                            :port port
                            :req (format nil "~A:~A/session/~A" host port
                                         (cdr (assoc "sessionId" vals :test #'string=)))
                            :status t)))

(defun server-status (&optional
                        (host *webdriver-default-host*)
                        (port *webdriver-default-port*))
  (with-results (vals (format nil "~A:~A/status" host port) :method :get)
    (values (cdr (assoc "ready" vals :test #'string=))
            (cdr (assoc "message" vals :test #'string=)))))

(defun navigate-to (session url)
  (let ((url-json (make-string-output-stream)))
    (yason:encode (a:plist-hash-table (list "url" url) :test #'equal) url-json)
    (with-results (vals (format nil "~A/url" (session-req session))
                        :method :post
                        :content-type "application/json"
                        :content (get-output-stream-string url-json))
      vals)))

;; TODO: Make params a real lambda list
;; TODO: Make body parsing a thing
(defmacro defget (name ((req-string &optional (rets nil)) &rest params) &body body)
  (a:with-gensyms (vals)
    (let ((control-string (concatenate 'string "~A/" req-string)))
      `(defun ,name (session ,@params)
         (with-results (,(if rets `,rets `,vals)
                        (format nil ,control-string (session-req session) ,@params))
           ,(if rets
                `(progn ,@body)
                `,vals))))))

(defget get-url (("url")))
(defget get-title (("title")))
(defget get-timeouts (("timeouts")))
(defget get-window-handle (("window")))
(defget get-window-handles (("window/handles")))
(defget get-window-rect (("window/rect")))
(defget element-selected-p (("element/~A/selected") element-id))
(defget get-element-attribute (("element/~A/attribute/~A") element-id attribute-name))
(defget get-element-property (("element/~A/property/~A") element-id property-name))
(defget get-element-css-value (("element/~A/css/~A") element-id css-property-name))
(defget get-element-text (("element/~A/text") element-id))
(defget get-element-tag-name (("element/~A/name") element-id))
(defget get-element-rect (("element/~A/rect") element-id))
(defget element-enabled-p (("element/~A/enabled") element-id))
(defget get-page-source (("source")))
(defget get-cookies (("cookie")))
(defget get-cookie (("cookie/~A") cookie-name))
(defget get-alert-text (("alert/text")))

(defget get-active-element (("element/active" ele))
  (when ele
    (cdr (first ele))))

(defget take-screenshot (("screenshot" shot))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image "~/image" :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop for ele across decoded
            do (write-byte ele image)))))

(defget take-element-screenshot (("element/~A/screenshot" shot) element-id)
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image "~/element-image" :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create
                                             :element-type '(unsigned-byte 8))
      (loop for ele across decoded
            do (write-byte ele image)))))

(defun destroy (session)
  (with-results (vals (session-req session) :method :delete)
    vals))
