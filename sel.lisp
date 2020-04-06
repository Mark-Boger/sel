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
                  :initform ()
                  :type list)
   (%host :initarg :host
          :reader server-host
          :type (simple-array character *))
   (%port :initarg :port
          :reader server-port
          :type integer)
   (%req :initarg :req
         :reader session-req
         :type (simple-array character *))))

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
          (progn ,@body)
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

(defun split (string &optional (on #\-))
  (let ((ret ())
        (char-accum ()))
    (loop :for char :across string
          :if (char= char on)
            :do (progn (push (nreverse (coerce char-accum 'string)) ret)
                       (setf char-accum nil))
          :else
            :do (push char char-accum)
          :finally (push (nreverse (coerce char-accum 'string)) ret))
    (nreverse ret)))

(defun camel-case (symbol)
  (let* ((string (string-downcase (string symbol)))
         (splits (split string)))
    (format nil "~A~{~:(~A~)~}" (first splits) (rest splits))))

(defmacro defreq (name type ((&optional (req-string "") (rets nil)) &rest params) &body body)
  (a:with-gensyms (vals json)
    (multiple-value-bind (forms decls docs) (a:parse-body body :documentation t)
      (multiple-value-bind (req opt rest keys allow aux keys-exist)
          (a:parse-ordinary-lambda-list params)
        (declare (ignore opt rest allow aux keys-exist))
        (let* ((control-string (concatenate 'string "~A/" req-string))
               (req-stuff `(with-results (,(if rets `,rets `,vals)
                                          (format nil ,control-string
                                                  (session-req session) ,@req)
                                          ,@(case type
                                              (:delete `(:method :delete))
                                              (:post 
                                               `(:method :post
                                                    :content-type "application/json"
                                                  :content (get-output-stream-string ,json)))
                                              (otherwise nil)))
                             ,(if rets
                                  `(progn ,@forms)
                                  `,vals))))
          `(defun ,name (session ,@params)
             ,@docs
             ,@decls
             ,(if (eq type :post)
                  `(let ((,json (make-string-output-stream)))
                     (yason:encode (a:plist-hash-table
                                    (list ,@(loop :for key :in keys
                                                  :append
                                                  (let* ((key-name (second (first key)))
                                                         (str (camel-case key-name)))
                                                    `(,str ,key-name)))))
                                   ,json)
                     ,req-stuff)
                  `,req-stuff)))))))

;;; This sucks so bad; figure out a way to do this better
(defmacro defsubs (base-name (using &rest args) &optional name-ending &rest rest)
  (let ((str-base (string-upcase (string base-name)))
        (str-ending (when name-ending (string-upcase (string name-ending)))))
    `(progn
       ,@(loop :for f :in args
               :when (listp f)
                 :append (let* ((func (first f))
                                (val (first (last f))))
                           (loop :for alt :in (second f)
                                 :collect (let ((str-alt (string-upcase
                                                          (reduce (lambda (x y)
                                                                    (concatenate 'string x
                                                                                 "-" y))
                                                                  (split alt #\Space)))))
                                            `(defun ,(a:symbolicate str-base str-alt)
                                                 (session ,@rest ,val)
                                               (,using session ,@rest
                                                       ,(intern (string func) :keyword)
                                                       ,alt
                                                       ,(intern (string val) :keyword)
                                                       ,val)))))
               :else 
                 :collect (let ((str-f (string-upcase (string f))))
                            `(defun ,(a:symbolicate str-base str-f (if str-ending
                                                                       str-ending
                                                                       ""))
                                 (session ,@rest ,f)
                               (,using session ,@rest ,(intern str-f :keyword) ,f)))))))

(defreq get-url :get (("url")))
(defreq go-to :post (("url") &key url))

(defreq get-timeouts :get (("timeouts")))
;; TODO: this should probably update the session object that's passed
(defreq set-timeouts :post (("timeouts") &key (script 30000) (page-load 300000) (implicit 0)))
(defsubs set- (set-timeouts script page-load implicit) -timeout)

(defreq back :post (("back")))
(defreq forward :post (("forward")))
(defreq refresh :post (("refresh")))

(defreq get-active-element :get (("element/active" ele))
  (if (= (length ele) 1)
      (cdr (first ele))
      ele))

(defreq get-title :get (("title")))
(defreq get-window-handle :get (("window")))
(defreq get-window-handles :get (("window/handles")))
(defreq get-window-rect :get (("window/rect")))
(defreq set-window-rect :post (("window/rect") &key width height x y))
(defsubs set- (set-window-rect width height x y))
(defreq maximize-window :post (("window/maximize")))
(defreq minimize-window :post (("window/minimize")))
(defreq fullscreen-window :post (("window/fullscreen")))
(defreq switch-to-window :post (("window") &key handle))
(defreq new-window :post (("window/new") &key type))

(defreq switch-to-frame :post (("frame")) &key id)
(defreq switch-to-parent-frame :post (("frame/parent")))

;;;;;;; Mega trash

(defreq find-element :post (("element" ele) &key using value)
  (if (= (length ele) 1)
      (cdr (first ele))
      ele))
(defsubs find-element-by- (find-element (using ("css selector"
                                                "link text"
                                                "partial link text"
                                                "tag name"
                                                "xpath")
                                               value)))

(defreq find-elements :post (("elements" eles) &key using value)
  (loop :for (web-id . uuid) :in (mapcar #'first eles)        
        :collect uuid))
(defsubs find-elements-by- (find-elements (using ("css selector"
                                                  "link text"
                                                  "partial link text"
                                                  "tag name"
                                                  "xpath")
                                                 value)))

(defreq from-element :post (("element/~A/element" ele) element-id &key using value)
  (when ele (cdr (first ele))))
(defsubs from-element-by- (from-element (using ("css selector"
                                                "link text"
                                                "partial link text"
                                                "tag name"
                                                "xpath")
                                               value))
         nil element-id)

(defreq elements-from-element :post (("element/~A/elements" eles) element-id &key using value)
  (loop :for (web-id . uuid) :in (mapcar #'first eles)
        :collect uuid))
(defsubs elements-from-element-by- (elements-from-element (using ("css selector"
                                                                  "link text"
                                                                  "partial link text"
                                                                  "tag name"
                                                                  "xpath")
                                                                 value))
         nil element-id)

;;;;;;;;;;

(defreq click :post (("element/~A/click") element-id))
(defreq clear :post (("element/~A/clear") element-id))
(defreq type-text :post (("element/~A/value") element-id &key text))

(defreq element-selected-p :get (("element/~A/selected") element-id))
(defreq get-element-attribute :get (("element/~A/attribute/~A") element-id attribute-name))
(defreq get-element-property :get (("element/~A/property/~A") element-id property-name))
(defreq get-element-css-value :get (("element/~A/css/~A") element-id css-property-name))
(defreq get-element-text :get (("element/~A/text") element-id))
(defreq get-element-tag-name :get (("element/~A/name") element-id))
(defreq get-element-rect :get (("element/~A/rect") element-id))
(defreq element-enabled-p :get (("element/~A/enabled") element-id))

(defreq get-page-source :get (("source")))

(defreq get-cookies :get (("cookie")))
(defreq get-cookie :get (("cookie/~A") cookie-name))

(defreq dismiss-alert :post (("alert/dismiss")))
(defreq accept-alert :post (("alert/accept")))
(defreq send-alert-text :post (("alert/text") &key text))
(defreq get-alert-text :get (("alert/text")))

(defreq print-page :post (("print")))

(defreq take-screenshot :get (("screenshot" shot) &optional (file-name "~/image"))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image file-name :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop for ele across decoded
            do (write-byte ele image)))))

(defreq take-element-screenshot :get (("element/~A/screenshot" shot) element-id
                                      &optional (file-name "~/element-image"))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image file-name :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop for ele across decoded
            do (write-byte ele image)))))

(defreq destroy :delete (()))
