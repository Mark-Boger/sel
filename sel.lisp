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

(defmacro defget (name ((req-string &optional (rets nil)) &rest params) &body body)
  (a:with-gensyms (vals)
    (multiple-value-bind (forms decls docs) (a:parse-body body :documentation t)
      (let ((req-params (a:parse-ordinary-lambda-list params))
            (control-string (concatenate 'string "~A/" req-string)))
        `(defun ,name (session ,@params)
           ,docs
           ,@decls
           (with-results (,(if rets `,rets `,vals)
                          (format nil ,control-string (session-req session) ,@req-params))
             ,(if rets
                  `(progn ,@forms)
                  `,vals)))))))

(defmacro defpost (name ((req-string &optional (rets nil)) &rest params) &body body)
  (a:with-gensyms (vals json req-out json-str)
    (multiple-value-bind (forms decls docs) (a:parse-body body :documentation t)
      (multiple-value-bind (req opt rest keys allow aux keys-exist)
          (a:parse-ordinary-lambda-list params)
        (declare (ignore opt rest allow aux keys-exist))
        (let ((control-string (concatenate 'string "~A/" req-string)))
          `(defun ,name (session ,@params)
             ,docs
             ,@decls
             (let ((,json (make-string-output-stream))
                   (,json-str nil)
                   (,req-out nil))
               (yason:encode (a:plist-hash-table
                              (list ,@(loop :for key :in keys
                                            :append
                                            (let* ((key-name (second (first key)))
                                                   (str (camel-case key-name)))
                                              `(,str ,key-name)))))
                             ,json)
               (setf ,json-str (get-output-stream-string ,json))
               (with-results (,(if rets `,rets `,vals)
                              (format nil ,control-string (session-req session) ,@req)
                              :method :post
                              :content-type "application/json"
                              :content ,json-str)
                 (setf ,req-out ,(if rets
                                     `(progn ,@forms)
                                     `,vals))))))))))

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

(defget get-url (("url")))
(defpost go-to (("url") &key url))

(defget get-timeouts (("timeouts")))
;; TODO: this should probably update the session object that's passed
(defpost set-timeouts (("timeouts") &key (script 30000) (page-load 300000) (implicit 0)))
(defsubs set- (set-timeouts script page-load implicit) -timeout)

(defpost back (("back")))
(defpost forward (("forward")))
(defpost refresh (("refresh")))

(defget get-active-element (("element/active" ele))
  (if (= (length ele) 1)
      (cdr (first ele))
      ele))

(defget get-title (("title")))
(defget get-window-handle (("window")))
(defget get-window-handles (("window/handles")))
(defget get-window-rect (("window/rect")))
(defpost set-window-rect (("window/rect") &key width height x y))
(defsubs set- (set-window-rect width height x y))
(defpost maximize-window (("window/maximize")))
(defpost minimize-window (("window/minimize")))
(defpost fullscreen-window (("window/fullscreen")))
(defpost switch-to-window (("window") &key handle))
(defpost new-window (("window/new") &key type))

(defpost switch-to-frame (("frame")) &key id)
(defpost switch-to-parent-frame (("frame/parent")))

;;;;;;; Mega trash

(defpost find-element (("element" ele) &key using value)
  (if (= (length ele) 1)
      (cdr (first ele))
      ele))
(defsubs find-element-by- (find-element (using ("css selector"
                                                "link text"
                                                "partial link text"
                                                "tag name"
                                                "xpath")
                                               value)))

(defpost find-elements (("elements" eles) &key using value)
  (loop :for (web-id . uuid) :in (mapcar #'first eles)        
        :collect uuid))
(defsubs find-elements-by- (find-elements (using ("css selector"
                                                  "link text"
                                                  "partial link text"
                                                  "tag name"
                                                  "xpath")
                                                 value)))

(defpost from-element (("element/~A/element" ele) element-id &key using value)
  (when ele (cdr (first ele))))
(defsubs from-element-by- (from-element (using ("css selector"
                                                "link text"
                                                "partial link text"
                                                "tag name"
                                                "xpath")
                                               value))
         nil element-id)

(defpost elements-from-element (("element/~A/elements" eles) element-id &key using value)
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

(defpost click (("element/~A/click") element-id))
(defpost clear (("element/~A/clear") element-id))
(defpost type-text (("element/~A/value") element-id &key text))

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

(defpost dismiss-alert (("alert/dismiss")))
(defpost accept-alert (("alert/accept")))
(defpost send-alert-text (("alert/text") &key text))
(defget get-alert-text (("alert/text")))

(defpost print-page (("print")))

(defget take-screenshot (("screenshot" shot) &optional (file-name "~/image"))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image file-name :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop for ele across decoded
            do (write-byte ele image)))))

(defget take-element-screenshot (("element/~A/screenshot" shot) element-id
                                 &optional (file-name "~/element-image"))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image file-name :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop for ele across decoded
            do (write-byte ele image)))))

(defun destroy (session)
  (with-results (vals (session-req session) :method :delete)
    vals))
