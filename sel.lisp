;;;; sel.lisp

(in-package #:sel)

(defvar *webdriver-default-host* "http://127.0.0.1")
(defvar *webdriver-default-port* 4444)

(eval-when (:compile-toplevel :load-toplevel :execute)
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

  (defun make-control-string (string)
    (when (string= string "")
      (return-from make-control-string (values "~A" ())))
    (let* ((splits (if (char= (aref string 0) #\/)
                       (split (subseq string 1) #\/)
                       (split string #\/)))
           (variables ())
           (str (with-output-to-string (s)
                  (format s "~~A")
                  (dolist (ele splits)
                    (if (char= (aref ele 0) #\{)
                        (let ((var (string-upcase (subseq ele 1 (1- (length ele))))))
                          (push (intern var) variables)
                          (format s "/~~A"))
                        (format s "/~A" ele))))))
      (values str (nreverse variables)))))

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
                                         (cdr (assoc "sessionId" vals :test #'string=))))))

(defun server-status (&optional
                        (host *webdriver-default-host*)
                        (port *webdriver-default-port*))
  (with-results (vals (format nil "~A:~A/status" host port) :method :get)
    (values (cdr (assoc "ready" vals :test #'string=))
            (cdr (assoc "message" vals :test #'string=)))))


(defmacro defreq (name type (&optional (req "") &rest params) &body body)
  (a:with-gensyms (vals json)
    (multiple-value-bind (forms decls docs) (a:parse-body body :documentation t)
      (when (and (null docs) (stringp (first forms)))
        (setf docs (first forms))
        (setf forms (rest forms)))
      (multiple-value-bind (requir opt rest keys allow aux keys-exist)
          (a:parse-ordinary-lambda-list params)
        (declare (ignore requir opt rest allow aux keys-exist))
        (let ((req-string (if (listp req) (first req) req))
              (rets (when (listp req) (second req))))
          (multiple-value-bind (control-string vars) (make-control-string req-string)
            (let* ((req-stuff `(with-results
                                   (,(if rets `,rets `,vals)
                                    (format nil ,control-string
                                            (session-req session) ,@vars)
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
              `(defun ,name (session ,@vars ,@params)
                 ,docs
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
                      `,req-stuff)))))))))

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

(defreq get-url :get ("url")
  "Get the current url that the given session is at.")

(defreq go-to :post ("url" &key url)
  "Go to URL using SESSION.")

(defreq get-timeouts :get ("timeouts")
  "Get the current timeouts used by SESSION")
;; TODO: this should probably update the session object that's passed
(defreq set-timeouts :post ("timeouts" &key (script 30000) (page-load 300000) (implicit 0))
  "Set SESSIONs SCRIPT PAGE-LOAD and IMPLICIT timouts")
(defsubs set- (set-timeouts script page-load implicit) -timeout)

(defreq back :post ("back"))
(defreq forward :post ("forward"))
(defreq refresh :post ("refresh"))

(defreq get-active-element :get (("element/active" ele))
  "Get the currently active element in SESSION"
  (if (= (length ele) 1)
      (cdr (first ele))
      ele))

(defreq get-title :get ("title"))
(defreq get-window-handle :get ("window"))
(defreq get-window-handles :get ("window/handles"))
(defreq get-window-rect :get ("window/rect"))
(defreq set-window-rect :post ("window/rect" &key width height x y))
(defsubs set- (set-window-rect width height x y))
(defreq maximize-window :post ("window/maximize"))
(defreq minimize-window :post ("window/minimize"))
(defreq fullscreen-window :post ("window/fullscreen"))
(defreq switch-to-window :post ("window" &key handle))
(defreq new-window :post ("window/new" &key type))

(defreq switch-to-frame :post ("frame") &key id)
(defreq switch-to-parent-frame :post ("frame/parent"))

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

(defreq from-element :post (("element/{element-id}/element" ele) &key using value)
  (when ele (cdr (first ele))))
(defsubs from-element-by- (from-element (using ("css selector"
                                                "link text"
                                                "partial link text"
                                                "tag name"
                                                "xpath")
                                               value))
         nil element-id)

(defreq elements-from-element :post (("element/{element-id}/elements" eles) &key using value)
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

(defreq click :post ("element/{element-id}/click"))
(defreq clear :post ("element/{element-id}/clear"))
(defreq type-text :post ("element/{element-id}/value" &key text))

(defreq element-selected-p :get ("element/{element-id}/selected"))
(defreq get-element-attribute :get ("element/{element-id}/attribute/{attribute-id}"))
(defreq get-element-property :get ("element/{element-id}/property/{property-name}"))
(defreq get-element-css-value :get ("element/{element-id}/css/{css-property-name}"))
(defreq get-element-text :get ("element/{element-id}/text"))
(defreq get-element-tag-name :get ("element/{element-id}/name"))
(defreq get-element-rect :get ("element/{element-id}/rect"))
(defreq element-enabled-p :get ("element/{element-id}/enabled"))

(defreq get-page-source :get ("source"))

(defreq get-cookies :get ("cookie"))
(defreq get-cookie :get ("cookie/{cookie-name}"))

(defreq dismiss-alert :post ("alert/dismiss"))
(defreq accept-alert :post ("alert/accept"))
(defreq send-alert-text :post ("alert/text" &key text))
(defreq get-alert-text :get ("alert/text"))

(defreq print-page :post ("print"))

(defreq take-screenshot :get (("screenshot" shot) &optional (file-name "~/image"))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image file-name :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop :for ele :across decoded
            :do (write-byte ele image)))))

(defreq take-element-screenshot :get (("element/{element-id}/screenshot" shot)
                                      &optional (file-name "~/element-image"))
  (let ((decoded (qbase64:decode-string shot)))
    (with-open-file (image file-name :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
      (loop :for ele :across decoded
            :do (write-byte ele image)))))

(defreq destroy :delete ())
