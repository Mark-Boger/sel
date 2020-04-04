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
   (%server-host :initarg :host
                 :reader host
                 :type (simple-array character *))
   (%server-port :initarg :port
                 :reader port
                 :type integer)
   (%)
   (%status :initarg :status
            :initform nil
            :reader open-p)))

(defun make-session (&key (host *webdriver-default-host*) (port *webdriver-default-port*))
  (let ((s (http-request (format nil "~A:~A/session" host port)
                         :method :post
                         :content-type "application/json"
                         :content "{\"capabilities\" : { \"alwaysMatch\" : {\"browserName\" : \"firefox\"}}}"
                         :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format s) :utf-8)
    (let ((vals (gethash "value" (yason:parse s))))
      (make-instance 'session :id (gethash "sessionId" vals)
                              :capabilities (gethash "capabilities" vals)
                              :host host
                              :port port
                              :status t))))

(defun destroy-session (session)
  (http-request 
   :method :delete))
