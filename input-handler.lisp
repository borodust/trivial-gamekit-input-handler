(cl:defpackage :trivial-gamekit.input-handler
  (:nicknames :gamekit.input-handler)
  (:use :cl)
  (:export #:input-handler
           #:activate-input-handler
           #:button-pressed
           #:button-released
           #:cursor-position
           #:pressed-buttons
           #:deactivate-input-handler))
(cl:in-package :trivial-gamekit.input-handler)


(defclass input-handler ()
  ((bag :initform nil)
   (cursor :initform (gamekit:vec2 0 0))))


(defgeneric button-pressed (input-handler button)
  (:method (input-handler button) (declare (ignore input-handler button))))


(defgeneric button-released (input-handler button)
  (:method (input-handler button) (declare (ignore input-handler button))))


(defun cursor-position (input-handler)
  (slot-value input-handler 'cursor))


(defun pressed-buttons (input-handler)
  (slot-value input-handler 'bag))


(defun activate-input-handler (input-handler)
  (with-slots (cursor bag) input-handler
    (gamekit:bind-cursor (lambda (x y)
                           (setf (gamekit:x cursor) x
                                 (gamekit:y cursor) y)))
    (flet ((process-button (button state)
             (case state
               (:pressed
                (push button bag)
                (button-pressed input-handler button))
               (:released
                (setf bag (delete button bag))
                (button-released input-handler button)))))
      (gamekit:bind-any-button #'process-button))))


(defun deactivate-input-handler (input-handler)
  (with-slots (cursor bag) input-handler
    (setf (gamekit:x cursor) 0
          (gamekit:y cursor) 0)
    (gamekit:bind-cursor nil)
    (setf bag nil)
    (gamekit:bind-any-button nil)))
