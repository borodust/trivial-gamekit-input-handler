(cl:defpackage :trivial-gamekit.input-handler
  (:nicknames :gamekit.input-handler)
  (:use :cl)
  (:export #:input-handler
           #:gamepad
           #:gamepad-connected
           #:gamepad-disconnected
           #:button-pressed
           #:button-released
           #:dpad-changed
           #:cursor-position
           #:left-stick-position
           #:right-stick-position
           #:left-trigger-value
           #:right-trigger-value
           #:pressed-buttons
           #:activate-input-handler
           #:deactivate-input-handler))
(cl:in-package :trivial-gamekit.input-handler)


(defvar *gamepad* nil)


(defstruct gamepad
  (left-stick (gamekit:vec2 0 0))
  (right-stick (gamekit:vec2 0 0))
  (left-trigger 0)
  (right-trigger 0))


(defclass input-handler ()
  ((bag :initform nil)
   (cursor :initform (gamekit:vec2 0 0))
   (gamepad-map :initform (make-hash-table))))


(defgeneric button-pressed (input-handler button)
  (:method (input-handler button) (declare (ignore input-handler button))))


(defgeneric button-released (input-handler button)
  (:method (input-handler button) (declare (ignore input-handler button))))


(defgeneric dpad-changed (input-handler state)
  (:method (input-handler state) (declare (ignore input-handler state))))


(defgeneric gamepad-connected (input-handler gamepad)
  (:method (input-handler gamepad) (declare (ignore input-handler gamepad))))


(defgeneric gamepad-disconnected (input-handler gamepad)
  (:method (input-handler gamepad) (declare (ignore input-handler gamepad))))


(defun gamepad ()
  *gamepad*)


(defun cursor-position (input-handler)
  (slot-value input-handler 'cursor))


(defun %find-gamepad (input-handler gamepad)
  (with-slots (gamepad-map) input-handler
    (if gamepad
        (gethash gamepad gamepad-map)
        (loop for g being the hash-value of gamepad-map
                thereis g))))


(defun left-stick-position (input-handler &optional gamepad)
  (bodge-util:when-let ((gamepad (%find-gamepad input-handler gamepad)))
    (gamepad-left-stick gamepad)))


(defun right-stick-position (input-handler &optional gamepad)
  (bodge-util:when-let ((gamepad (%find-gamepad input-handler gamepad)))
    (gamepad-right-stick gamepad)))


(defun left-trigger-value (input-handler &optional gamepad)
  (bodge-util:when-let ((gamepad (%find-gamepad input-handler gamepad)))
    (gamepad-left-trigger gamepad)))


(defun right-trigger-value (input-handler &optional gamepad)
  (bodge-util:when-let ((gamepad (%find-gamepad input-handler gamepad)))
    (gamepad-right-trigger gamepad)))


(defun pressed-buttons (input-handler)
  (slot-value input-handler 'bag))


(defun gamepad-button->input (button)
  (case button
    (:a :gamepad-a)
    (:b :gamepad-b)
    (:x :gamepad-x)
    (:y :gamepad-y)
    (:left-bumper :gamepad-left-bumper)
    (:right-bumper :gamepad-right-bumper)
    (:start :gamepad-start)
    (:back :gamepad-back)
    (:guide :gamepad-guide)
    (:left-thumb :gamepad-left-thumb)
    (:right-thumb :gamepad-right-thumb)))


(defvar +dpad-states+ '(:up :down :left :right
                        :right-up :right-down :left-up :left-down
                        :centered))


(defun %bind-gamepad-input (input-handler)
  (with-slots (gamepad-map bag) input-handler
    (flet ((process-button (gamepad button state)
             (let ((input-button (gamepad-button->input button)))
               (case state
                 (:pressed
                  (push input-button bag)
                  (let ((*gamepad* gamepad))
                    (button-pressed input-handler input-button)))
                 (:released
                  (setf bag (delete input-button bag))
                  (let ((*gamepad* gamepad))
                    (button-released input-handler input-button))))))
           (dpad-processor (state)
             (lambda (gamepad)
               (let ((*gamepad* gamepad))
                 (dpad-changed input-handler state)))))
      (gamekit:bind-gamepad-any-button #'process-button)
      (loop for state in +dpad-states+
            do (gamekit:bind-gamepad-dpad state (dpad-processor state)))
      (labels ((%update-stick-fu (accessor)
                 (lambda (gamepad x y)
                   (bodge-util:when-let ((gamepad (gethash gamepad gamepad-map)))
                     (let ((coords (funcall accessor gamepad)))
                       (setf (gamekit:x coords) x
                             (gamekit:y coords) y)))))
               (%update-left-trigger (gamepad value)
                 (bodge-util:when-let ((gamepad (gethash gamepad gamepad-map)))
                   (setf (gamepad-left-trigger gamepad) value)))
               (%update-right-trigger (gamepad value)
                 (bodge-util:when-let ((gamepad (gethash gamepad gamepad-map)))
                   (setf (gamepad-right-trigger gamepad) value))))
        (gamekit:bind-gamepad-stick :left
                                    (%update-stick-fu #'gamepad-left-stick))
        (gamekit:bind-gamepad-stick :right
                                    (%update-stick-fu #'gamepad-right-stick))
        (gamekit:bind-gamepad-trigger :left #'%update-left-trigger)
        (gamekit:bind-gamepad-trigger :right #'%update-right-trigger)))))


(defun %unbind-gamepad-input (input-handler)
  (declare (ignore input-handler))
  (gamekit:bind-gamepad-any-button nil)
  (loop for state in +dpad-states+
        do (gamekit:bind-gamepad-dpad state nil))
  (gamekit:bind-gamepad-stick :left nil)
  (gamekit:bind-gamepad-stick :right nil)
  (gamekit:bind-gamepad-trigger :left nil)
  (gamekit:bind-gamepad-trigger :right nil))


(defun register-gamepad (input-handler gamepad)
  (with-slots (gamepad-map) input-handler
    (setf (gethash gamepad gamepad-map) (make-gamepad))))


(defun remove-gamepad (input-handler gamepad)
  (with-slots (gamepad-map) input-handler
    (remhash gamepad gamepad-map)))


(defun activate-input-handler (input-handler)
  (with-slots (cursor bag) input-handler
    (%bind-gamepad-input input-handler)
    (gamekit:bind-any-gamepad (lambda (gamepad state)
                                (if (eq :connected state)
                                    (progn
                                      (register-gamepad input-handler gamepad)
                                      (gamepad-connected input-handler gamepad))
                                    (progn
                                      (gamepad-disconnected input-handler gamepad)
                                      (remove-gamepad input-handler gamepad)))))
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
    (gamekit:bind-any-button nil)
    (%unbind-gamepad-input input-handler)))
