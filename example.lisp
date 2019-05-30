(cl:defpackage :trivial-gamekit.input-handler.example
  (:use :cl)
  (:export #:run))

(cl:in-package :trivial-gamekit.input-handler.example)

(defun switch-input-handler (new-input-handler)
  (with-slots (active-input-handler) (gamekit:gamekit)
    (when active-input-handler
      (gamekit.input-handler:deactivate-input-handler active-input-handler))
    (setf active-input-handler new-input-handler)
    (gamekit.input-handler:activate-input-handler active-input-handler)))


(defclass direction ()
  ((direction :initform nil :accessor direction-of)))


(defclass wasd-handler (gamekit.input-handler:input-handler direction) ())


(defmethod gamekit.input-handler:button-pressed ((this wasd-handler)
                                                 (button (eql :w)))
  (setf (direction-of this) :up))


(defmethod gamekit.input-handler:button-pressed ((this wasd-handler)
                                                 (button (eql :a)))
  (setf (direction-of this) :left))


(defmethod gamekit.input-handler:button-pressed ((this wasd-handler)
                                                 (button (eql :s)))
  (setf (direction-of this) :down))


(defmethod gamekit.input-handler:button-pressed ((this wasd-handler)
                                                 (button (eql :d)))
  (setf (direction-of this) :right))


(defmethod gamekit.input-handler:button-pressed ((this wasd-handler)
                                                 (button (eql :enter)))
  (switch-input-handler (make-instance 'arrows-handler)))


(defclass arrows-handler (gamekit.input-handler:input-handler direction) ())


(defmethod gamekit.input-handler:button-pressed ((this arrows-handler) button)
  (when (member button '(:up :down :left :right))
    (setf (direction-of this) button))
  (when (eq button :enter)
    (switch-input-handler (make-instance 'wasd-handler))))


(gamekit:defgame input-handler-example ()
  ((active-input-handler :initform nil))
  (:viewport-title "Input Handler Example"))


(defmethod gamekit:post-initialize ((this input-handler-example))
  (switch-input-handler (make-instance 'wasd-handler)))


(defmethod gamekit:draw ((this input-handler-example))
  (with-slots (active-input-handler) this
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 2 2)
      (gamekit:draw-text "PRESS ENTER TO SWITCH INPUT SCHEME" (gamekit:vec2 55 10))
      (bodge-util:when-let ((direction (direction-of active-input-handler)))
        (gamekit:translate-canvas 180 150)
        (gamekit:rotate-canvas (case direction
                                 (:up (/ pi 2))
                                 (:down (- (/ pi 2)))
                                 (:left pi)
                                 (:right 0)))
        (gamekit:translate-canvas 10 -4)
        (gamekit:draw-text "=>" (gamekit:vec2 0 0))))))

(cl:in-package :trivial-gamekit.input-handler.example)

(defun run ()
  (gamekit:start 'input-handler-example))
