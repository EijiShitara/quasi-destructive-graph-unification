(defpackage #:qa
  (:use #:cl)
  (:export #:node
           #:node-type
           #:node-arc-list
           #:node-comp-arc-list
           #:node-forword
           #:node-copy
           #:node-generation
           #:arc
           #:arc-label
           #:arc-values))

(defclass node ()
  ((node-type          :initarg :type
                       :accessor node-type)
   (node-arc-list      :initarg :arc-list
                       :accessor node-arc-list)
   (node-comp-arc-list :initarg :comp-arc-list
                       :accessor node-comp-arc-list)
   (node-forward       :initarg :forward
                       :accessor node-forward)
   (node-copy          :initarg :copy
                       :accessor node-copy)
   (node-generation    :initarg :generation
                       :accessor node-generation)))

(defclass arc ()
  ((arc-label  :initarg :lavel
               :accessor arc-label)
   (arc-values :initarg :values
               :accessor arc-values)))
