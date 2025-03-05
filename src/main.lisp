(defparameter *unify-global-counter* 10)

(defun unify-dg (dg1 dg2)
  )

(defun unify-dg0 (dg1 dg2)
  )

(defun unify-dg1 (dg1-underef dg2-underef2)
  )

;; Graph node dereferencing
(defun dereference-dg (dg)
  )

;; Quasi-destructive copying
(defun copy-dg-with-comp-arcs (dg-underef)
  )

(defun copy-arc-and-comp-arc (input-arc)
  )

;; Q-D copying with structure-sharing
(defun copy-dg-with-comp-arcs-share (dg-underef)
  )

(defun copy-node-comp-not-forward (dg)
  )

(defun copy-node-comp-forward (dg)
  )

(defun copy-arc-and-comp-arc-share (input-arc)
  )

;;;
(defvar *features*)
(defparameter *atom-sharing* nil)
(defparameter *str-sharing* nil)
(defparameter *unification* 'quasi-unify)

(proclaim '(type t *atom-sharing* *str-sharing*))

(defvar *debug-stream1* *standard-output*)
(defvar *dgnode-list* nil)
(defvar *unify-global-counter* 10)

(proclaim '(fixnum *unify-global-counter*))

(defvar *dgnodes* 0)
(defvar *dgarcs* 0)
(defvar *unify0* 0)
(defvar *unify1* 0)

(proclaim '(fixnum *dg-nodes* *dg-arcs* *unify0* *unify1*))

;;; data structure

;;; arc-types
(defconstant +normal+ '=)
(defconstant +must-be-present+ '=c)
(defconstant +multiple-valued+ '>)

;;; dggnode creation
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype dgarc () 'cons))

(defmacro create-arc (&key (label nil)
                        (type +normal+)
                        (value nil))
  (declare (type symbol label)
           (type symbol type)
           (type dgnode values)
           (special *dgarcs*))
  `(progn (incf *dgarcs*)
          (cons ,label ,values)))

(defmacro arc-label (arc) `(car ,arc))
(defun arc-label-function (arc) (arc-label arc))
(defmacro arc-calue (arc) `(cdr ,arc))
(defmacro arc-p (arc) `(consp ,arc))

(defun create-dgnode (&key (type :atomic)
                           (arc-list nil))
  (declare (type symbol type)
           (type list arc-list))
  (let ((temp (make-dgnode
               :arc-list arc-list
               :type type)))
    (declare (type dgnode temp))
    ;; (incf *dgnodes*)
    temp))

(defmacro %create-dgnode (&optional (type :atomic))
  (declare (type symbol type)
           (special *dgnodes*))
  `(let ((temp (make-dgnode :type ,type)))
     ;; (push temp *dgnode-list*)
     (declare (type dgnode temp))
     (incf *dgnodes*)
     temp))

;;; Macro definitions
(defmacro atomicnode-p (dgnode)
  (declare (type dgnode dgnode))
  `(eq (dgnode-type ,dgnode) :atomic))

(defmacro leafnode-p (dgnode)
  (declare (type dgnode dgnode))
  `(eq (dgnode-type ,dgnode) :leaf))

(defmacro complexnode-p (dgnode)
  (declare (type dgnode dgnode))
  `(eq (dgnode-type ,dgnode) :complex))

(defmacro find-atomic (arc-list)
  `(find :atomic ,arc-list
         :test #'eq
         :key #'(lambda (arc) (arc-label arc))))

(defmacro find-leaf (arc-list)
  `(find :leaf ,arc-list
         :test #'eq
         :key #'(lambda (arc) (arc-label arc))))

(defmacro find-complex (arc-list)
  `(find :complex ,arc-list
         :test #'eq
         :key #'(lambda (arc) (arc-label arc))))

;;;;
(defmacro graph-unify (dg1 dg2 &optional result)
  `(unify-dg ,dg1 ,dg2 ,result))

