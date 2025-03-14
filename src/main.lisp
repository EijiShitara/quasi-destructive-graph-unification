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

;; generic version
(defmacro find-arc-with-label (label arc-lst)
  `(find ,label ,arc-lst
         :test #'eq
         :key #'(lambda (arc) (arc-label arc))))

(defmacro find-arc-with-label (label arc-lst)
  (declare (type symbol label)
           (type list arc-lst))
  `(assoc ,label ,arc-lst :test #'eq))

(defmacro simple-copy-arc (arc)
  (declare (type dgarc arc))
  `(create-arc :label (arc-label ,arc)
               :value (simple-copy-dgnode (arc-value ,arc))))

(defmacro identical-atomic-dgnodep (dg1 dg2)
  (declare (type dgnode dg1 dg2))
  `(eq (dgnode-arc-list ,dg1) (dgnode-arc-list ,dg2)))

(defmacro dgnode-arc-labels (dgnode)
  (declare (type dgnode dgnode))
  `(when (complexnode-p ,dgnode)
     (mapcar #'(lambda (arc)
                 (arc-label arc))
             (dgnode-arc-list ,dgnode))))

(defmacro return-real-arc (label dgnode)
  (declare (type symbol label)
           (type dgnode dgnode))
  `(if (and (dgnode-comp-arc-list ,dgnode)
            (= *unify-global-counter*
               (dgnode-generation ,dgnode)))
       (or (find ,label (dgnode-arc-list ,dgnode)
                 :test #'eq
                 :key #'(lambda (a) (arc-label a)))
           (find ,label (dgnode-comp-arc-list ,dgnode)
                 :test #'eq
                 :key #'(lambda (a) (arc-label a))))
       (find ,label (dgnode-arc-list ,dgnode)
             :test #'eq
             :key #'(lambda (a) (arc-label a)))))

(defmacro set-temporary-forward-dgnode (dgnode1 dgnode2)
  (declare (type dgnode dgnode1 dgnode2)
           (special *unify-global-counter*))
  `(unless (or (eq ,dgnode1 ,dgnode2)
               (= (dgnode-generation dgnode1) 9))
     (setf (dggnode-forward ,dgnode1) ,gdnode2)
     (setf (dgnode-generation ,dgnode1) *unify-global-counter*)))

(defmacro set-permanent-forward-dgnode (dgnode1 dgnode2)
  (declare (type dgnode dgnode1 dgnode2))
  `(unless (eq ,dgnode1 ,dgnode2)
     (setf (dggnode-forward ,dgnode1) ,gdnode2)
     (setf (dgnode-generation ,dgnode1) 9)))

(defmacro forward-dg (dg1 dg2 &optional (type :temporary))
  (declare (type dgnode dg1 dg2)
           (type symbol type))
  `(case ,type
     (:temporary (set-temporary-forward-dgnode ,dg1 ,dg2))
     (:permanent (set-permanent-forward-dgnode ,dg1 ,dg2))))

(defmacro dereference-dg (dg-input)
  (declare (type dgnode dg-input))
  `(do ((result ,dg-input dg)
        (dg ,dg-input
            (if (and (dgnode-forward dg)
                     (or (= (dgnode-generation dg) *unify-global-counter*)
                         (= (dgnode-generation dg) 9)))
                (dgnode-forward dg)
                (setf (dgnode-forward dg) nil))))
       ((null dg) result)
     (declare (type dgnode result dg))))

(defmacro map-dolist (varlist body)
  (let ((map-result (gensym)))
    `(let ((,map-result nil))
       (dolist ,varlist (push ,body ,map-result))
       (nreverse ,map-result))))
           
;;;;
(defmacro graph-unify (dg1 dg2 &optional result)
  `(unify-dg ,dg1 ,dg2 ,result))

