;;; author: Vsevolod Dyomkin <vseloved@gmail.com>

;;; apply on top of ASDF v.2.106

;;; Changes:
;;; - removed ERROR-P argument from FIND-SYSTEM and added VERSION and VERSION-P
;;; - added simple variant for version
;;; - added cerror for version conflicts


(defpackage :asdf-utilities (:use :cl))
(defpackage :asdf (:use :cl :asdf-utilities
                        #+mutest :mutest))

(in-package :asdf)



;;; utilities for version handling

(ignore-errors (fmakunbound 'version-satisfies))

(defgeneric* version-satisfies (current-version version version-p)
  (:documentation "Tests, that CURRENT-VERSION is in desired
relation (VERSION-P) with required VERSION.")
  ;; VERSION-P unknown means :EXACT
  (:method (current-version version version-p)
    (version-satisfies current-version version :exact))
  (:method (current-version (version (eql nil)) version-p)
    t)
  (:method (current-version version (version-p (eql 'nil)))
    (version-satisfies current-version version :exact))
  (:method (current-version version (version-p (eql :exact)))
    (equal version current-version))
  (:method (current-version version (version-p (eql :above)))
    (v> current-version version))
  (:method (current-version version (version-p (eql :major+)))
    (and (equal (major-version version) (major-version current-version))
         (v> current-version version)))
  (:method (current-version version (version-p (eql :minor+)))
      (and (equal (minor-version version) (minor-version current-version))
           (v> current-version version))))

(defmethod version-satisfies :around (current-version version version-p)
  (call-next-method (proper-version current-version)
                    (proper-version version)
                    version-p))

#+mutest
(deftest version-satisfies ()
  '(nil nil nil)              t
  '(nil nil :above)           t
  '(nil nil :major+)          t
  '(nil nil :minor+)          t
  '((0 0 0) nil nil)          t
  '((0 0 0) nil :above)       t
  '((0 0 0) nil :major+)      t
  '((0 0 0) nil :minor+)      t
  '((1 2 3) (1 2 3) nil)      t
  '((1 2 4) (1 2 3) nil)      nil
  '(nil (1 2 3) nil)          nil
  '(nil '(1 2 3) nil)         t  ; (proper-version ''(1 2 3)) -> (0 0 0)
  '((1 2 3) (1 2 3) :above)   t
  '((1 2 4) (1 2 3) :above)   t
  '((1 2 2) (1 2 3) :above)   nil
  '((1 2 3) (1 2 3) :major+)  t
  '((2 2 3) (1 2 3) :major+)  nil
  '((1 0 3) (1 2 3) :major+)  nil
  '((1 2 4) (1 2 3) :major+)  t
  '((1 3 4) (1 2 3) :major+)  t
  '((1 2 3) (1 2 3) :minor+)  t
  '((2 2 3) (1 2 3) :minor+)  nil
  '((1 0 3) (1 2 3) :minor+)  nil
  '((1 2 4) (1 2 3) :minor+)  t
  '((1 3 4) (1 2 3) :minor+)  nil)


;; instead of version-p second part of interval (non-inclusive)
;; ??? is there demand for this? --vseloved

(flet ((inside-interval (v v1 v2)
         ;; non-inclusive interval top (as usual in Lisp)
         (let ((v2 (proper-version v2))
               bot top)
           (if (v> v2 v1)
               (setf bot v1 top v2)
               (setf bot v2 top v1))
           (and (v> v v1)
                (v> v2 v)))))
  (defmethod version-satisfies (current-version version (version-p integer))
    (inside-interval current-version version version-p))
  (defmethod version-satisfies (current-version version (version-p list))
    (inside-interval current-version version version-p))
  (defmethod version-satisfies (current-version version (version-p string))
    (inside-interval current-version version version-p)))

#+mutest
(deftest version-satisfies ()
  '(nil nil nil)              t
  '(nil nil (1 4 1))          t
  '((1 2 3) nil nil)          t
  '((1 2 4) (1 2 3) (1 4 1))  t
  '((1 2 2) (1 2 3) (1 4 1))  nil)


(defgeneric* proper-version (version-spec)
  (:documentation "Proper version is a list of not less than 3 integers.")
  (:method ((spec list))
    (if (and (every #'integerp spec) (> (length spec) 2))
        spec
        (flet ((parse-version-part (spec)
                 (typecase spec
                   (integer spec)
                   (string  (or (parse-integer spec :junk-allowed t) 0))
                   (float   (floor spec))
                   (t       0))))
          (nconc (list (parse-version-part (car spec))
                       (parse-version-part (cadr spec)))
                 (aif (cddr spec)
                      (mapcar #'parse-version-part it)
                      (list 0))))))
  (:method ((spec string))
    (proper-version (split-string spec :separator '(#\.))))
  (:method ((spec integer))
    (list spec 0 0))
  (:method (spec)
    '(0 0 0)))

#+mutest
(deftest proper-version (:test #'equal)
    '(1)            '(1 0 0)
    '("a")          '(0 0 0)
    '("1a")         '(1 0 0)
    '(nil)          '(0 0 0)
    '((1))          '(1 0 0)
    '((1 2))        '(1 2 0)
    '((1 2 3))      '(1 2 3)
    '((1 2 3 4))    '(1 2 3 4)
    '((1 2 "3" 4))  '(1 2 3 4))

(defun major-version (spec)
  "First number in version SPEC."
  (car (proper-version spec)))

(defun minor-version (spec)
  "Second number in version SPEC."
  (let ((v (proper-version spec)))
    (list (car v) (cadr v))))

(defun v> (v1 v2)
  "Test if V1 is not less, than V2."
  (loop :for x1 :in (proper-version v1) :and x2 :in (proper-version v2) :do
     (cond ((> x1 x2) (return-from v> t))
           ((> x2 x1) (return-from v> nil))))
  (>= (length v1) (length v2)))

#+mutest
(deftest v> ()
  '((1 2 3) (1 2 3))    t
  '((1 2 3) (1 2 3 4))  nil
  '((1 2 3) (1 2 2))    t
  '((1 2 3) (1 2 4))    nil
  '((1 2 3) (1 2))      t
  '((1 2) (1 2 3))      nil
  '((1 2) (1 2 0))      nil
  '((1 2) (2))          nil
  '((2) (1 9 9))        t
  '(nil nil)            t
  '(nil (1))            nil
  '((0) nil)            t)

(defmethod slot-unbound (class (instance system) (slot (eql 'version)))
  '(0 0 0))


;;; new find-component

(ignore-errors (fmakunbound 'find-component))

(defgeneric* find-component (base path &optional version version-p)
  (:documentation "Finds the component with PATH starting from BASE module;
if BASE is nil, then the component is assumed to be a system.")
  (:method ((base string) path &optional version version-p)
    (aif (find-system base version version-p)
         (find-component it path version version-p)))
  (:method ((base symbol) path &optional version version-p)
    (cond
      (base (find-component (coerce-name base) path version version-p))
      (path (find-component path nil version version-p))
      (t    nil)))
  (:method ((base cons) path &optional version version-p)
    (find-component (car base) (cons (cdr base) path) version version-p))
  (:method ((module module) (name string) &optional version version-p)
    (declare (ignore version version-p))
    (unless (slot-boundp module 'components-by-name)  ; SBCL may miss the u-i-f-r-c method!!!
      (compute-module-components-by-name module))
    (gethash name (module-components-by-name module)))
  (:method ((component component) (name symbol) &optional version version-p)
    (if name
        (find-component component (coerce-name name) version version-p)
        component))
  (:method ((module module) (name cons) &optional version version-p)
    (find-component (find-component module (car name) version version-p)
                    (cdr name)
                    version version-p)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter +parse-component-keys+
    '(components pathname default-component-class
      perform explain output-files operation-done-p
      weakly-depends-on depends-on serial in-order-to
      version version-p
      &allow-other-keys)
    "Keys, that receive special treatment in PARSE-COMPONENT-FORM."))

(let ((*read-eval* t))
  (defun parse-component-form (parent options)
    ;; FIXME: add docstring
    (destructuring-bind (type name &rest rest &key . #.+parse-component-keys+)
        options
      (declare (ignorable perform explain output-files operation-done-p))
      (check-component-input type name weakly-depends-on depends-on
                             components in-order-to)
      (when (and parent
                 (find-component parent name version version-p)
                 ;; ignore the same object when rereading the defsystem
                 (not (typep (find-component parent name version version-p)
                             (class-for-type parent type))))
        (error 'duplicate-names :name name))
      (let* ((other-args (remove-keys +parse-component-keys+ rest))
             (ret (or (find-component parent name version version-p)
                      (make-instance (class-for-type parent type)))))
        (when weakly-depends-on
          (appendf depends-on
                   (remove-if (complement #'find-system) weakly-depends-on)))
        (when *serial-depends-on*
          (push *serial-depends-on* depends-on))
        (apply #'reinitialize-instance ret
               :name (coerce-name name)
               :pathname pathname
               :parent parent
               other-args)
        (component-pathname ret)  ; eagerly compute the absolute pathname
        (when (typep ret 'module)
          (setf (module-default-component-class ret)
                (or default-component-class
                    (and (typep parent 'module)
                         (module-default-component-class parent))))
          (let ((*serial-depends-on* nil))
            (setf (module-components ret)
                  (loop
                     :for c-form :in components
                     :for c = (parse-component-form ret c-form)
                     :for name = (component-name c)
                     :collect c
                     :when serial :do (setf *serial-depends-on* name))))
          (compute-module-components-by-name ret))

        (setf (component-load-dependencies ret) depends-on) ;; Used by POIU
        (setf (component-in-order-to ret)
              (union-of-dependencies in-order-to
                                     `((compile-op (compile-op ,@depends-on))
                                       (load-op (load-op ,@depends-on)))))
        (setf (component-do-first ret) `((compile-op (load-op ,@depends-on))))

        (%refresh-component-inline-methods ret rest)
        ret))))


;;; new find-system

(defun timestamp-elt (pair) "Alias for CAR." (car pair))
(defun sysdef-elt    (pair) "Alias for CDR." (cdr pair))
(defsetf timestamp-elt rplaca)
(defsetf sysdef-elt    rplacd)

(define-condition system-version-conflict (operation-error)
  ((name :initarg :name :reader component-name)
   (version :initarg :version
            :reader component-version)
   (conflict-version :initarg :conflict-version
                     :reader component-conflict-version))
  (:documentation "There's a conflict of already loaded system and
the same system to be loaded with different version.")
  (:report (lambda (c s)
             (format s "~@<Version conflict for system ~A: v~{.~A~} already ~
present, v~{.~A~} to be loaded~@:>"
                     (component-name c)
                     (component-version c)
                     (component-conflict-version c)))))

(defun coerce-name (object)
  "From OBJECT, representing a system, determine its string name."
  (typecase object
    (null nil)
    (component (component-name object))
    (symbol (string-downcase (symbol-name object)))
    (string object)
    (t (sysdef-error "~@<invalid component designator ~A~@:>" object))))

#+mutest
(deftest coerce-name (:test #'string=
                      :eq-test (lambda (x y)
                                 (or (and (eql (class-of x) 'system)
                                          (eql (class-of y) 'system)
                                          (string= (component-name x)
                                                   (component-name y))
                                          (equal (component-version x)
                                                 (component-version y)))
                                     (equal x y))))
  `(,(make-instance 'system :name "foo")) "foo"
  '(foo)                                  "foo"
  '("foo")                                "foo"
  '(nil)                                    nil)


(defun system-registered-p (name &optional version version-p)
  "Return a pair of atime and system object, if system's NAME (with optionally
satisfying VERSION and VERSION-P) is registered in *defined-systems*, or NIL,
is not found there.  If only VERSION doesn't satisfy, 2nd value is the currenty
registered version."
  (aif (gethash (coerce-name name) *defined-systems*)
       (let ((inmemory-version (component-version (sysdef-elt it))))
         (if (version-satisfies inmemory-version version version-p)
             it
             (values nil
                     inmemory-version)))))

#+mutest
(defun registered-system-test (rez expected)
  (case (car expected)
    (:some-timestamp (and (sysdef-elt rez)
                          (eql (class-of (sysdef-elt rez)) (find-class 'system))
                          (string= (sysdef-elt expected)
                                   (slot-value (sysdef-elt rez) 'name))))
    (:values (equal (cdr expected) rez))
    (t (null expected))))

#+mutest
(defmacro w/registered-systems ((&rest names-and-params) &body body)
  `(let ((*defined-systems* (make-defined-systems-table)))
     (mapcar (lambda (n-&-p)
               (register-system (car n-&-p)
                                (make-instance
                                 'system :name (car n-&-p)
                                 :source-file (getf (cdr n-&-p) :source-file)
                                 :version (proper-version
                                           (getf (cdr n-&-p) :version)))))
             (list ,@names-and-params))
     ,@body))

#+mutest
(deftest system-registered-p (:test #'registered-system-test
                              :setup '(w/registered-systems ('("foo"))
                                        :body))
  '(nil)             nil
  '("bar")           nil
  '("foo")         '(:some-timestamp . "foo")
  '("foo" (1 2 3)) '(:values nil (0 0 0)))

#+mutest
(deftest system-registered-p (:test #'registered-system-test
                              :setup '(w/registered-systems
                                        ('("foo" :version "1.2.3"))
                                        :body))
  '("foo")                '(:some-timestamp . "foo")
  '("foo" "1.2.3")        '(:some-timestamp . "foo")
  '("foo" (1 2 2) :above) '(:some-timestamp . "foo")
  '("foo" (1 2 4))        '(:values nil (1 2 3))
  '("bar" (1 2 3))        nil)


(defun register-system (name system)
  "Add or update SYSTEM's entry in *DEFINED-SYSTEMS* by NAME.
The entry is a pair of atime and SYSTEM object,
it has convenience accessor functions TIMESTAMP-ELT and SYSDEF-ELT."
  (when (system-registered-p name)
    (asdf-message "System ~A already registered." name))
  (asdf-message "~&~@<; ~@;registering ~A as ~A v~{.~A~}~@:>~%"
                system name (component-version system))
  (setf (gethash (coerce-name name) *defined-systems*)
        (cons (get-universal-time) system)))

(defun unregister-system (name)
  "Remove system's entry in *DEFINED-SYSTEMS* by NAME."
  (if (system-registered-p name)
      (progn (asdf-message "~&~@<; ~@;unregistering ~A.~@:>~%" name)
             (remhash (coerce-name name) *defined-systems*))
      (asdf-message "System ~A already registered." name)))

(defun find-system (name &optional version version-p)
  "Find and load appropriate ASD file for system NAME with possibly
provided VERSION and VERSION-Predicate.
If such system isn't found return nil."
  (macrolet ((load-sysdef ()
               `(let ((package (make-temporary-package)))
                  (unwind-protect
                       (progn
                         (handler-bind
                             ((system-version-conflict
                               (lambda (condition)
                                 (invoke-restart (find-restart 'continue
                                                               condition))))
                              (error (lambda (condition)
                                       (error 'load-system-definition-error
                                              :name name :pathname on-disk
                                              :condition condition))))
                           (let ((*package* package))
                             (asdf-message "~&~@<; ~@;loading system ~
definition from ~A into ~A~@:>~%"
                                           on-disk *package*)
                             (load on-disk)))  ; load sysdef file only
                         (let ((in-memory (system-registered-p name)))
                           (register-system name (sysdef-elt in-memory))
                           (%set-system-source-file on-disk
                                                    (sysdef-elt in-memory))
                           (sysdef-elt in-memory)))
                    (delete-package package)))))
    (catch 'find-system
      (let ((name (coerce-name name)))
        (multiple-value-bind (in-memory different-version?)
            (system-registered-p name version version-p)
          (let ((on-disk (system-definition-pathname name version version-p)))
            (when on-disk
              (if in-memory
                  (if (< (timestamp-elt in-memory)
                         (safe-file-write-date on-disk))
                      (load-sysdef)
                      (sysdef-elt in-memory))
                  (if different-version?
;                      (handler-case
                          (progn ; ask to reload system with different version
                            (cerror "Use new system definition v~{.~A~}, ~
instead of the old one (v~{.~A~})?"
                                    'system-version-conflict
                                    (proper-version version) different-version?
                                    :name name
                                    :version different-version?
                                    :conflict-version (proper-version version))
                            (load-sysdef))
;                        (system-version-conflict ()
;                          #| return nil, i.e. system not found |# ))
                      (load-sysdef))))))))))

(defun sysdef-central-registry-search (system &optional version version-p)
  "Find SYSTEM in directories, present in *CENTRAL-REGISTRY*."
  (let ((name (coerce-name system))
        (to-remove nil)    ; entries, that user will choose to remove
        (to-replace nil))  ; entries, that user will choose to coerce to dir
    (unwind-protect
         (let (candidates)
           ;; collect system candidates
           (dolist (dir *central-registry*)
             (let ((defaults (eval dir)))
               (when defaults
                 (if (directory-pathname-p defaults)
                     (let ((file (probe-asd name defaults)))
                       (when file
                         (pushnew file candidates :test #'equal)))
                     (restart-case
                         (let ((*print-circle* nil))
                           (error (format nil "~@<While searching for system ~
~S: ~S evaluated to ~S which is not a directory.~@:>"
                                          system dir defaults)))
                       (remove-entry-from-registry ()
                         :report
                         "Remove entry from *central-registry* and continue"
                         (push dir to-remove))
                       (coerce-entry-to-directory ()
                         :report (lambda (s)
                                   (format s "Coerce entry to ~a, replace ~a ~
and continue."
                                           (ensure-directory-pathname defaults)
                                           dir))
                         (push (cons dir
                                     (ensure-directory-pathname defaults))
                               to-replace)))))))
           (match-sysdef candidates name version version-p))
      ;; cleanup
      (dolist (dir to-remove)
        (setf *central-registry* (remove dir *central-registry*)))
      (dolist (pair to-replace)
        (let* ((current (car pair))
               (new (cdr pair))
               (position (position current *central-registry*)))
          (setf *central-registry*
                (append (subseq *central-registry* 0 position)
                        (list new)
                        (subseq *central-registry* (1+ position)))))))))

(defun sysdef-source-registry-search (system &optional version version-p)
  "Find SYSTEM in Source registry."
  (ensure-source-registry)
  (let ((name (coerce-name system)))
    (loop :for defaults :in (source-registry)
       :collect (probe-asd name defaults) :into candidates
       :finally (return (match-sysdef (remove nil candidates)
                                      name version version-p)))))

(defun sysdef-find-asdf (system &optional version version-p)
  "Create ASDF system, if required SYSTEM is named ASDF, otherwise return nil."
  (declare (ignore version version-p))
  (let ((name (coerce-name system)))
    (when (equal name "asdf")
      (eval `(defsystem :asdf
               :pathname ,(or *compile-file-truename* *load-truename*)
               :depends-on () :components ())))))

(defun match-sysdef (asd-files name &optional version version-p)
  "Among ASD-FILES find the one, for which NAME and VERSION, VERSION-P satisfy.
If the system with this NAME is already registered and it's version satisfies,
use its file."
  (multiple-value-bind (in-memory different-version?)
      (system-registered-p name version version-p)
    (if in-memory
        (system-source-file (sysdef-elt in-memory))
        (let ((readevaller (get-dispatch-macro-character #\# #\.)))
          (unwind-protect
               (progn
                 (set-dispatch-macro-character #\# #\.
                                               (lambda (stream char arg)
                                                 (declare (ignore char arg))
                                                 (read stream t nil t)))
                 (dolist (asd-file asd-files)
                   (with-open-file (in asd-file)
                     (loop :for form := (read in nil) :while form :do
                        (when (and (eq (car form) 'defsystem)
                                   (string-equal (cadr form) name)
                                   (version-satisfies
                                    (handler-case
                                        (eval (getf (cddr form) :version))
                                      (error () '(0 0 0)))
                                    version version-p))
                          (return-from match-sysdef asd-file))))))
            (set-dispatch-macro-character #\# #\. readevaller))))))

#+mutest
(let ((asd-files '("test/bar-3.0.0/bar.asd"
                   "test/foo/foo.asd"
                   "test/foo-1.2.3/foo.asd"
                   "test/foo-1.2.2/foo.asd")))
  (deftest match-sysdef (:test #'string=
                         :setup `(w/registered-systems
                                     ('("bar" :version "1.2.3"
                                        :source-file "bar.asd"))
                                   (let ((*default-pathname-defaults*
                                          (pathname-directory-pathname
                                           ,(load-pathname))))
                                     :body)))
    `(,asd-files "baz")                 nil
    `(,asd-files "bar")                 "bar.asd"
    `(,asd-files "bar" (2 0 0))         nil
    `(,asd-files "foo")                 "test/foo/foo.asd"
    `(,asd-files "foo" (1 2 3))         "test/foo-1.2.3/foo.asd"
    `(,asd-files "foo" (1 2 2) :above)  "test/foo-1.2.3/foo.asd"
    '(("test/foo-1.2.3/foo.asd"
       "test/foo/foo.asd"
       "test/foo-1.2.2/foo.asd") "foo") "test/foo-1.2.3/foo.asd"))

;;; new defsystem

(defmacro defsystem (name &body options)
  (destructuring-bind (&key (pathname nil pathname-arg-p) (class 'system)
                            version defsystem-depends-on
                            &allow-other-keys)
      options
    (let ((component-options (remove-keys '(defsystem-depends-on class)
                                          options))
          (gversion (gensym))
          (in-memory (gensym))
          (different-version? (gensym)))
      `(let ((,gversion (proper-version ,version)))
         ;; system must be registered before we parse the body, otherwise
         ;; we recur when trying to find an existing system of the same name
         ;; to reuse options (e.g. pathname) from
         ,@(loop :for system :in defsystem-depends-on
             :collect `(find-system ,@(if (listp system) system (list system))))
         (multiple-value-bind (,in-memory ,different-version?)
             (system-registered-p ',name ,gversion)
           (when ,different-version?
             (cerror "Use new system definition v~{.~A~}, instead of old ~
one (v~{.~A~})?"
                     'system-version-conflict ,gversion ,different-version?
                     :name ',name
                     :version ,different-version?
                     :conflict-version ,gversion))
           (if ,in-memory
               (if (eq (type-of (sysdef-elt ,in-memory)) ',class)
                   (setf (timestamp-elt ,in-memory) (get-universal-time))
                   (change-class (sysdef-elt ,in-memory) ',class))
               (register-system ',name (make-instance ',class
                                                      :name ',name
                                                      :version ,gversion)))
           (%set-system-source-file (load-pathname)
                                    (sysdef-elt (system-registered-p
                                                 ',name ,gversion))))
         (parse-component-form nil
                               (list* :module (coerce-name ',name)
                                      :pathname ,(determine-system-pathname
                                                  pathname pathname-arg-p)
                                      ',component-options))))))


;;; changing do-dep to accomodate version-p

(defun %do-one-dep (operation c collect required-op required-c
                    &optional version version-p)
  "Collects a partial plan that results from performing REQUIRED-OP on
  REQUIRED-C, possibly with a VERSION (and VERSION-Predicate)"
  (let* ((dep-c (or (find-component (component-parent c) required-c
                                    version version-p)
                    (if version
                        (error 'missing-dependency-of-version
                               :required-by c
                               :version version
                               :version-p version-p
                               :requires required-c)
                        (error 'missing-dependency
                               :required-by c
                               :requires required-c))))
         (op (make-sub-operation c operation dep-c required-op)))
    (do-traverse op dep-c collect)))

(defun do-one-dep (operation c collect required-op required-c
                   &optional version version-p)
  "This function is a thin, error-handling wrapper around %DO-ONE-DEP.
  Returns a partial plan per that function."
  (loop
    (restart-case
        (return (%do-one-dep operation c collect
                             required-op required-c version version-p))
      (retry ()
        :report (lambda (s)
                  (format s "~@<Retry loading component ~S.~@:>"
                          required-c))
        :test
        (lambda (c)
          ;; (print (list :c1 c (typep c 'missing-dependency)))
          ;; (when (typep c 'missing-dependency)
          ;;   (print (list :c2 (missing-requires c) required-c
          ;;                (equalp (missing-requires c)
          ;;                        required-c))))
          (or (null c)
              (and (typep c 'missing-dependency)
                   (equalp (missing-requires c)
                           required-c))))))))

(defun do-dep (operation c collect op dep)
  "FIXME: add description
Type of arguments uncertain:
   * OP seems to at least potentially be a symbol, rather than an operation
   * DEP is a list of component names"
  (if (eq op 'feature)
      (unless (member (car dep) *features*)
        (error 'missing-dependency
               :required-by c
               :requires (car dep))))
      (let ((flag nil))
        (flet ((dep (op comp &optional ver ver-p)
                 (when (do-one-dep operation c collect
                                   op comp ver ver-p)
                   (setf flag t))))
          (dolist (d dep)
            (if (atom d) (dep op d)
                ;; structured dependencies --- this parses keywords
                ;; the keywords could be broken out and cleanly (extensibly)
                ;; processed by EQL methods
                (case (first d)
                  ;; This particular subform is not documented and
                  ;; has always been broken in the past.
                  ;; Therefore no one uses it, and I'm cerroring it out,
                  ;; after fixing it
                  ;; See https://bugs.launchpad.net/asdf/+bug/518467
                  (:feature (cerror "Continue nonetheless."
                                    "Congratulations, you're the first ever
user of FEATURE dependencies! Please contact the asdf-devel mailing-list.")
                            (when (find (second d) *features*
                                        :test 'string-equal)
                              (dep op (third d) nil)))
                  ;; versioned components
                  ;;  * (:version component version [version-p])
                  (:version (apply #'dep op (second d) (nthcdr 2 d)))
                  ;;  * (component [version version-p])
                  (otherwise (apply #'dep op d))))))
        flag))


;;; files

(defun system-definition-pathname (system &optional version version-p)
  "Return some pathname for SYSTEM.  It can be either gotten from sysdef in
*defined-systems*, if VERSION, VERSION-P satisfy, or found with one of
*system-definition-search-functions*."
  (when system
    (let ((name (coerce-name system)))
      (or (multiple-value-bind (in-memory different-version?)
              (system-registered-p name version version-p)
            (when (and in-memory (not different-version?))
              (handler-case (system-source-file (sysdef-elt in-memory))
                (unbound-slot () (unregister-system name)
                                 nil))))
          (some (lambda (x) (funcall x name version version-p))
                *system-definition-search-functions*)))))

#+mutest
(deftest system-definition-pathname
    (:test (lambda (rez expected)
             (if rez
                 (let ((rez-str (princ-to-string rez)))
                   (string= rez-str expected
                            :start1 (- (length rez-str)
                                       (length expected))))
                 (null expected)))
    :setup `(w/registered-systems
                ('("bar" :version "1.2.3"
                   :source-file "bar.asd"))
              (let ((*central-registry*
                     ',(mapcar (lambda (suffix)
                                 (format nil "~a~a"
                                         (pathname-directory-pathname
                                          (load-pathname))
                                         suffix))
                               '("test/bar-3.0.0/"
                                 "test/foo/"
                                 "test/foo-1.2.3/"
                                 "test/foo-1.2.2/"))))
                :body)))
  '("foo")                 "foo.asd"
  '("foo" (1 2 3))         "foo-1.2.3/foo.asd"
  '("foo" (1 2 3) :above)  "foo-1.2.3/foo.asd"
  '("bar")                 "bar.asd"
  '("bar" (2 9 9) :above)  "bar-3.0.0/bar.asd"
  '("baz")                 nil)
  

(defmethod input-files ((operation operation) (c component))
  (let ((parent (component-parent c))
        (self-deps (component-self-dependencies operation c)))
    (if self-deps
        (mapcan (lambda (dep)
                  (destructuring-bind (op name) dep
                    (output-files (make-instance op)
                                  (find-component parent name
                                                  (when (eq (class-of parent)
                                                            'system)
                                                    (component-version
                                                     parent))))))
                self-deps)
        ;; no previous operations needed?  I guess we work with the
        ;; original source file, then
        (list (component-pathname c)))))

;; limitation of SYSTEM-SOURCE-FILE method: deals with versions, only
;; when receiving SYSTEM object as input (not string etc)


;;; operate

(defmethod operate (operation-class system &rest args
                    &key ((:verbose *asdf-verbose*) *asdf-verbose*) force
                    version version-p &allow-other-keys)
  (declare (ignore force))
  (let* ((*package* *package*)
         (*readtable* *readtable*)
         (op (apply #'make-instance operation-class
                    :original-initargs args
                    args))
         (*verbose-out* (if *asdf-verbose* *standard-output*
                            (make-broadcast-stream)))
         (system (if (typep system 'component) system
                     (or (find-system system)
                         (error 'missing-component :requires system)))))
    (unless (version-satisfies (component-version system) version version-p)
      (error 'missing-component-of-version :requires system :version version))
    (let ((steps (traverse op system)))
      (with-compilation-unit ()
        (loop :for (op . component) :in steps :do
          (loop
            (restart-case
                (progn
                  (perform-with-restarts op component)
                  (return))
              (retry ()
                :report (lambda (s)
                          (format s "~@<Retry performing ~S on ~S.~@:>"
                                  op component)))
              (accept ()
                :report (lambda (s)
                          (format s "~@<Continue, treating ~S on ~S as ~
having been successful.~@:>"
                                  op component))
                (setf (gethash (type-of op)
                               (component-operation-times component))
                      (get-universal-time))
                (return)))))))
    op))

(defun oos (operation-class system &rest args
            &key ((:verbose *asdf-verbose*) *asdf-verbose*) force
            version version-p &allow-other-keys)
  "Alias for OPERATE."
  (declare (ignore force))
  (apply #'operate operation-class system args))

;;; end