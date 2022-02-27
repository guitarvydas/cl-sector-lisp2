(defsystem :cl-sector-lisp2
  :depends-on (:lookup)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "cl-sector-lisp" :depends-on ("package"))
                                     (:file "atom-memory" :depends-on ("cl-sector-lisp"))
                                     (:file "intern" :depends-on ("atom-memory"))
                                     (:file "read" :depends-on ("intern"))
                                     ))))

