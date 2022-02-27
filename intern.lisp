(defun @intern (str mem)
  (multiple-value-bind (index found)
      (lookup str mem)
    (if found
        index
      (let ((new-index (@putatom (split-into-chars str))))
        new-index))))

(defun itry (s mem)
  (format *standard-output* "~s is interned as ~a~%" s (@intern s mem))
  (values))

(defun itest ()
  (initialize-memory)
  (let ((mem (make-instance 'atom-memory :bytes *memory*)))
    (itry "QUOTE" mem)
    (itry "X" mem)
    (itry "QUOTE" mem)
    (itry "X" mem)
    (itry "QUOTE" mem)
    ))

(defun split-into-chars (s)
  (concatenate 'list s))
      