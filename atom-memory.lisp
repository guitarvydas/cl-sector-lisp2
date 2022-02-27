(defclass atom-memory ()
  ((bytes :initarg :bytes :accessor bytes)
   (current-atom-index :initform @NIL :accessor current-atom-index)))

(defmethod mem-reset ((self atom-memory))
  (setf (current-atom-index self) @NIL))

(defmethod ?eof ((self atom-memory))
  (atoms-end? self (current-atom-index self)))

(defmacro exit-when (pred) `(when ,pred (return)))

(defmethod atoms-end? ((self atom-memory) index)
  (>= (@adjust-index index) (@adjust-index *next-free-atom-pointer*)))

(defun getc (i)
  (let ((n (@get i)))
    (if (subtypep (type-of n) 'character)
        n
      (if (subtypep (type-of n) 'fixnum )
            (code-char n)
	  (progn
	    (format *error-output* "type-of n is ~a~%" (type-of n))
            (assert nil))))))
      
(defun sl-car (index)
  (char-code (getc index)))

(defmethod next-cell ((self atom-memory) cell-index)
  (declare (ignore self))
  (+ +cell-size+ cell-index))

(defmethod maxed-out ((self atom-memory) cell-index)
  (atoms-end? self (next-cell self cell-index)))

(defmethod @advance-to-next-atom ((self atom-memory))
  (let ((cell-index (current-atom-index self)))
    ;; cell-index might be @NIL - assume that there are >0 characters at @NIL
    (if (maxed-out self cell-index)
        (setf (current-atom-index self) (next-cell self *next-free-atom-pointer*))
      (let ()
        (setf cell-index (+ +cell-size+ cell-index))
        ;;;     while not (@null? (@cdr cell-index))
        ;;;         cell-index := (@cdr cell-index)
        ;;;     if maxed-out (cell-index)
        ;;;         return cell-index
        ;;;     else
        ;;;         return (@cdr cell-index)
        (loop while (not (@null? (@cdr cell-index)))
              do (setf cell-index (@cdr cell-index)))
        (setf (current-atom-index self)
              (if (maxed-out self cell-index)
                  cell-index
                (next-cell self cell-index)))))))

(defmethod ?current-atom-index ((self atom-memory))
  (current-atom-index self))

(defmethod ?match-string ((self atom-memory) s)
  ;; return (true) if every character of s matches the current atom
  ;; return false otherwise
  ;; don't advance the atom-index, simply test s against the current atom
  (match-string self s (current-atom-index self)))

(defun @NIL-as-char () (code-char @NIL))


(defmethod match-string ((self atom-memory) s atom-index)
  (if (and
       (@null? atom-index)
       (at-end s))
      t
    (let ((c-atom (getc atom-index)))
      (if (at-end s)
          nil
        (let ((c-s (string-car s)))
          (if (char= c-s c-atom)
              (match-string self (string-cdr s) (@cdr atom-index))
            nil))))))

(defun string-car (s)
  (char s 0))

(defun string-cdr (s)
  (subseq s 1))

(defun at-end (s)
  (= 0 (length s)))
      
(defun atest ()
  (initialize-memory)
  (let ((mem (make-instance 'atom-memory :bytes *memory*)))
    (let ((nileq (?match-string mem "NIL")))
      (let ((quoteeq0 (?match-string mem "QUOTE")))
        (format *standard-output* "NIL match success=~a ~a~%" nileq (current-atom-index mem))
        (format *standard-output* "QUOTE match success=~a ~a~%" quoteeq0 (current-atom-index mem))
        (let ()
          (@advance-to-next-atom mem)
          (format *standard-output* "next atom=~a~%" (current-atom-index mem))
          (let ((quoteeq (?match-string mem "QUOTE")))
            (format *standard-output* "QUOTE match success=~a ~a~%" quoteeq (current-atom-index mem))

            (@advance-to-next-atom mem)
            (@advance-to-next-atom mem)
            (let ((eqeq (?match-string mem "EQ")))
              (format *standard-output* "EQ match success=~a ~a~%" eqeq (current-atom-index mem))

              (values))))))))
