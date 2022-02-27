(defun @read(str mem)
  (multiple-value-bind (result remainder)
      (%read str)
    (declare (ignore remainder))
    (convert-lisp-to-sl result mem)))

;; read using CL primitives
(defun %read(str)
  (catch 'read-failure
    (let ((lstr (@listify-string str)))
      (multiple-value-bind (result remaining)
          (@lread lstr)
        (let ((tail (@trim-leading-spaces remaining)))
          (cond
           ((null tail) (values result nil))
           ((@is-follow-separator (car tail))
            (format *error-output* "too many right parentheses in ~s~%" str)
            (throw 'read-failure (values nil nil)))
           (t (values result tail))))))))

(defun @listify-string(s)
  (concatenate 'list s))

(defun @lread(raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (cond

     ((null lstr)
      (values nil lstr))

     ((@is-begin-separator (car lstr)) ;; (
      (multiple-value-bind (result-list leftover)
          (@lmap-read nil (cdr lstr))
        (@need-follow-separator leftover raw-lstr) ;; )
        (values result-list (cdr leftover))))

     ((@is-follow-separator (car lstr)) ;; )
      (values @NIL lstr))

     (t (@lread-atom lstr)))))

(defun db (s)
  (format *error-output* "[~a]" s))

(defun @lmap-read (accumulator raw-lstr)
  ;; objective: create list of items for inside "(" ... ")"
  ;; read from raw-lstr (skipping spaces) and append to accumulator
  ;; return (1) accumulated list (2) left-over string
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (cond
     ((null lstr) (values accumulator nil))
     ((@is-follow-separator (car lstr)) (values accumulator lstr))
     (T 
        (multiple-value-bind (front leftover)
            (@lread lstr)
          (@lmap-read (%append1 accumulator front) leftover))))))

(defun @lread-atom (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (let ((front (@upto-separator lstr)))
      (let ((tail (@after-separator-inclusive lstr)))
        (if (null front)
            (values nil tail)
          (let ((astring (collapse-character-list-to-string front)))
            (let ((atom-index (intern astring)))
              (values atom-index tail))))))))

(defun @trim-leading-spaces (lstr)
  (if lstr
      (if (char= #\Space (car lstr))
	  (@trim-leading-spaces (cdr lstr))
	  lstr)
      nil))

(defun @upto-separator (lstr)
  (if (null lstr)
      nil
      (if (@is-separator (car lstr))
          nil
        (cons (car lstr) (@upto-separator (cdr lstr))))))

(defun @after-separator-inclusive (lstr)
  (if (null lstr)
      nil
    (if (@is-separator (car lstr))
        lstr
      (@after-separator-inclusive (cdr lstr)))))

(defun @is-separator (c)
  (or (char= c #\Space)
      (char= c #\()
      (char= c #\))))

(defun @is-begin-separator (c)
  (char= c #\( ))

(defun @is-follow-separator (c)
  (char= c #\) ))

(defun @need-follow-separator (leftover original-string)
  (if (and (listp leftover) (not (null leftover)) (char= #\) (car leftover)))
      t
    (@read-error (format nil "*** ERROR: while reading list ~s, expected ')' but got ~s" original-string leftover))))


(defun @empty(s) (= 0 (length s)))
(defun @read-error (s)
  (format *error-output* "~a~%" s)
  (throw 'read-failure (values nil nil)))

(defun collapse-character-list-to-string (l)
  (format nil "~{~a~}" l))

;; stubbed out calls to sector-lisp (to allow debugging of read using full-blown CL)
;;; (defun @cons (a b) (cons a b))
;;; (defun @NIL () nil)
;;; (defun @intern (s)
;;;   (intern s))


(defun @reverse (l)
  (reverse l))

(defun %append1 (L item)
  ;; append 1 item to end of L
  (if (null L)
      (list item)
    (cons (car L) (%append1 (cdr L) item))))



(defun convert-lisp-to-sl (L mem)
  (cond
   ((null L) @NIL)
   ((atom L) (@intern (symbol-name L) mem))
   (t (@cons (convert-lisp-to-sl (car L) mem)
             (let ((r (mapcar #'(lambda (x)
                                  (convert-lisp-to-sl x mem))
                              (cdr L))))
               (convert-lisp-to-sl-top-level-only r mem))))))

(defun convert-lisp-to-sl-top-level-only (L mem)
  (cond
   ((null L) @NIL)
   ((atom L) L)
   (t (@cons (car L) (convert-lisp-to-sl-top-level-only (cdr L) mem)))))
  


(defun rtry-a (s)
  (multiple-value-bind (result leftover)
      (@lread-atom (@listify-string s))
    (format *error-output* "~s -> ~a ~a~%" s result leftover)))

(defparameter *R* nil)
(defparameter *L* nil)

(defun rtry-r (s)
  (multiple-value-bind (result leftover)
      (%read s)
    (setf *R* result)   ;; debug
    (setf *L* leftover) ;; debug
    (format *error-output* "~s -> ~s ~s~%" s result leftover)))

(defun rtest ()
  (rtry-a "X")
  (rtry-r "X")
  (rtry-r "Y")
  (rtry-r "(Z)")
  (rtry-r "(A B)")
  (rtry-r "((E))")
  (rtry-r "(F(G))")
  (rtry-r "(H(I J)K)")
  (rtry-r "( L ( M N ) O )")

  (rtry-r "((LAMBDA (X) (QUOTE X)) (CONS (QUOTE A) (QUOTE B)))")

  (initialize-memory)
  (let ((mem (make-instance 'atom-memory :bytes *memory*)))
    (values (@read  "(A)" mem)
            *memory*)
    (values (@read  "(B C)" mem)
            *memory*)
    (multiple-value-bind (r tail)
        (values (@read  "( L ( M N ) O )" mem)
                *memory*)
      (@print r))
    (multiple-value-bind (r tail)
        (values (@read "((LAMBDA (X) (QUOTE X)) (CONS (QUOTE A) (QUOTE B)))"  mem)
                *memory*)
      (@print r))
    ))
