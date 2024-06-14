;;;; shn.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl-user))

(in-package :shn)

(defconstant %e 2.718281828459045d0)
(defconstant %c 299792458d0) ; m / s
(defconstant %G 6.67430e-11) ; m^3 / Kg s^2
(defconstant %N0 0.933198821833744d-26) ; m/Kg
(defconstant %mp 1.67262171d-27) ; Kg
(defconstant %mn 1.67492729d-27) ; Kg
(defconstant %me 9.1093837015d-31) ; Kg
(defconstant %ee 1.602176634d-19) ; C
(defconstant %Mo 1.9885d30) ; Kg
(defconstant %GMo 1.327124d20) ; m^3/s^2
(defconstant %ly (* %c 365.25d0 60 60 24)) ; m
(defconstant %au 149597870700d0) ;m
(defconstant %pc (* (/ 1 (TAN (/ PI (* 60 60 180)))) %AU)) ; m
(defconstant π pi)

(defparameter *shn-symbol*
    (list
     '+ '(:s-type op :t-op + :arg-n 2)
     '- '(:s-type op :t-op - :arg-n 2)
     '* '(:s-type op :t-op * :arg-n 2)
     '× '(:s-type op :t-op * :arg-n 2)
     '/ '(:s-type op :t-op / :arg-n 2)
     '÷ '(:s-type op :t-op / :arg-n 2)
     '< '(:s-type op :t-op < :arg-n 2)
     '<= '(:s-type op :t-op <= :arg-n 2)
     '> '(:s-type op :t-op > :arg-n 2)
     '>= '(:s-type op :t-op >= :arg-n 2)
     '= '(:s-type op :t-op = :arg-n 2)
     '/= '(:s-type op :t-op /= :arg-n 2)
     '0- '(:s-type op :t-op 0- :arg-n 1)
     '1/ '(:s-type op :t-op 1/ :arg-n 1)
     '√ '(:s-type op :t-op sqrt :arg-n 1)
     'inDeg '(:s-type op :t-op deg :arg-n 1)
     'toDeg '(:s-type op :t-op radTodeg :arg-n 1)	 
     'e^ '(:s-type op :t-op exp :arg-n 1)
     '^ '(:s-type op :t-op expt :arg-n 2)
     'ln '(:s-type op :t-op log :arg-n 1)
     'log '(:s-type op :t-op log :arg-n 2)))

(defun deg (x) (* pi (/ x 180)))
(defun radTodeg (x) (* 180 (/ x pi)))
(defun 1/ (x) (/ 1 x))
(defun 0- (x) (- 0 x))
(defun arg-num? (s)
  (let ((n (getf (getf *shn-symbol* s) :arg-n)))
    (if n n (loop for i in (arg:arglist s)
		  until (or (equal i '&rest) (equal i '&key) (equal i '&optional) (equal i '&body)) count i))))
(defun to-sym (s) (let ((op (getf (getf *shn-symbol* s) :t-op))) (if op op s)))
(defun is-op? (s) (if (equal 'op (getf (getf *shn-symbol* s) :s-type)) 't
		      (if (symbolp s) (if (fboundp s) 't nil) nil)))

(defun to-S-exp (in &optional t-stk)
  (labels ((list? (x) (and (listp x) (not (equal 'quote (car x))) (not (equal 'function (car x))))))
    (if (null in) (return-from to-S-exp (reverse t-stk)))
    (if (is-op? (car in))
	(let ((o-stk (list (pop in))))
	  (loop while o-stk do
	    (loop while (is-op? (car in)) do (push (pop in) o-stk))
	    (let ((arg (if (list? (car in)) (to-S-exp (pop in)) (list (pop in)))))
	      (loop while (< (length arg) (arg-num? (car o-stk))) do (push (pop t-stk) arg))
	      (push (push (to-sym (pop o-stk)) arg) t-stk))
	    (if o-stk (push nil in))))
	(if (list? (car in)) (push (to-S-exp (pop in)) t-stk) (push (pop in) t-stk)))
    (to-S-exp in t-stk)))

(defun rmv-p (in)
  (labels ((list? (x) (and (listp x) (not (equal 'quote (car x))) (not (equal 'function (car x)))))
	   (slist? (x) (and (list? x)(= 1 (length x))(not (is-op? (car x))))))
    (loop while (slist? in) do (setq in (car in)))
    (if (list? in) (map 'list 'rmv-p in) in)))

(defun add-l (in)
  (labels ((rlist? (x) (and (listp x) (< 1 (length x)) (not (is-op? (car x))))))
    (if (not (rlist? in)) (return-from add-l in))
    (push 'list in)
    (map 'list 'add-l in)))

(defmacro shn (&rest in) (add-l (rmv-p (to-S-exp in))))

(defun l-brace (stream char)
  (declare (ignore char))
  (cons 'shn (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'l-brace)
(set-macro-character #\] (get-macro-character #\)))
