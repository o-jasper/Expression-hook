


(expr-hook:expand
(macroexpand-1 '(denest
	  (block args-block)
	  (let (state))
	  (collecting (:onto arg :collect col-arg))
	  (collecting (:onto key :collect col-key))
	  (collecting (:onto opt :collect col-opt))
	  (collecting (:onto rest :collect col-rest))
	  (after (return-from args-block
		   `(,@arg ,@(when opt `(&key ,@opt))
			   ,@(when key `(&optional ,@key))
			   ,@(when rest `(&rest ,@rest)))))))

( "/home/jasper/proj/expression-hook/expression-hook.lisp")

(expr-scan:scan :expression-hook)
(expr-scan:access-result 'defun)

(in-package :expr-hook)
(print(expr-hook:expand
 '(DEF-BASE-MACRO DEFMETHOD (DEFMETHOD NAME &REST REST)
 (ASSERT (LISTP REST))
 (MULTIPLE-VALUE-BIND (REST WAY)
     (IF (SYMBOLP (CAR REST))
         (VALUES (CDR REST) (LIST (CAR REST)))
         REST)
   (ASSERT (LISTP REST))
   (DESTRUCTURING-BIND
       (ARGS &BODY BODY)
       REST
     `(,DEFMETHOD ,NAME ,@WAY ,ARGS
       ,@(BASE-FUN NAME ARGS BODY :FLAT-ARG T)))))))
(macroexpand-1 '(multiple-value-bind (a b) (d)))