;Implementations of filter that stop execution at a certain point.

(defun filterBreak (l f)
  (cond
   ((null l) nil)
   ((not (funcall f (car l))) nil)
   (t (cons (car l) (filterBreak (cdr l) f))))) ;Break on False

(defun removeBreak (l f)
  (cond
    ((null l) nil)
    ((not (funcall f (car l))) l)
    (t (removeBreak (cdr l) f))));Snap on false

(defun btr (l f r)
  (cond
   ((null l) nil)
   ((funcall f (car l)) (cons (car r) (btr (cdr l) f r)))
   (t (cons (cadr r) (btr (cdr l) f r)))));Transofmr (car r) on true (cadr r) on false

(defun imbreakp (l f)
  (cond
   ((null l) nil)
   ((funcall f (car l)) t)
   (t (imbreak (cdr l) f))));True if predicate is ever true
  
