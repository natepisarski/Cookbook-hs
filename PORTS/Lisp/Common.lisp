(defun convstring (x) (coerce x 'list)) ; String to list

(defun zip (a b)
  (cond
   ((or (null a) (null b)) nil)
   (t (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))) ; Haskell-esque zip

(defun range (x c)
  (if (eql x (+ 1 c)) nil (cons x (range (+ x 1) c)))) ; Numbers from x to c

(defun fmap (l f)
  (if (null l) nil (cons (funcall f (car l)) (fmap (cdr l) f)))) ;Map a function over a list

(defun filter (x f)
  (cond
   ((null x) nil)
   ((funcall f (car x)) (cons (car x) (filter (cdr x) f)))
   (t (filter (cdr x) f)))) ;Entries matching predicate

(defun flt (x)
  (cond
   ((null x) nil)
   (t (cons (caar x) (flt (cdr x))))));Flatten list one leve


(defun flatten (ls);Thanks Graham!
  (labels (
    (mklist (x) (if (listp x) x (list x)))
    )
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)));Flatten list all the way

(defun sum (x)
  (if (null x) 0 (+ (car x) (sum (cdr x)))));Add a list of numbers
  
(defun positions (l c)
(flt 
 (fmap 
  (let ((x (zip l (range 0 (- (length l) 1))))) 
    (filter x (lambda (y) (eql (car y) c)))) #'cdr)));All positions of c in l

(defun pos (l c) (car (positions l c))) ; First position found

(defun sub (l c)
  (cond
   ((eql c 0) l)
   ((null l) nil)
   (t (sub (cdr l) (- c 1)))));Collect to point

(defun while (l f c)
  (cond
   ((null l) nil)
   ((eql c 0) nil)
   (t (cons (funcall f l) (while (cdr l) f (- c 1))))));Map with a counter

(defun generate (x c)
  (cond
   ((eql c 0) nil)
   (t (cons x (generate x (- c 1)))))) ;Make a c length list of x

(defun take (x c)
  (while x #'car c)) ;Haskellish take implementation

(defun after (l c) (cdr (sub l (pos l c)))) ;All elements after first occurence

(defun before (l c) (take l (pos l c))) ;All elements before first occurence

(defun bor (x) (if (null x) nil (if (car x) t (bor (cdr x)))));Boolean Or
