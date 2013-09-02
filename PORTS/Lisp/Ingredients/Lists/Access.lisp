(load "../../Common.lisp")
(defun elemp (x c) (bor (fmap x (lambda (y) (eql y c)))));C found in list?

(defun fcount (x c) (length (filter x (lambda (y) (eql y c)))));Count for lists.

(defun containsp (x c)
  (cond
   ((null x) nil)
   ((equal (take x (length c)) c) t)
   (t (containsp (cdr x) c))))

(defun qsort (list);Thanks deliciousrobots, for a Haskell-like implementation of qs.
  (when list
    (destructuring-bind (p . xs) list
      (let ((lesser (filter xs (lambda (x) (< x p))))
            (greater (filter xs (lambda (x) (>= x p)))))
        (append (qsort lesser) (list p) (qsort greater))))))




