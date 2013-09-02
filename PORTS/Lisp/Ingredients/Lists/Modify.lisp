;Functions for manipulating lists
(load "../../Common.lisp")
(load "Access.lisp")

(defun rev (x) (reverse x));Unlike Haskell, reverse already exists. Function included for familiarity

(defun rm (x c) (filter x (lambda (y) (not (eql y c)))))

(defun splitOn (x c)
  (rm
   (cond
    ((null x) nil)
    ((not (elemp x c)) (cons x nil))
    (t (cons (before x c) (splitOn (after x c) c)))) nil))

(defun snipe (x c);snipe '(1 2 3) (2,'a')
  (flatten (list (take x (-  (car c) 1)) (cadr c) (sub x (+ (car c) 1)))))

  
