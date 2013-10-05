(defun look (tlist x)
  (cond
   ((null tlist) nil)
   ((eql (caar tlist) x) (car tlist))
   (t (look (cdr tlist) x))))
