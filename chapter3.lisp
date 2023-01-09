;; execise 2
(defun new-union (l1 l2)
  (let ((r (reverse l1)))
    (dolist (x l2)
      (if (not (member x r))
          (push x r)))
    (reverse r)))

;; execise 3
(defun occurrences (l)
  (let ((acc nil))
    (dolist (x l)
      (let ((pair (assoc x acc)))
        (if pair
            (incf (cdr pair))
            (push (cons x 1) acc))))
    (sort acc #'> :key #'cdr)))

;; execise 4
;; because the default comparator used by member function is eql

;; execise 5
(defun pos+rec (list)
  (pos+rec-helper list 0 nil))

(defun pos+rec-helper (list i acc)
  (if (null list)
      (reverse acc) 
      (let* ((v (car list))
            (sum (+ v i)))
        (pos+rec-helper (cdr list) (+ 1 i) (push sum acc)))))

(defun pos+iter (list)
  (let ((result nil))
    (do ((i 0 (+ 1 i))
         (lst list (cdr lst)))
        ((null lst) (reverse result))
      (push (+ i (car lst)) result))))

(defun pos+mapcar (list)
  (let ((idx -1))
    (mapcar #'(lambda (x)
                (+ x (incf idx)))
            list)))

;; execise 6
(defun gov-cons (x y)
  (let ((pair '(nil . nil)))
    (setf
     (car pair) y
     (cdr pair) x)
    pair))

(defun gov-list (&rest items)
  (gov-list-0 items))

(defun gov-list-0 (items)
  (if (null items)
      nil
      (cons (gov-list-0 (cdr items)) (car items))))

(defun gov-length (list)
  (if list
      (+ 1 (gov-length (car list)))
      0))

(defun gov-member (item list)
  (if list
      (let ((head (cdr list))
            (tail (car list)))
        (if (eql head item)
            list
            (gov-member item tail)))))

;; exercise 7
(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)   ;instead of (list n elt)
      elt))

;; execise 8
(defun showdots (list)
  (showdots-rec list))

(defun showdots-rec (list)
  (if list
      (if (atom list)
          list
          (format nil "(~A . ~A)"
                  (car list)
                  (showdots-rec (cdr list))))))
