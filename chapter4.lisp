;;; execise 1
(defun quarter-turn (arr)
  (let ((dim (array-dimensions arr)))
    (let ((n (car dim)))
      (let ((result (make-array (list n n))))
        (do ((row 0 (+ 1 row)))
            ((>= row n) result)
          (do ((col 0 (+ 1 col)))
              ((>= col n))
            (setf (aref result row col) (aref arr (- (- n 1) col) row))))))))


;;; execise 2
(defun my-copy-list (list)
  (reduce #'cons list
          :from-end t
          :initial-value nil))

(defun my-reverse (list)
  (reduce (lambda (acc x)
            (cons x acc))
          list
          :initial-value nil))

;;; execise 3
(defstruct three-way-tree-node
  data
  (left nil)
  (middle nil)
  (right nil))

(defun copy-three-way-tree (tree)
  (if (null tree)
      nil
      (let ((l (three-way-tree-node-left tree))
            (m (three-way-tree-node-middle tree))
            (r (three-way-tree-node-right tree))
            (data (three-way-tree-node-data tree)))
        (make-three-way-tree-node
         data
         (copy-three-way-tree l)
         (copy-three-way-tree m)
         (copy-three-way-tree r)))))


(defun find-three-way-tree (obj tree)
  (if tree
      (or
       (eql obj (three-way-tree-node-data tree))
       (find-three-way-tree obj (three-way-tree-node-left tree))
       (find-three-way-tree obj (three-way-tree-node-middle tree))
       (find-three-way-tree obj (three-way-tree-node-right tree)))))

;;; execise 4
(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defun bst-rec (bst acc)
  (if bst
      (let ((smaller (bst-rec (node-l bst) acc))
            (data (node-elt bst)))
        (bst-rec (node-r bst) (cons data smaller)))
      acc))

(defun bst2lst (bst)
  (bst-rec bst nil))

;;; execise 5
;;; same as bst-insert

;;; execise 6
(defun alist->hash (alist &key (test #'eql))
  (let ((hash (make-hash-table :test test)))
    (do ((lst alist (cdr lst)))
        ((null lst))
      (let ((pair (car lst)))
        (setf
         (gethash (car pair) hash) (cdr pair))))
    hash))

(defun hash->alist (hashtable)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (let ((con (cons k v)))
                   (setf
                    acc (cons con acc))
                   acc))
             hashtable)
    acc))
