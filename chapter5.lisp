;;; execise 1
;; (a)
;; ((lambda (x)
;;    (cons x x))
;;  (car y))

;; (b)
;; ((lambda (w)
;;    ((lambda (y)
;;       (cons w y))
;;     (+ w z)))
;;  (car x))

;;; exercise 2
(defun mystery (x y)
  (cond
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery x (cdr y))))
         (and z (+ z 1))))))

;;; execise 3
(defun sq-if-greater-than-5 (x)
  (cond
    (((and (integerp x)
           (< 0 x 6))
      (* x x)))
    (t x)))

;;; execise 4
(defun month-num (m y)
  (+ (case m
     (1 0)
     (2 31)
     (3 59)
     (4 90)
     (5 120)
     (6 151)
     (7 181)
     (8 212)
     (9 243)
     (10 273)
     (11 304)
     (12 334)
     (13 365))
     (if (and (> m 2) (leap? y))
         1
         0)))

;;; execise 5

(defun hash-table-keys (ht)
  (let ((result nil))
    (maphash #'(lambda (k v)
                 (push k result))
             ht)
    result))

(defun precedes-iter (x v)
  (let ((charMap (make-hash-table)))
    (dotimes (i (length v))
      (if (and (> i 0)
               (char-equal (aref v i) x))
          (setf (gethash (aref v (- i 1)) charMap) 1)))
    (hash-table-keys charMap)))

(defun precedes-rec (x v)
  (let ((acc (make-hash-table)))
    (precedes-rec-helper x v 0 acc)
    (hash-table-keys acc)))

(defun precedes-rec-helper (x v i acc)
  (cond
    ((= 0 i) (precedes-rec-helper x v (+ 1 i) acc))
    ((= i (length v)) acc)
    ((char-equal x (aref v i))
     (progn
       (setf (gethash (aref v (- i 1)) acc) 1)
       (precedes-rec-helper x v (+ 1 i) acc)))
    (t (precedes-rec-helper x v (+ 1 i) acc))))


;;; execise 6
(defun intersperse-iter (obj lst)
  (do ((l lst (cdr l))
       (result nil (cons obj (cons (car l) result))))
      ((null l) (reverse (cdr result)))))

(defun intersperse-rec (obj lst)
  (let ((result (intersperse-rec-helper obj lst nil)))
    (reverse (cdr result))))

(defun intersperse-rec-helper (obj lst acc)
  (cond
    ((null lst) acc)
    (t (intersperse-rec-helper obj (cdr lst) (cons obj (cons (car lst) acc))))))

;;; execise 7
(defun list-abs-1?-rec (lst)
  (list-abs-1?-rec-helper lst t))

(defun list-abs-1?-rec-helper (lst acc)
  (cond
    ((not acc) acc)
    ((null lst) acc)
    (t (let ((tail (cdr lst)))
         (if (null tail)
             acc
             (let ((x (car lst))
                   (y (car tail)))
               (if (= 1 (abs (- x y)))
                   (list-abs-1?-rec-helper tail acc)
                   nil)))))))

(defun list-abs-1?-do (lst)
  (do ((tail (cdr lst) (cdr tail))
       (head (car lst) (car tail)))
      ((null tail) t)
    (if (/= 1 (abs (- head (car tail))))
        (return nil))))

(defun list-abs-1?-mapc (lst)
  )
