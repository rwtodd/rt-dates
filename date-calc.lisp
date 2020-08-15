;; R T - D A T E S
(in-package "RT-DATES")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +weekdays-per-year+ (* 52 5))
  (defconstant +ydayz+
    (coerce (loop :for x :across #(0 31 28 31 30 31 30 31 31 30 31 30)
		  :summing x :into y :collecting y) 'vector)))

(defmacro divisible-p (y n) `(zerop (mod ,y ,n)))
(defun leap-year-p (y)
  "is Y a leap year?"
  (and (divisible-p y 4)
       (or (not (divisible-p y 100)) (divisible-p y 400))))

(defun ymd-2-yday (y m d)
  "Give the 0-based day-of-the-year (0-364; 365 on leap-years)"
  (+ (svref +ydayz+ (1- m))
     (1- d)
     (if (and (> m 2) (leap-year-p y)) 1 0)))

(defun yday-2-md (y yday)
  (let* ((ly (leap-year-p y))
	 (ld (and ly (= yday 59))))
    (if ld
	(values 2 29)
	(progn
	  (when (and ly (> yday 59))
	    (decf yday))
	  (let ((pos (position-if #'(lambda (x) (<= x yday))
				  +ydayz+
				  :from-end t)))
	    (values (1+ pos)
		    (- yday (svref +ydayz+ pos) -1)))))))

(defun date-2-ymd (d)
  (cond
    ((null d)
     (multiple-value-bind (_1 _2 _3 d m y) (get-decoded-time)
       (declare (ignore _1 _2 _3))
       (values y m d)))
    ((integerp d)
     (multiple-value-bind (_1 _2 _3 d m y) (decode-universal-time d)
       (declare (ignore _1 _2 _3))
       (values y m d)))
    ((and (listp d) (= (length d) 2))
     (multiple-value-bind (month day) (apply #'yday-2-md d)
       (values (first d) month day)))
    ((and (listp d) (= (length d) 3))
     (values-list d))
    (t (error "not a valid date!"))))

(defun date-2-yday (d)
   (cond
    ((null d)
     (multiple-value-bind (_1 _2 _3 d m y) (get-decoded-time)
       (declare (ignore _1 _2 _3))
       (values y (ymd-2-yday y m d))))
    ((integerp d)
     (multiple-value-bind (_1 _2 _3 d m y) (decode-universal-time d)
       (declare (ignore _1 _2 _3))
       (values y (ymd-2-yday y m d))))
    ((and (listp d) (= (length d) 2))
     (values-list d))
    ((and (listp d) (= (length d) 3))
     (values (first d) (apply #'ymd-2-yday d)))
    (t (error "not a valid date!"))))

(defun days-between (start end &key inclusive)
  "give the number of days between the start and end dates.
The days can be given as: (1) integer universal-time,
(2) a list '(year month day) (3) a list (year yday)"
  (multiple-value-bind (ys ds) (date-2-yday start)
    (declare (type fixnum ys ds))
    (multiple-value-bind (ye de) (date-2-yday end)
      (declare (type fixnum ye de))
      (when (< ys 0)
	(let ((addend (* -400 (floor ys 400))))
	  (incf ys addend)
	  (incf ye addend)))
      
      (+ (if inclusive 1 0)
	 (* 365 (- ye ys))
	 (- de ds)
	 (- (ceiling ye 4)   (ceiling ys 4))
	 (- (ceiling ys 100) (ceiling ye 100))
	 (- (ceiling ye 400) (ceiling ys 400))))))

(defun weekdays-between (start end &key inclusive)
  (let ((days (days-between start end :inclusive inclusive))
	(day-of-week
	  (multiple-value-bind (y m d) (date-2-ymd start)
	    (nth-value 6
		       (decode-universal-time
			(encode-universal-time 0 0 0 d m y))))))
    (declare (type integer days)
	     (type (integer 0 6) day-of-week))
    ;; 6 = sunday 5 = saturday
    (- days
       (- (ceiling (+ day-of-week days 2) 7) (ceiling (+ 2 day-of-week) 7))
       (- (ceiling (+ day-of-week days 1) 7) (ceiling (1+ day-of-week) 7)))))
