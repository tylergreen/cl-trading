; Frank Duncan's Common Lisp Options Trading Utilities
; Modified by Tyler Green  

;**************
; General Utilities

(defmacro mapit (fn-body lst)
  `(mapcar (lambda (it) ,fn-body) ,lst))

(defun string-to-symbol (ctrl-str &rest args) 
 (read-from-string
  (apply #'format nil ctrl-str args)))

;******************
; Money Data Structure

(defstruct (money (:print-function print-money)) pennies)

(defun print-money (money str depth)
  (declare (ignore depth))
  (format str "$~$" (/ (money-pennies money) 100)))

(set-macro-character #\$ (lambda (str char)
			   (declare (ignore char))
			   (let ((amount (read str)))
			     `(make-money :pennies (floor (* ,amount 100))))))


(defun m+ (&rest moneys) (make-money :pennies (apply #'+ (mapcar #'money-pennies moneys))))
(defun m- (&rest moneys) (make-money :pennies (apply #'- (mapcar #'money-pennies moneys))))
(defun m* (money &rest magnitudes) (make-money :pennies
                                               (apply #'* (money-pennies money) magnitudes)))

(defun m/ (m1 m2) (/ (money-pennies m1) (money-pennies m2))) ; Oddball, defines ratio, not money
(defun m< (&rest ms) (apply #'< (mapcar #'money-pennies ms)))
(defun m> (&rest ms) (apply #'> (mapcar #'money-pennies ms)))
(defun m= (&rest ms) (apply #'= (mapcar #'money-pennies ms)))

;*********************
; Strategies

; Tools

(defun buy-stock (initial-price num-shares)
  (lambda (final-price)
    (m- (m* final-price num-shares)
	(m* initial-price num-shares))))

(defun short-sale (initial-price)
 (lambda (final-price)
  (m* (m- initial-price final-price) num-shares)))

(defmacro defoption (action opt-type dir)
  `(defun ,(string-to-symbol "~A-~A" action opt-type)
       (strike-price premium num-shares)
     (lambda (final-price)
       (m+ (m* premium num-shares ,@(when (eql action 'hold) '(-1)))
       (if (,(if (eql opt-type 'put) 'm< 'm>) final-price strike-price)
	   (m* (m- ,@(if (eql dir 'bullish)
			 '(final-price strike-price)
			 '(strike-price final-price))) num-shares)
	   $0)))))

(defoption write call bearish)
(defoption hold call bullish)
(defoption write put bullish)
(defoption hold put bearish)

; Strategy Combinations

(defun combine (&rest strats)
  (lambda (final-price)
    (reduce #'m+ (mapcar (lambda (strat) (funcall strat final-price))
			 strats))))

(defun covered-call (initial-price num-shares strike-price premium)
  (combine
   (buy-stock initial-price num-shares)
   (write-call strike-price premium num-shares)))

(defun straddle (strike-price put-premium call-premium num-shares)
  (combine (hold-call strike-price call-premium num-shares)
	   (hold-put strike-price put-premium num-shares))) 

(defun reinvested-covered-call (initial-price num-shares strike-price premium)
  (combine
   (buy-stock initial-price num-shares)
   (write-call strike-price premium num-shares)))

(defun reinvested-covered-call (initial-price num-shares strike-price premium)
  (combine 
   (buy-stock initial-price (m/ (m* premium num-shares) initial-price))
   (buy-stock initial-price num-shares)
   (write-call strike-price premium num-shares)))

(defun short-sale-via-options (strike-price call-premium put-premium num-shares)
 (combine
  (write-call strike-price call-premium num-shares)
  (hold-put strike-price put-premium num-shares)))

(defun buy-stock-via-options (strike-price call-premium put-premium num-shares)
 (combine
  (hold-call strike-price call-premium num-shares)
  (write-put strike-price put-premium num-shares)))

;*******************
; Evaluating Strategies

(defun evaluate-strat (strat initial-price)
  (mapcar 
   (lambda (price) (list price (funcall strat price)))
   (sort
    (remove-duplicates
     (append (list $0 initial-price (m* initial-price 2))
	     (get-break-even-points strat initial-price))
     :test #'m=)
    #'m<)))

(defun get-break-even-points (strat initial-price)
  (loop for price = $0.1 then (m+ $.01  price) until (m> price (m* initial-price 2))
     when (let ((p (funcall strat (m- price $.01)))
		(p1 (funcall strat price)))
	    (or (m< p $0 p1)
		(m> p $0 p1)
		(m= $0 p1)))
       collect price))

;****************
; Comparing Strategies

(defun compare-strats (strat1 strat2 initial-price)
  (mapcar
   (lambda (price) (list price (funcall strat1 price) (funcall strat2 price)))
   (sort 
    (remove-duplicates
     (append (list $0 initial-price (m* initial-price 2))
	     (get-overlap-points strat1 strat2 initial-price))
     :test #'m=)
     #'m<)))

(defun get-overlap-points (strat1 strat2 initial-price)
 (loop for price = $.01 then (m+ $.01 price) until (m> price (m* initial-price 2))
       when (or (m= (funcall strat1 price) (funcall strat2 price))
                (and (m< (funcall strat1 (m- price $.01)) (funcall strat2 (m- price $.01)))
                     (m> (funcall strat1 price) (funcall strat2 price)))
                (and (m> (funcall strat1 (m- price $.01)) (funcall strat2 (m- price $.01)))
                     (m< (funcall strat1 price) (funcall strat2 price))))
       collect price))

;***************
; Visualization

(defvar *gnuplot* "/usr/bin/gnuplot")

(defun build-strat-plot (low high strats &optional labels)
 (let*
  ((proc (sb-ext:run-program *gnuplot*  nil :wait nil :input :stream :output t))
   (str (sb-ext:process-input proc)))
  (format str "set terminal png size 640,480~%")
  (format str "set output 'output.png'~%")
  (format str "set xzeroaxis lt -1~%")
  (format str "~{~{set label '$~$: $~$' at first ~2:*~$, first ~$~%~}~}"
              (mapit (mapcar #'m-to-float it) labels))
  (format str "plot ~{'-' with lines ti '~A'~^,~}~%" (mapcar #'car strats))
  (format str "~{~A~}" (mapit (strat-to-plots it low high) (mapcar #'cadr strats)))
  (close str)))

(defun strat-on-range (strat low high)
 (loop for price = low then (m+ $0.01 price) until (m> price high)
         collect (list price (funcall strat price))))

(defun m-to-float (money) (/ (money-pennies money) 100))

(defun strat-to-plots (strat low high)
 (format nil "~{~{~$ ~$~%~}~}e" (mapit (mapcar #'m-to-float it)
                                (strat-on-range strat low high))))


