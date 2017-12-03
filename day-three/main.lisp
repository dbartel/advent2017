
(defparameter input 265149)


;;;; Sample Grid (infinite spiral)
;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6   1   2  11
;; 20   7   8   9  10
;; 21  22  23  24  25
;;;;


(defun sq (x) (* x x))

;; Part One
(defun find-steps (i)
  (let* (
	 ;; Bottom left corner is increasing odd squares
	 ;;; So, sqrt of our input will give us the "layer number" (e.g. ceil(sqrt 3) = 2)
	 (root (ceiling (sqrt i)))
	 
	 ;; Length of  a side is increasing odd numbers
	 (side-length (if (evenp root) (+ root 1) root))

	 ;; Steps from current layer's center axis to center of grid
	 ;;; Just the ordinal value of the layer (e.g. *3* is on layer *1*)
	 (ordinal (/ (- side-length 1) 2))
	 (cycle (- i (sq (- side-length 2))))
	 (offset (mod cycle (- side-length 1))))
    (+ ordinal (abs (- offset ordinal)))))


;; Part 2 https://oeis.org/A141481

