;±±±±± Hanoi Towers - Daniel Doubrovkine - uniGE 1995 ±±±±±±±±±±±±±±±±±±±±±±±±
;±±±±±		      Stancu Radu (iteratif method)   ±±±±±±±±±±±±±±±±±±±±±±±±
;note: unauthorized functions for this TP, like (SET! ) have been used for
;      drawing purposes, thus the TP itself is absolutely compliant with
;      the requirements.
(writeln "Compiling Tours de Hanoi (TP2.14.11.95)")
(writeln "Daniel Doubrovkine (c) - uniGE - email welcome: dblock@infomaniak.ch")
(writeln " (hanoi) to run!")
(writeln " code: (hrec num) - slower recursif method")
(writeln "       (hitr num) - faster iteratif method (code by Stancu Radu (c) uniGE)")
(writeln "       hanoi towers graphics implemenation by D.D.")
(writeln "NOTE: IF ERROR WHILE COMPILING INLINE CODE, SIMPLY RECOMPILE (Scheme bug).")

(define tige1_list (list '-)) ;definition of three non-empty lists for 
(define tige2_list (list '-)) ;                 step state maintaining
(define tige3_list (list '-))
(define column 1)	      ;<global> default column 
(define maxnum 0)	      ;<global> maximum items
(define local_counter 0)      ;<global> local counter ...see drawing
(define method_window 0)

;reading parameters from the keyboard, choosing a method
(define (hanoi)
  (define (hanoi_ask method)
    (cond
      ((= method 1) 1)
      ((= method 2) 2)
      (else 
	(display "Illegal method: ")
	(hanoi_ask (integer_read (read)))
      )
    ))
  (writeln "Daniel Doubrovkine & Radu Stancu (c) uniGE, email welcome: dblock@infomaniak.ch")
  (display "Please choose the method: (1 - iteratif, 2 - recursif): ")
  (if (= (hanoi_ask (integer_read (read))) 1) (hitr)
      (hrec))
)

;read an number (integer or float > 0)
(define (integer_read input)
  (cond 
    ((and (> input 0) (number? input))
     input)
    (else
      (display "Positive integer required: ")
      (integer_read (read))
      ))
)

(define (hitr)	            ;hitr: iteratif method of hanoi
  (display "[Iteratif] Please enter the number of disks: ")
  (main (integer_read (read)) 2)                  ;      n is the number of items to start with
)

(define (hrec)              ;hrec: recursif method of hanoi
  (display "[Recursif] Please enter the number of disks: ")
  (main (integer_read (read)) 1)
)

(define (main n method)       ;'play director'
;construct a linear sorted list of n items from 1 to n
(define (construct_list n) (cons n (if (> n 1) (construct_list (- n 1)))))
  (init-graph)
  (set! maxnum n)	      ;<global> set maxnum to the number of items
  (set-world! (cons (- 50) (+ 50 480 (/ 480 maxnum))) ;define orthogonal 
	      (cons 690 (- (/ 480 maxnum) 50)))	      ;coordinates
  (set! tige1_list (construct_list n))		      ;construct the initial
  (set! tige2_list (list '-))			      ;list and <global> set
  (set! tige3_list (list '-))			      ;the other two to not
  (if   					      ;empty
    (= method 1) (hanoi-recur 1 2 3 n)	            ;call the apropriate
    (hanoi-iter n)		       ;method
  )
  (column_list)					      ;draw the last step
  (presskey)
  (close-graph)
)

(define (hanoi-recur from via where n)	    ;recursif method of Hanoi Towers
  (cond ((= n 1) 
         (hanoi_write from via where))
  (else (hanoi-recur from where via (- n 1))
	(hanoi_write from via where)
	(hanoi-recur via from where (- n 1)))
))

(define (hanoi_write from via where)	   ;every move calls this function
  (define (sort_lists)			   ;sort lists
    (set! tige1_list (sort! tige1_list))
    (set! tige2_list (sort! tige2_list))
    (set! tige3_list (sort! tige3_list)))
  (sort_lists)
  (column_list)				   ;draw the state
  (move_list from where)		   ;move the next item
  (sort_lists)
)

(define (move_list from where)
(define (concat x y)			   ;list updater
  (if (null? x)
      y
      (cons (car x ) (concat (cdr x) y))))
  ;take the three lists and update the target, then the source list
  (cond
    ((= from 1) 
     (cond
       ((= where 2) 
	(set! tige2_list (concat tige2_list (list (car tige1_list)))))
       ((= where 3) 
	(set! tige3_list (concat tige3_list (list (car tige1_list)))))
     )
    (set! tige1_list (cdr tige1_list)))

    ((= from 2)
      (cond
	((= where 1) 
	 (set! tige1_list (concat tige1_list (list (car tige2_list)))))
	((= where 3) 
	 (set! tige3_list (concat tige3_list (list (car tige2_list)))))
      )
    (set! tige2_list (cdr tige2_list)))

    ((= from 3)
       (cond
	 ((= where 1) 
	  (set! tige1_list (concat tige1_list (list (car tige3_list)))))
	 ((= where 2) 
	  (set! tige2_list (concat tige2_list (list (car tige3_list)))))
       )
    (set! tige3_list (cdr tige3_list)))
))

(define (column_list)		 ;the real drawing function
(define (column_number num)	 ;will apply to every item
(define (pion)			 ;draws an item
  (set-fill-style 'solid num)    ;sets the color by the way...
  (fill-ellipse (cons (* column (/ 640 6)) (* local_counter (/ 480 maxnum)))
		(cons (* num (/ 640 3 maxnum) 0.5) 3))
  (bar (cons (- (* column (/ 640 6)) (* num (/ 640 3 maxnum) 0.5)) 
	     (* local_counter (/ 480 maxnum)))
       (cons (+ (* column (/ 640 6)) (* num (/ 640 3 maxnum) 0.5))
    	     (+ (* local_counter (/ 480 maxnum)) (/ 480 maxnum))))
  (fill-ellipse (cons (* column (/ 640 6)) 
		      (+ (* local_counter (/ 480 maxnum)) (/ 480 maxnum)))
		(cons (* num (/ 640 3 maxnum) 0.5) 3))
)

  (set! local_counter (+ local_counter 1)) ;update the local counter
  (if (integer? num) (pion))		   ;if non-empty item draw it
)
  (asm_delay)				   ;wait a bit less that half a sec
  (clear-device)			   ;here all calculation is done for
  (set! column 1)			   ;the drawing of the actual move
  (set! local_counter 0)		   ;use the three lists for plotting
  (for-each column_number (reverse tige1_list))
  (set! column 3)
  (set! local_counter (- 1))
  (for-each column_number (reverse tige2_list))
  (set! column 5)
  (set! local_counter (- 1))
  (for-each column_number (reverse tige3_list))
)

;Hanoi (iteratif) (c) Radu Stancu uniGE 1995.
;this function is probably the best ever written for the iteratif method
;of the hanoi towers. It is based on pure constatation of the disk's moves:
;there's a number of 2^n - 1 steps to do at all. This can be proved by 
;recurrence method.
;each disk moves the same way all the time:
; disk 3 in a 3 disks' case will move by 2 all the time
; disk 2                    will move by 1 all the time (one step rotation...)
; disk 1                    will move by 2 all the time	(two steps rotation)
; with an example of 3 disks containing the disk no. you move and knowing
; the step "length", you easilly see that all the information for the move
; is included in the move number itself! This is just the general idea, for
; more detail on the code, please contact Radu Stancu.
; Note: moves are to the center.
(define (hanoi-iter n)   
  (move (- (2-puissance n) 1) n)
)

(define (2-puissance k)	 
  (if (= k 0) 1 (* 2 (2-puissance (- k 1))))
)

(define	(move m n)    
	 (define (move-aux k)  
	   (if (even? (- n (max2_puiss k))) 
	       (put k 2 (max2_puiss k)) 
	       (put k 1 (max2_puiss k))
	   )
	   (if (<> k  m) (move-aux (+ k 1)))
	 )

 	 (define (max2_puiss p)	 
	   (if (even? p) 
	       (+ 1 (max2_puiss (/ p 2)))
	       0)
	 )

	 (define (put no gr p2)	 
	   
	   (define (pow i)
	     (if (= 0 i) 1 (* 2 (pow (- i 1))))
	   )

	   (hanoi_write (+ 1 (modulo (* gr (/ (- (/ no (pow p2)) 1) 2)) 3))
			(- 1)
			(+ 1 (modulo (* gr (/ (+ (/ no (pow p2)) 1) 2)) 3)))

	 )
  (move-aux 1)
)

;since the (runtime) function didn't seem to work, here's a little code
;for waiting a certain number of secs...the 0.2 of the 18.2 ticks timer
;has not been taken into consideration
;        mov     ah,0	     B4 00    ;call the timer, updated 18,2 times per sec
;        int     1Ah	     CD 1A
;        mov     cx,dx	     8B CA    ;store the actual time into cx
;        add     cx,7	     83 C1 07 ;add to cx the number of cycles to wait
;  (1)   push    cx	     51       ;preserve it
;        mov     ah,0	     B4 00
;        int     1Ah	     CD 1A    ;call the timer -> cx=high word, dx=low word
;        pop     cx	     59       ;restore cx
;        cmp     dx,cx	     3B D1    ;compare to the low word: is this the time
;        jb      $ - 8	     72 F6    ;we should stop? if not, go back to (1)
;        retn		     CB
(define (asm_delay)
  (%EXECUTE '(PCS-INLINE-BLOCK 20 (#hB4 #h00 #hCD #h1A #h8B #hCA #h83 #hC1 
				   #h07 #h51 #hB4 #h00 #hCD #h1A #h59 #h3B 
				   #hD1 #h72 #hF6 #hCB)))
)
;(get-char) in a normal language just for a try for inline assembly
(define (presskey) 
  ;push ax        0x50
  ;xor ax,ax      0x33 0xC0 
  ;int 0x16       0xCD 0x16
  ;pop ax         0x58
  ;retf           0xCB
  (%EXECUTE '(PCS-INLINE-BLOCK 5 (#h33 #hC0 #hCD #h16 #hCB)))
)

(writeln "Done compiling.")
