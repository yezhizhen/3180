C My first program
C print out hello world
c CSCI3180 Principles of Programming Languages
c--- Declaration ---
c I declare that the assignment here submitted is original except for source material explicitly acknowledged. I also acknowledge that I am aware of University policy and regulations on honesty in academic work, and of the disciplinary guidelines and procedures applicable to breaches of such policy and regulations, as contained in the website http://www.cuhk.edu.hk/policy/academichonesty/
c   Assignment 1
c   Name: YE ZHIZHEN
c   Student ID: 1155046993
c   Email Addr: yezhizhenjiakang@gmail.com

      PROGRAM dda
C force explicit type declarations
      IMPLICIT NONE
C variable declaration
      INTEGER fd, NUM_OF_RECORDS
      INTEGER X_POINT(99), Y_POINT(99)
      PARAMETER (fd = 77)
      CHARACTER*79 MATRIX(23)
c      data MATRIX/ 1817 * ' '/	  

C Main program
      CALL OPEN_FILE()

      CALL READ_FIRST(NUM_OF_RECORDS)  
      
      CALL INIT_MATRIX(MATRIX)
      CALL READ_CONTENT(X_POINT, Y_POINT, NUM_OF_RECORDS)
      CLOSE(fd)
        
      CALL CALCULATE_AND_FILL(MATRIX, NUM_OF_RECORDS,X_POINT,Y_POINT)      
	  
        
      CALL WRITE_DATA(MATRIX)
        
 11   STOP
      END

	  
C Subroutine for opening file
      SUBROUTINE OPEN_FILE()
      OPEN(UNIT = fd, ERR = 233, FILE = 'input.txt', STATUS = 'OLD')	  
      RETURN
c  for i/o error handling
233   print *, 'errror opening input.txt. program terminates.'
      STOP
      END


      SUBROUTINE WRITE_DATA(MATRIX)
      CHARACTER*79 MATRIX(23)
      INTEGER ROW
      ROW = 1
      
37    if (ROW .LE. 23) then
          WRITE(*,'(A)') MATRIX(ROW)
          ROW = ROW + 1
          GO TO 37
      endif	  
      RETURN 
      END

      SUBROUTINE CALCULATE_AND_FILL(MATRIX, NUM_OF_RECORDS,
     + X_POINT,Y_POINT)      
      CHARACTER*79 MATRIX(23)
      INTEGER NUM_OF_RECORDS
      INTEGER flag
      INTEGER X_POINT(99), Y_POINT(99), i, j
      INTEGER first_x, first_y, larger
      DOUBLE PRECISION slope
      i = 1
      j = 2
      slope = 0
C     compute the slope first
    
      
c     When aligned to y axis  
72    if (X_POINT(i) .EQ. X_POINT(j)) then
        flag = 2
        if (Y_POINT(i) .LT. Y_POINT(j)) then
            first_x = X_POINT(i)
            first_y = Y_POINT(i)
            larger = Y_POINT(j)

        endif
        if (Y_POINT(i) .GT. Y_POINT(j)) then
           	first_x = X_POINT(j)
            first_y = Y_POINT(j)
            larger = Y_POINT(i)	
        endif
        GO TO 129 
      endif
	  
         slope = DBLE( Y_POINT(j) - Y_POINT(i) ) 
     +  / DBLE( X_POINT(j) - X_POINT(i) )
c     The case where |slope| <= 1   	  
        if (slope .LE. 1 .AND. slope .GE. -1) then
            if (X_POINT(i) .LT. X_POINT(j)) then
                flag = 0
			    first_x = X_POINT(i)
                first_y = Y_POINT(i)
                larger = X_POINT(j)
			
            endif
            
            if (X_POINT(i) .GT. X_POINT(j)) then
                flag = 0
                first_x = X_POINT(j)
                first_y = Y_POINT(j)
                larger = X_POINT(i)
            endif
        endif   
C     The case where |slope| > 1
        if (slope .GT. 1 .OR. slope .LT. -1) then
            if (Y_POINT(i) .LT. Y_POINT(j)) then
                flag = 1
                first_x = X_POINT(i)
                first_y = Y_POINT(i)
                larger = Y_POINT(j)
            endif
			
            if (Y_POINT(i) .GT. Y_POINT(j)) then
                flag = 1
		        first_x = X_POINT(j)
                first_y = Y_POINT(j)
                larger = Y_POINT(i)
            endif
        endif
		
129        CALL CALCULATE_TWO_POINTS(MATRIX,first_x, first_y, larger, 
     +  flag,slope)
        i = i+1
        j = j+1		
        if (j .LE. NUM_OF_RECORDS) then
            GO TO 72 
        endif		
		
      return  
      end  
	  

      SUBROUTINE CALCULATE_TWO_POINTS(MATRIX,first_x, first_y, larger, 
     +  flag,slope)
      CHARACTER*79 MATRIX(23)
      INTEGER first_x, first_y, larger, flag, mat_x, mat_y
      DOUBLE PRECISION slope  
      INTEGER travel_x, travel_y
      DOUBLE PRECISION temp
      travel_x = first_x
      travel_y = first_y

144	  mat_x = 23 - travel_y                              
      mat_y = travel_x + 1
          
      if (flag .EQ. 2 .AND. travel_y .LE. larger) then
          MATRIX(mat_x)(mat_y:mat_y) = '*'
          travel_y = travel_y + 1
          GO TO 144  		  
      endif
c   Propagate in x  
      if (flag .EQ. 0 .AND. travel_x .LE. larger) then
          MATRIX(mat_x)(mat_y:mat_y) = '*'
          travel_x = travel_x + 1   
          temp = DBLE(first_y) + slope * (travel_x - first_x)   
          travel_y = NINT(temp)
          GO TO 144
      endif
  
      if (flag .EQ. 1 .AND. travel_y .LE. larger) then
          MATRIX(mat_x)(mat_y:mat_y) = '*'
          travel_y = travel_y + 1	  
          temp = DBLE(first_x) + (1.0 / slope) * (travel_y - first_y)   
          travel_x = NINT(temp)
          GO TO 144  
      endif
      return
      end
	  


      SUBROUTINE INIT_MATRIX(MATRIX)
      CHARACTER*79 MATRIX(23)
      INTEGER counter, ccounter 
      counter = 1
      ccounter = 1
c  Init the whole matrix to spaces
59    if (counter .LE. 23) then
60      if (ccounter .LE. 79) then
             MATRIX(counter)(ccounter:ccounter) =' ' 
             ccounter = ccounter + 1
             GO TO 60 
        endif
        ccounter = 1
        counter = counter + 1
        GO TO 59
      ENDIF
      
      counter = 2
C     Init x-axis. Start with (23,2). (in axis, which is (1,0))
61    if (counter .LT. 80) then
        MATRIX(23)(counter:counter) = '-' 
        counter = counter + 1
        GO TO 61
      ENDIF

C     Init y-axis. Start with (22,1). (in axis, which is (0,1))
      counter = 22
70    if (counter .GE. 1) then
          MATRIX(counter)(1:1) = '|'
          counter = counter -1
          GO TO 70
      endif
      MATRIX(23)(1:1) = '+' 	  
      return
      end
      
	  
	  
      SUBROUTINE READ_CONTENT(X_POINT, Y_POINT, NUM_OF_RECORDS)
      INTEGER X_POINT(99), Y_POINT(99)
      INTEGER counter
      counter = 1
35    if (counter .LE. NUM_OF_RECORDS) then
          READ(fd, 99) X_POINT(counter), Y_POINT(counter)
          counter = counter + 1
          GO TO 35 	  
      endif
99    format(I2, X ,I2)
      RETURN
      END
	  
	  
C   READ the number of points	  
      SUBROUTINE READ_FIRST(FIRST)
      INTEGER FIRST

      READ(fd, 100) FIRST
100	  format (I2)
      return
      end
	  
	  
