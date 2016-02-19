      * CSCI3180 Principles of Programming Languages
      * --- Declaration ---
      * I declare that the assignment here submitted is original except for source material explicitly acknowledged. I also acknowledge that I am aware of University policy and regulations on honesty in academic work, and of the disciplinary guidelines and procedures applicable to breaches of such policy and regulations, as contained in the website http://www.cuhk.edu.hk/policy/academichonesty/
      * Assignment 1
      * Name: YE ZHIZHEN
      * Student ID: 1155046993
      * Email Addr: yezhizhenjiakang@gmail.com
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. DDA.
000300 ENVIRONMENT DIVISION.
000500 INPUT-OUTPUT SECTION.
      * Setting up configuration
000600 FILE-CONTROL.
000700     SELECT INPUT-FILE
000800         ASSIGN TO DISK
000900         ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS in-file.
001000     SELECT OUTPUT-FILE
000800         ASSIGN TO DISK
000900         ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS out-file.
000400 DATA DIVISION.
       FILE SECTION.
      *Set up reading file
       FD   INPUT-FILE
            LABEL RECORDS ARE STANDARD
            DATA RECORD IS DATAS
            VALUE OF FILE-ID IS "input.txt".
       01   DATAS.
             03    X-COO        PIC  99.
             03    FILLER       PIC   X.       
             03    Y-COO        PIC  99.

       FD   OUTPUT-FILE
            LABEL RECORDS ARE STANDARD
            DATA RECORD IS ROW
            VALUE OF FILE-ID IS "output.txt".
       01   ROW.
            03    FOR-OUT   PIC X  OCCURS 79.

       WORKING-STORAGE SECTION
       01   in-file     PIC XX.
       01   out-file    PIC XX.
      * Index used to iterate through all points.
      * TEMP-X, TEMP-Y as integer to fill in grids.
       01   TEMP-X   PIC 99.
       01   TEMP-Y   PIC 99.
       01   INDEX-I  PIC 99   VALUE 1.
       01   INDEX-J  PIC 99   VALUE 2.
       01   TRAVELLER-X    PIC 99.
       01   TRAVELLER-Y    PIC 99.
       01   SLOPE    PIC  S99V9(5).              
       01   COUNTER     PIC 99.
       01   Y-PRINT     PIC 99    VALUE 1.
      *0 represents proceed in x. 1 represents in y, 2 represents aligning with y.
       01   X-Y      PIC 9.
      *Two pointers. First point to the one with lower x or y. Second to larger.
       01   FIRST-POINT-X   PIC 99.
       01   FIRST-POINT-Y   PIC 99.
       01   SECOND-POINTER  PIC 99.
       01   NUM-OF-RECORDS  PIC 99.	   
       01   FLOAT           PIC 99V9(5).
       01   TMP.
            02  DIGIT       PIC 9     OCCURS  2.
       01   POINT-SET.
            02  X-POINT   PIC 99  OCCURS  99.  
            02  Y-POINT   PIC 99  OCCURS  99.
       01   MATRIX.
            03  ROWS  OCCURS  23.
                05  SYMBOLS   PIC X   OCCURS 79.
                
			
000500 PROCEDURE DIVISION.
       
       MAIN-LOGIC.
             PERFORM FILE-OPEN.
             PERFORM INIT-MATRIX.          
             PERFORM READ-FIRST.
      *From now on, use COUNTER to iterate set. Starting with 1.
             PERFORM READ-CONTENT.
             
      *Calculate lines for every pair
             PERFORM CALCULATE-AND-FILL.             
      
      *Write the temporary table into the file. Starting from y = 22. Y-PRINT = 23.
             PERFORM WRITE-FILE.
             PERFORM FILE-CLOSE.
             GO TO PROGRAM-DONE.			 
 
000800 PROGRAM-DONE.
000900     STOP RUN.



      * READ the first row. Store in NUM-OF-RECORDS.
       READ-FIRST.
            READ INPUT-FILE.
            PERFORM PROCESSING-FIRST.
      
      *READ the content   
	  
       READ-CONTENT.
	     
            IF  COUNTER not > NUM-OF-RECORDS
                        
                READ INPUT-FILE
                PERFORM PROCESSING-CONTENT
				ADD  1  TO COUNTER
				GO TO READ-CONTENT.
            				
       PROCESSING-FIRST.
            MOVE X-COO TO TMP.
            MOVE DIGIT(2) TO NUM-OF-RECORDS.            
            IF DIGIT(1) NOT = SPACE
                 MOVE TMP TO NUM-OF-RECORDS.
       
       PROCESSING-CONTENT.
      *Data is read into X-COO, Y-COO. When try to put them into POINT-SET.
              MOVE X-COO TO TMP.
              MOVE DIGIT(2) TO X-POINT(COUNTER).            
              IF DIGIT(1) NOT = SPACE
                   MOVE TMP TO X-POINT(COUNTER).
             
              MOVE Y-COO TO TMP.
              MOVE DIGIT(2) TO Y-POINT(COUNTER).            
              IF DIGIT(1) NOT = SPACE
                   MOVE TMP TO Y-POINT(COUNTER).
             

       WRITE-FILE.
            IF Y-PRINT  NOT > 23
                MOVE ROWS(Y-PRINT) TO ROW
                WRITE ROW
                ADD 1 TO Y-PRINT
                GO TO WRITE-FILE.
             
       INIT-MATRIX.
            MOVE SPACE TO MATRIX.
            PERFORM INIT-AXIS.

      *Initialize the axis points first 
       INIT-AXIS.
            MOVE '+' TO SYMBOLS(23, 1).
            MOVE 2 TO COUNTER.
            PERFORM INIT-X.
            MOVE 22 TO COUNTER.
            PERFORM INIT-Y.
            MOVE 1 TO COUNTER.
       
       INIT-X.
      *start with (23,2). (in axis, which is (1,0))
            IF COUNTER < 80  
                MOVE '-' TO SYMBOLS(23, COUNTER)
                ADD 1 TO COUNTER
                GO TO INIT-X.

       INIT-Y.
      *start with (22,1). (in axis, which is (0,1))
            IF COUNTER not < 1
                MOVE '|' TO SYMBOLS(COUNTER, 1)
                SUBTRACT 1 FROM COUNTER
                GO TO INIT-Y.
				

      * Main logic for calculation
       CALCULATE-AND-FILL.
      *Specifies when and where to calculate
      *Calculate slope, determine X-Y, Set up traveller point.
	        COMPUTE SLOPE = ( Y-POINT(INDEX-J) - 
			Y-POINT(INDEX-I) ) / ( X-POINT(INDEX-J) 
             - X-POINT(INDEX-I) ).
      *First, set up which end to start
           
      *The case aligning to y axis ...
            IF X-POINT(INDEX-I) = X-POINT(INDEX-J) 
             AND Y-POINT(INDEX-I) < Y-POINT(INDEX-J) 
                  MOVE 2 TO X-Y
                  MOVE Y-POINT(INDEX-I) TO TRAVELLER-Y
			      MOVE X-POINT(INDEX-I) TO TRAVELLER-X
                  MOVE Y-POINT(INDEX-J) TO SECOND-POINTER.
               
	  

            IF X-POINT(INDEX-I) = X-POINT(INDEX-J) 
			   AND Y-POINT(INDEX-I) > Y-POINT(INDEX-J)
                  MOVE 2 TO X-Y
                  MOVE X-POINT(INDEX-J) TO TRAVELLER-X
                  MOVE Y-POINT(INDEX-J) TO TRAVELLER-Y
                  MOVE Y-POINT(INDEX-I) TO SECOND-POINTER.
		 
      *The case with |slope| NOT larger than 1.
            IF SLOPE NOT > 1 AND 
			    SLOPE NOT < -1 AND 
                X-POINT(INDEX-I) < X-POINT(INDEX-J)
                   MOVE 0 TO X-Y
                   MOVE X-POINT(INDEX-I) TO TRAVELLER-X
                   MOVE Y-POINT(INDEX-I) TO TRAVELLER-Y
                   MOVE X-POINT(INDEX-J) TO SECOND-POINTER.

            IF SLOPE NOT > 1 AND 
			    SLOPE NOT < -1 AND 
                X-POINT(INDEX-J) < X-POINT(INDEX-I)   
                   MOVE 0 TO X-Y
                   MOVE X-POINT(INDEX-J) TO TRAVELLER-X
                   MOVE Y-POINT(INDEX-J) TO TRAVELLER-Y
                   MOVE X-POINT(INDEX-I) TO SECOND-POINTER.	
      *The case with |slope|  larger than 1.
            IF ( SLOPE  > 1 OR 
			    SLOPE  < -1 ) AND 
                Y-POINT(INDEX-I) < Y-POINT(INDEX-J)
                   MOVE 1 TO X-Y
                   MOVE X-POINT(INDEX-I) TO TRAVELLER-X
                   MOVE Y-POINT(INDEX-I) TO TRAVELLER-Y
                      
                   MOVE Y-POINT(INDEX-J) TO SECOND-POINTER.

            IF ( SLOPE > 1 OR 
			    SLOPE < -1 ) AND 
                Y-POINT(INDEX-J) < Y-POINT(INDEX-I)   
                   
                   MOVE 1 TO X-Y
                   MOVE X-POINT(INDEX-J) TO TRAVELLER-X
                   MOVE Y-POINT(INDEX-J) TO TRAVELLER-Y
                   MOVE Y-POINT(INDEX-I) TO SECOND-POINTER.	
                   
   
            MOVE TRAVELLER-X TO FIRST-POINT-X.
            MOVE TRAVELLER-Y TO FIRST-POINT-Y.
            PERFORM CALCULATE-TWO-POINTS.       
            ADD 1 TO INDEX-J.
            ADD 1 TO INDEX-I.				   
				   
            IF INDEX-J NOT > NUM-OF-RECORDS
                   GO TO CALCULATE-AND-FILL.
			
	
      *Here we have the INDEX-I, INDEX-J. We update the line between two points here.
       CALCULATE-TWO-POINTS.
            COMPUTE TEMP-X = 23 - TRAVELLER-Y
            COMPUTE TEMP-Y = TRAVELLER-X + 1
      *Trivial case here, aligning with y axis.
          	IF X-Y = 2
                AND TRAVELLER-Y NOT > SECOND-POINTER 
      *Now, set (TRAVELLER-X ,TRAVELLER-Y), WHICH IS (23-TRAVELLER-Y , TRAVELLER-X + 1)
                      
                      MOVE '*' TO SYMBOLS(TEMP-X, TEMP-Y)
                      ADD 1 TO TRAVELLER-Y
                      GO TO CALCULATE-TWO-POINTS.					  
      * For this case, we propagate in x axis.
            
            IF X-Y = 0 AND TRAVELLER-X NOT > SECOND-POINTER
               MOVE '*' TO SYMBOLS(TEMP-X, TEMP-Y)  
               ADD 1 TO TRAVELLER-X
      * Compute current traveller-y
               COMPUTE FLOAT  =  FIRST-POINT-Y 
                + ( SLOPE * (TRAVELLER-X - FIRST-POINT-X) )
               COMPUTE TRAVELLER-Y ROUNDED = FLOAT
               GO TO CALCULATE-TWO-POINTS.				   

      * For this case, we propagate in y axis.
            IF X-Y = 1 AND TRAVELLER-Y NOT > SECOND-POINTER

               MOVE '*' TO SYMBOLS(TEMP-X, TEMP-Y)
               
               ADD 1 TO TRAVELLER-Y
      * Compute current traveller-x
               COMPUTE FLOAT =  FIRST-POINT-X +
               ( 1 / SLOPE * (TRAVELLER-Y - FIRST-POINT-Y) )
               COMPUTE TRAVELLER-X ROUNDED = FLOAT 
               GO TO CALCULATE-TWO-POINTS.	
               			   
                        
				
				
				
				
				
				
				
				
				
				

				
				

      *Start opening the file 
       FILE-OPEN.
            OPEN INPUT INPUT-FILE.
            IF in-file  not = '00'
                DISPLAY "Fail to open the file"
                  " input.txt. Program terminates."
                 GO TO PROGRAM-DONE.

            OPEN OUTPUT OUTPUT-FILE.
            IF out-file not = '00'
                 DISPLAY "Fail to open the file"
                  " output.txt. Program terminates"
                 GO TO PROGRAM-DONE.
             
      * Close the file
       FILE-CLOSE.
            CLOSE INPUT-FILE OUTPUT-FILE.   
