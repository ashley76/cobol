       IDENTIFICATION DIVISION.
	   PROGRAM-ID.			CBLANL01
	   AUTHOR.				ASHLEY LINDQUIST
       DATE-WRITTEN.		12/10/18
	   DATE-COMPILED.		
		

      ***************************************************************
      * THIS PROGRAM IS CASE PROBLEM #1A.                           *
      *                                                             *
      * PIZZA REPORT WILL BE GENERATED.                             *
      ***************************************************************
		
		
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

       SELECT STUDENT-MASTER
			    ASSIGN TO "C:\COBOL\bob.dat"
				ORGANIZATION IS LINE SEQUENTIAL.
				
	      SELECT PRTOUT
			    ASSIGN TO "C:\COBOL\PROJECTS\PIZZARPT.PRT"
				ORGANIZATION IS RECORD SEQUENTIAL.
				
	   DATA DIVISION.
	   FILE SECTION.
		    
	   FD STUDENT-MASTER
	      LABEL RECORD IS STANDARD
		  RECORD CONTAINS 26 CHARACTERS
	      DATA RECORD IS I-PIZZA-REC.
			
	   01  I-PIZZA-REC.
		    05  I-PIZZA-ITEM-NO.
                10  I-PIZZA-ITEM-ONE   PIC 9.
                10  I-PIZZA-ITEM-TWO   PIC 9.
                10  I-PIZZA-ITEM-THREE PIC 99.
			05  I-PIZZA-CUR-DATE.
				10  I-PIZZA-CUR-YY	   PIC X(4).
				10  I-PIZZA-CUR-MM	   PIC 99.
				10  I-PIZZA-CUR-DD	   PIC 99.
			05  I-PIZZA-PRICE		   PIC 99V99.
            05  I-PIZZA-CUR-QTY        PIC 9(5).
            05  I-PIZZA-PREV-QTY       PIC 9(5).
			
			
	   FD PRTOUT
		    LABEL RECORD IS OMITTED
		    RECORD CONTAINS 132 CHARACTERS
		    LINAGE IS 60 WITH FOOTING AT 55
		    DATA RECORD IS PRTLINE.

	   01 PRTLINE			    PIC X(132).
		
	   WORKING-STORAGE SECTION.
	   01 WORK-AREA.
	      05  C-SCTR            PIC S999		VALUE 0.
	      05  C-PCTR            PIC S99		    VALUE ZERO.
          05  C-PQCTR           PIC S9(6)       VALUE ZERO.
          05  C-PIZZA-AMT       PIC S9(5).
          05  C-PIZZA-PERCENT   PIC S9(5).
          05  C-TOTAL-SALES     PIC S9(11)V99.
          05  GT-PIZZA-AMT      PIC S9(11)      VALUE ZERO.
          05  GT-TOTAL-SALES    PIC S9(16)V99   VALUE ZERO.
          05  AVG-PIZZA-AMT     PIC S9(8)       VALUE ZERO.
          05  AVG-PIZZA-PERCENT PIC S9(6)       VALUE ZERO.
	      05  MORE-RECS         PIC XXX		    VALUE "YES".
		
	   01 I-DATE.
	      05  I-YEAR            PIC 9(4).
          05  I-MONTH			PIC 99.
	      05  I-DAY			    PIC 99.
			
	   01 COMPANY-TITLE.
	      05  FILLER			PIC X(6)	VALUE "DATE:".
          05  O-MONTH			PIC 99.
	      05  FILLER			PIC X		VALUE "/".
	      05  O-DAY			    PIC 99.
	      05  FILLER			PIC X		VALUE "/".
	      05  O-YEAR 			PIC 9(4).
	      05  FILLER   		    PIC X(38) 	VALUE SPACES.
	      05  FILLER			PIC X(33)	VALUE 'LINDQUIST''S MONTHLY
      - 									'SALES - PIZZA'.
	      05  FILLER			PIC X(37)	VALUE SPACES.
	      05  FILLER			PIC X(6)	VALUE "PAGE:".
          05  O-PCTR			PIC Z9.
			
			
	   01  COLUMN-HEADING-1.
		   05  FILLER 		   PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'ITEM'.
           05  FILLER          PIC X(23)   VALUE SPACES.
           05  FILLER          PIC X(5)    VALUE 'PRIOR'.
           05  FILLER          PIC X(7)    VALUE SPACES.
           05  FILLER          PIC X(7)    VALUE 'CURRENT'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(14)   VALUE 'SALES INCREASE'.
           05  FILLER          PIC X(8)    VALUE SPACES.
           05  FILLER          PIC X(9)    VALUE 'INCR/DECR'.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'SALE'.
           05  FILLER          PIC X(27)   VALUE SPACES.
         
	   01  COLUMN-HEADING-2.
           05  FILLER          PIC X(4)    VALUE SPACES.
	       05  FILLER 		   PIC X(6)	   VALUE 'NUMBER'.
           05  FILLER          PIC X(4)    VALUE SPACES.
           05  FILLER          PIC X(10)   VALUE 'SALES DATE'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(3)    VALUE 'QTY'.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(3)    VALUE 'QTY'.
           05  FILLER          PIC X(11)   VALUE SPACES.
           05  FILLER          PIC X(13)   VALUE '/DECREASE AMT'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(10)   VALUE 'PERCENTAGE'.
           05  FILLER          PIC X(8)    VALUE SPACES.
           05  FILLER          PIC X(5)    VALUE 'PRICE'.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'TOTAL SALES'.

       01  DETAIL-LINE.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  O-PIZZA-ITEM-NO.         
               10  O-PIZZA-ITEM-ONE    PIC X.
               10  FILLER			   PIC X	VALUE "-".
               10  O-PIZZA-ITEM-TWO    PIC X.
               10  FILLER			   PIC X	VALUE "-".
               10  O-PIZZA-ITEM-THREE  PIC XX.

           05  FILLER              PIC X(4)  VALUE SPACES.
           05  O-PIZZA-CUR-MM      PIC 99.
           05  FILLER			   PIC X	 VALUE "/".
           05  O-PIZZA-CUR-DD      PIC 99.
           05  FILLER              PIC X     VALUE "/".
           05  O-PIZZA-CUR-YY      PIC X(4).

           05  FILLER              PIC X(7)  VALUE SPACES.
           05  O-PIZZA-PREV-QTY    PIC ZZ,ZZ9.
           05  FILLER              PIC X(8)  VALUE SPACES.
           05  O-PIZZA-CUR-QTY     PIC ZZ,ZZ9.
           05  FILLER              PIC X(12) VALUE SPACES.
           05  O-PIZZA-AMT         PIC ZZ,ZZ9B-.
           05  FILLER              PIC X(13) VALUE SPACES.
           05  O-PIZZA-PERCENT     PIC +++9.
           05  FILLER              PIC X     VALUE '%'.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  O-PIZZA-PRICE       PIC $$$.99.
           05  FILLER              PIC X(7)  VALUE SPACES.
           05  O-TOTAL-SALES       PIC $$$,$$$,$$$.99.


       01  GRANDTOTAL-LINE.
           05  FILLER              PIC X(45)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE 'GRAND TOTALS:'.
           05  O-GT-PIZZA-AMT      PIC Z,ZZZ,ZZ9B-.
           05  FILLER              PIC X(38).
           05  O-GT-TOTAL-SALES    PIC $$,$$$,$$$,$$$.99.
           05  FILLER              PIC X(7)    VALUE SPACES.

       01  AVG-AMT-LINE.
           05  FILLER              PIC X(25)   VALUE SPACES.
           05  FILLER              PIC X(38)   VALUE 
                                    'AVERAGE INCREASE/DECREASE AMOUNT:'.
           05  O-AVG-PIZZA-AMT     PIC ZZ,ZZ9B-.
           05  FILLER              PIC X(63)   VALUE SPACES.

       01  AVG-PERCENT-LINE.
           05  FILLER              PIC X(21)   VALUE SPACES.
           05  FILLER              PIC X(44)   VALUE
                                'AVERAGE INCREASE/DECREASE PERCENTAGE:'.
           05  O-AVG-PIZZA-PERCENT PIC +++9.
           05  FILLER              PIC XX      VALUE " %".
           05  FILLER              PIC X(63)   VALUE SPACES.


       PROCEDURE DIVISION.

       0000-CBLANL01.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.


       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO I-DATE.
           MOVE I-DAY TO O-DAY.
           MOVE I-YEAR TO O-YEAR.
           MOVE I-MONTH TO O-MONTH.

           OPEN INPUT STUDENT-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM 9100-HEADING.
           PERFORM 9000-READ.

       2000-MAINLINE.
           PERFORM 2200-CALCS.
           PERFORM 2100-OUTPUT.
           PERFORM 9000-READ.
           
       2100-OUTPUT.
           MOVE I-PIZZA-ITEM-ONE TO O-PIZZA-ITEM-ONE.
           MOVE I-PIZZA-ITEM-TWO TO O-PIZZA-ITEM-TWO.
           MOVE I-PIZZA-ITEM-THREE TO O-PIZZA-ITEM-THREE.
           MOVE I-PIZZA-CUR-MM TO O-PIZZA-CUR-MM.
           MOVE I-PIZZA-CUR-DD TO O-PIZZA-CUR-DD.
           MOVE I-PIZZA-CUR-YY TO O-PIZZA-CUR-YY.
           MOVE I-PIZZA-PREV-QTY TO O-PIZZA-PREV-QTY.
           MOVE I-PIZZA-CUR-QTY TO O-PIZZA-CUR-QTY.
           MOVE I-PIZZA-PRICE TO O-PIZZA-PRICE.
           MOVE C-PIZZA-AMT TO O-PIZZA-AMT.
           MOVE C-PIZZA-PERCENT TO O-PIZZA-PERCENT.
           MOVE C-TOTAL-SALES TO O-TOTAL-SALES.
          

           WRITE PRTLINE
               FROM DETAIL-LINE
                   AFTER ADVANCING 2 LINES
                       AT EOP
                           PERFORM 9100-HEADING.


       2200-CALCS.
           ADD 1 TO C-SCTR.
      *    
           ADD I-PIZZA-PREV-QTY TO C-PQCTR GIVING C-PQCTR.

      *    SALES IN/DEC AMT = CUR QT - PRIOR QTY
           SUBTRACT I-PIZZA-PREV-QTY FROM I-PIZZA-CUR-QTY
               GIVING C-PIZZA-AMT.

      *    PERCENT IN/DEC AMT = ABOVE / PRIOR
           COMPUTE C-PIZZA-PERCENT ROUNDED = C-PIZZA-AMT/
                                           I-PIZZA-PREV-QTY * 100.

      *    TOTAL SALES = CUR QTY * PIZZA PRICE
           MULTIPLY I-PIZZA-CUR-QTY BY I-PIZZA-PRICE
               GIVING C-TOTAL-SALES.

      *    AMT DIFFERENCE PUT INTO GRAND TOTAL PIZZA AMT
           ADD C-PIZZA-AMT TO GT-PIZZA-AMT GIVING GT-PIZZA-AMT.
      *    TOTAL SALES PUT INTO GRAND TOTAL PIZZA TOTAL SALES
           ADD C-TOTAL-SALES TO GT-TOTAL-SALES GIVING GT-TOTAL-SALES.
 
           

       3000-CLOSING.

           DIVIDE GT-PIZZA-AMT BY C-SCTR GIVING AVG-PIZZA-AMT.
           COMPUTE AVG-PIZZA-PERCENT = GT-PIZZA-AMT / C-PQCTR * 100.

           MOVE GT-TOTAL-SALES TO O-GT-TOTAL-SALES.
           MOVE GT-PIZZA-AMT TO O-GT-PIZZA-AMT.
           MOVE AVG-PIZZA-AMT TO O-AVG-PIZZA-AMT.
           MOVE AVG-PIZZA-PERCENT TO O-AVG-PIZZA-PERCENT.


           WRITE PRTLINE
               FROM GRANDTOTAL-LINE
                   AFTER ADVANCING 3 LINES.
           WRITE PRTLINE
               FROM AVG-AMT-LINE
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM AVG-PERCENT-LINE
                   AFTER ADVANCING 2 LINES.
           CLOSE STUDENT-MASTER.
           CLOSE PRTOUT.


       9000-READ.
           READ STUDENT-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.

       9100-HEADING.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE
               FROM COLUMN-HEADING-1
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM COLUMN-HEADING-2
                   AFTER ADVANCING 1 LINE.
