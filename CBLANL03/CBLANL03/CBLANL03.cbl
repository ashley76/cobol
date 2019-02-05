       IDENTIFICATION DIVISION.
	   PROGRAM-ID.			CBLANL03
	   AUTHOR.				ASHLEY LINDQUIST
       DATE-WRITTEN.		1/8/19
	   DATE-COMPILED.		1/9/19
		

      ***************************************************************
      * THIS PROGRAM IS CASE PROBLEM #3.                            *
      * ADDED LEVEL 2 CONTROL BREAK, MINOR.                         *
      * REVISED BOAT REPORT WILL BE GENERATED.                      *
      ***************************************************************
		
		
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

       SELECT BOAT-MASTER
			    ASSIGN TO "C:\COBOL\PROJECTS\CBLANL03\CBLBOAT2.DAT"
				ORGANIZATION IS LINE SEQUENTIAL.
				
	   SELECT PRTOUT
			    ASSIGN TO "C:\COBOL\PROJECTS\CBLRPT.PRT"
				ORGANIZATION IS RECORD SEQUENTIAL.
				
	   DATA DIVISION.
	   FILE SECTION.
		    
	   FD BOAT-MASTER
	      LABEL RECORD IS STANDARD
		  RECORD CONTAINS 42 CHARACTERS
	      DATA RECORD IS I-REC.
			
	   01  I-REC.
		   05  I-LAST-NAME           PIC X(15).
           05  I-STATE               PIC XX.
           05  I-BOAT-COST           PIC 9(6)V99.
           05  I-PURCHASE-YY         PIC 9(4).
           05  I-PURCHASE-MM         PIC 99.
           05  I-PURCHASE-DD         PIC 99.
           05  I-BOAT-TYPE           PIC X.
	       05  I-ACCESS-PACKAGE      PIC 9.
           05  I-PREP-COST           PIC 9(5)V99.
			
	   FD PRTOUT
		    LABEL RECORD IS OMITTED
		    RECORD CONTAINS 132 CHARACTERS
		    LINAGE IS 60 WITH FOOTING AT 55
		    DATA RECORD IS PRTLINE.

	   01 PRTLINE			       PIC X(132).
       
	   WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-MINOR-SALES-CTR   PIC 9(4)        VALUE 0.
           05  C-MINOR-TOTAL       PIC 9(7)V99     VALUE ZERO.
           05  C-PCTR              PIC 99          VALUE ZERO.
           05  C-SUB-SALES-CTR     PIC 9(5)        VALUE ZERO.
           05  C-SUB-TOTAL         PIC S9(10)V99   VALUE ZERO.
           05  C-GT-SALES-CTR      PIC 9(7)        VALUE ZERO.
           05  C-GT-TOTAL          PIC S9(12)V99   VALUE ZERO.

           05  C-ACC-COST          PIC 9(4)V99     VALUE ZERO.
           05  C-PERC              PIC 9V99        VALUE ZERO.
           05  C-TOTAL-SALES       PIC S9(7)V99    VALUE ZERO.

           05  H-BOAT-TYPE         PIC X.
           05  H-STATE             PIC XX.
   
           05  MORE-RECS           PIC XXX         VALUE "YES".
		
	   01  I-DATE.
	       05  I-YEAR           PIC 9(4).
           05  I-MONTH		    PIC 99.
	       05  I-DAY			PIC 99.

       01  BOAT-TYPE-LINE.
           05  FILLER           PIC X(11)   VALUE "BOAT TYPE: ".
           05  O-BOAT-TYPE      PIC X(13).
           05  FILLER           PIC X(108)  VALUE SPACES.
			
	   01  COMPANY-TITLE.
	       05  FILLER			PIC X(6)	VALUE "DATE:".
           05  O-MONTH			PIC 99.
	       05  FILLER			PIC X		VALUE "/".
	       05  O-DAY			PIC 99.
	       05  FILLER			PIC X		VALUE "/".
	       05  O-YEAR 			PIC 9(4).
	       05  FILLER   		PIC X(38) 	VALUE SPACES.
	       05  FILLER			PIC X(33)	VALUE 'LINDQUIST''S BOATS IN
      - 									'C.'.
	       05  FILLER			PIC X(37)	VALUE SPACES.
	       05  FILLER			PIC X(6)	VALUE "PAGE:".
           05  O-PCTR			PIC Z9.
			
	   01  COLUMN-HEADING-1.
           05  FILLER          PIC X(8)    VALUE 'CUSTOMER'.
           05  FILLER          PIC X(36)   VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'BOAT'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'PURCHASE'.
           05  FILLER          PIC X(11)   VALUE SPACES.
           05  FILLER          PIC X(9)    VALUE 'ACCESSORY'.
           05  FILLER          PIC X(21)   VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'PREP'.
           05  FILLER          PIC X(17)   VALUE SPACES.
           05  FILLER          PIC X(5)    VALUE 'TOTAL'.
         
	   01  COLUMN-HEADING-2.
	       05  FILLER 		   PIC X(9)	   VALUE 'LAST NAME'.
           05  FILLER          PIC X(14)   VALUE SPACES.
           05  FILLER          PIC X(5)    VALUE 'STATE'.
           05  FILLER          PIC X(16)   VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'COST'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'DATE'.
           05  FILLER          PIC X(15)   VALUE SPACES.
           05  FILLER          PIC X(7)    VALUE 'PACKAGE'.
           05  FILLER          PIC X(23)   VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'COST'.
           05  FILLER          PIC X(18)   VALUE SPACES.
           05  FILLER          PIC X(4)    VALUE 'COST'.
.

       01  DETAIL-LINE.
           05  O-LAST-NAME         PIC X(16).
           05  FILLER              PIC X(8)        VALUE SPACES.
           05  O-STATE             PIC XX.
           05  FILLER              PIC X(12).
           05  O-BOAT-COST         PIC ZZZ,ZZZ.99.
           05  FILLER              PIC X(9)        VALUE SPACES.
           05  O-PURCHASE-MM       PIC 99.
           05  FILLER              PIC X           VALUE "/".
           05  O-PURCHASE-DD       PIC 99.
           05  FILLER              PIC X           VALUE "/".
           05  O-PURCHASE-YY       PIC 99.
           05  FILLER              PIC X(11)       VALUE SPACES.
           05  O-ACCESS-PACKAGE    PIC X(15).
           05  FILLER              PIC X(9)        VALUE SPACES.
           05  O-PREP-COST         PIC ZZZ,ZZZ.99.
           05  FILLER              PIC X(10)       VALUE SPACES.
           05  O-TOTAL-SALES       PIC Z,ZZZ,ZZZ.99.

       01  MINOR-SUB-LINE.
           05  FILLER              PIC X(10)     VALUE SPACES.
           05  FILLER              PIC X(14)     VALUE 'SUBTOTALS FOR '.
           05  O-MINOR-STATE       PIC XX.
           05  FILLER              PIC X(11)     VALUE SPACES.
           05  O-MINOR-BOAT-TYPE   PIC X(13).
           05  FILLER              PIC X(10)     VALUE SPACES.
           05  FILLER              PIC X(15)     VALUE 'NUMBER SOLD: '.
           05  O-MINOR-SALES-CTR   PIC Z,ZZ9.
           05  FILLER              PIC X(37)     VALUE SPACES.
           05  O-MINOR-TOTAL       PIC $$$$,$$$,$$$.99.
       
       
       01  SUBTOTAL-LINE.
           05  FILLER              PIC X(10)     VALUE SPACES.
           05  FILLER              PIC X(13)     VALUE 'SUBTOTALS FOR '.
           05  FILLER              PIC X(14)     VALUE SPACES.
           05  O-SUB-BOAT-TYPE     PIC X(13).
           05  FILLER              PIC X(10).    
           05  FILLER              PIC X(14)     VALUE 'NUMBER SOLD: '.
           05  O-SUB-SALES-CTR     PIC ZZ,ZZ9.
           05  FILLER              PIC X(34)     VALUE SPACES.
           05  O-SUB-TOTAL         PIC $$$,$$$,$$$,$$$.99.

       
       01  GRANDTOTAL-LINE.
           05  FILLER              PIC X(23)     VALUE SPACES.
           05  FILLER              PIC X(12)     VALUE 'GRAND TOTALS'.
           05  FILLER              PIC X(25)     VALUE SPACES.
           05  FILLER              PIC X(13)     VALUE 'NUMBER SOLD: '.
           05  O-GT-SALES-CTR      PIC ZZZ,ZZ9.
           05  FILLER              PIC X(31)     VALUE SPACES.
           05  O-GT-TOTAL          PIC $$,$$$,$$$,$$$,$$$.99.

       01  BLANK-LINE.
           05  FILLER              PIC X(132)     VALUE SPACES.


       PROCEDURE DIVISION.

       0000-CBLANL03.
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


           OPEN INPUT BOAT-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM 9000-READ.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
           MOVE I-STATE TO H-STATE.
           PERFORM 9900-HEADING.
          

       2000-MAINLINE.
           If H-BOAT-TYPE <> I-BOAT-TYPE
               PERFORM 9200-MINOR-SUB
               PERFORM 9300-SUBTOTAL
           ELSE 
               IF H-STATE <> I-STATE
                   PERFORM 9200-MINOR-SUB
               END-IF
           END-IF.

           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

           
       2100-CALCS.
      
           EVALUATE I-ACCESS-PACKAGE
               WHEN 1
                       MOVE 'ELECTRONICS' TO O-ACCESS-PACKAGE
                       MOVE 5415.30 TO C-ACC-COST
               WHEN 2
                       MOVE 'SKI PACKAGE' TO O-ACCESS-PACKAGE
                       MOVE 3980.00 TO C-ACC-COST
               WHEN 3
                       MOVE 'FISHING PACKAGE' TO O-ACCESS-PACKAGE
                       MOVE 345.45 TO C-ACC-COST
           END-EVALUATE.

           EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE .33 TO C-PERC
               WHEN 'P'
                   MOVE .25 TO C-PERC
               WHEN 'S'
                   MOVE .425 TO C-PERC
               WHEN 'J'
                   MOVE .33 TO C-PERC
               WHEN 'C'
                   MOVE .20 TO C-PERC
               WHEN 'R'
                   MOVE .30 TO C-PERC
           END-EVALUATE.


           COMPUTE C-TOTAL-SALES ROUNDED = ((C-PERC * I-BOAT-COST) + 
           C-ACC-COST+ I-PREP-COST + I-BOAT-COST) * 1.06.
       
      *    -----minor calcs
           COMPUTE C-MINOR-SALES-CTR = C-MINOR-SALES-CTR + 1.
           COMPUTE C-MINOR-TOTAL = C-MINOR-TOTAL + C-TOTAL-SALES.


       2200-OUTPUT.

           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-PREP-COST TO O-PREP-COST.
           MOVE I-PURCHASE-DD TO O-PURCHASE-DD.
           MOVE I-PURCHASE-MM TO O-PURCHASE-MM.
           MOVE I-PURCHASE-YY TO O-PURCHASE-YY.

           MOVE C-TOTAL-SALES TO O-TOTAL-SALES.


           WRITE PRTLINE
               FROM DETAIL-LINE
                   AFTER ADVANCING 1 LINE
                       AT EOP
                           PERFORM 9900-HEADING.


       9200-MINOR-SUB.

           MOVE C-MINOR-SALES-CTR TO O-MINOR-SALES-CTR.
           MOVE C-MINOR-TOTAL TO O-MINOR-TOTAL
           MOVE O-STATE TO O-MINOR-STATE.

           MOVE O-BOAT-TYPE TO O-MINOR-BOAT-TYPE.

           WRITE PRTLINE
               FROM MINOR-SUB-LINE
                   AFTER ADVANCING 2 LINES
                       AT EOP
                           PERFORM 9900-HEADING.
           WRITE PRTLINE
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINES.


           MOVE I-STATE TO H-STATE.

      *    ----majors calcs
           COMPUTE C-SUB-SALES-CTR = C-SUB-SALES-CTR + 
                                                      C-MINOR-SALES-CTR.
           COMPUTE C-SUB-TOTAL = C-SUB-TOTAL + C-MINOR-TOTAL.

           COMPUTE C-MINOR-SALES-CTR = 0.
           COMPUTE C-MINOR-TOTAL = 0.


       9300-SUBTOTAL.

           MOVE C-SUB-SALES-CTR TO O-SUB-SALES-CTR.
           MOVE C-SUB-TOTAL TO O-SUB-TOTAL.

           MOVE O-MINOR-BOAT-TYPE TO O-SUB-BOAT-TYPE.

           PERFORM 9400-BOAT-TYPE.

           WRITE PRTLINE
               FROM SUBTOTAL-LINE
                   AFTER ADVANCING 1 LINES
                       AT EOP
                           PERFORM 9900-HEADING.


           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.

           IF MORE-RECS = 'YES'
               WRITE PRTLINE
                   FROM BOAT-TYPE-LINE
                       AFTER ADVANCING 2 LINES
               WRITE PRTLINE
                   FROM BLANK-LINE
                       AFTER ADVANCING 1 LINE.

      *    do grand total calcs
           COMPUTE C-GT-TOTAL = C-GT-TOTAL + C-SUB-TOTAL.
           COMPUTE C-GT-SALES-CTR = C-GT-SALES-CTR + C-SUB-SALES-CTR.

           COMPUTE C-SUB-SALES-CTR = 0.
           COMPUTE C-SUB-TOTAL = 0.

       9400-BOAT-TYPE.

            EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE 'BASS BOAT' TO O-BOAT-TYPE
                   MOVE 'BASS BOAT' TO O-SUB-BOAT-TYPE
               WHEN 'P'
                   MOVE 'PONTOON' TO O-BOAT-TYPE
               WHEN 'S'
                   MOVE 'SKI BOAT' TO O-BOAT-TYPE
               WHEN 'J'
                   MOVE 'JOHN BOAT' TO O-BOAT-TYPE
               WHEN 'C'
                   MOVE 'CANOE' TO O-BOAT-TYPE
               WHEN 'R'
                   MOVE 'CABIN CRUISER' TO O-BOAT-TYPE
           END-EVALUATE.
          
       3000-CLOSING.

           PERFORM 9200-MINOR-SUB.
           PERFORM 9300-SUBTOTAL.
           PERFORM 3100-GRAND-TOTAL.

           CLOSE BOAT-MASTER.
           CLOSE PRTOUT.


       3100-GRAND-TOTAL.

           MOVE C-GT-SALES-CTR TO O-GT-SALES-CTR.
           MOVE C-GT-TOTAL TO O-GT-TOTAL.

           WRITE PRTLINE
               FROM GRANDTOTAL-LINE
                   AFTER ADVANCING 3 LINES.


       9000-READ.

           READ BOAT-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.


       9900-HEADING.

           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           PERFORM 9400-BOAT-TYPE.

           WRITE PRTLINE
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE
               FROM BOAT-TYPE-LINE
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM COLUMN-HEADING-1
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM COLUMN-HEADING-2
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM BLANK-LINE.
