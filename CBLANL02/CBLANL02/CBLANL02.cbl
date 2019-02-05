       IDENTIFICATION DIVISION.
	   PROGRAM-ID.			CBLANL02
	   AUTHOR.				ASHLEY LINDQUIST
       DATE-WRITTEN.		12/15/18
	   DATE-COMPILED.		12/20/18
		

      ***************************************************************
      * THIS PROGRAM IS CASE PROBLEM #2A.                           *
      * ADDED LEVEL 1 CONTROL BREAK.                                *
      * BOAT REPORT WILL BE GENERATED.                              *
      ***************************************************************
		
		
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

       SELECT BOAT-MASTER
			    ASSIGN TO "C:\COBOL\PROJECTS\CBLBOAT1.DAT"
				ORGANIZATION IS LINE SEQUENTIAL.
				
	      SELECT PRTOUT
			    ASSIGN TO "C:\COBOL\PROJECTS\BOATRPT.PRT"
				ORGANIZATION IS RECORD SEQUENTIAL.
				
	   DATA DIVISION.
	   FILE SECTION.
		    
	   FD BOAT-MASTER
	      LABEL RECORD IS STANDARD
		  RECORD CONTAINS 49 CHARACTERS
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
           05  I-PREP-DELIVER-COST   PIC 9(5)V99.
			
	   FD PRTOUT
		    LABEL RECORD IS OMITTED
		    RECORD CONTAINS 132 CHARACTERS
		    LINAGE IS 60 WITH FOOTING AT 55
		    DATA RECORD IS PRTLINE.

	   01 PRTLINE			      PIC X(132).
       
	   WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-SALES-CTR         PIC 9(4)        VALUE 0.
           05  C-PCTR              PIC 99          VALUE ZERO.
           05  C-SUB-SALES-CTR     PIC 9(5)        VALUE ZERO.
           05  C-SUB-TOTAL-SALES   PIC S9(10)V99   VALUE ZERO.
           05  C-GT-TOTAL-SALES    PIC S9(12)V99   VALUE ZERO.

           05  C-TOTAL-SALES       PIC S9(7)V99    VALUE ZERO.

           05  H-BOAT-TYPE         PIC X.
   
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
           05  O-PURCHASE-YY       PIC XX.
           05  FILLER              PIC X(11)       VALUE SPACES.
           05  O-ACCESS-PACKAGE    PIC X(15).
           05  FILLER              PIC X(9)        VALUE SPACES.
           05  O-PREP-DELIVER-COST PIC ZZZ,ZZZ.99.
           05  FILLER              PIC X(10)       VALUE SPACES.
           05  O-TOTAL-SALES       PIC Z,ZZZ,ZZZ.99.
       
       
       01  SUBTOTAL-LINE.
           05  FILLER              PIC X(23)     VALUE SPACES.
           05  FILLER              PIC X(14)     VALUE 'SUBTOTALS FOR '.
           05  O-SUB-BOAT-TYPE     PIC X(13).
           05  FILLER              PIC X(10).    
           05  FILLER              PIC X(14)     VALUE 'NUMBER SOLD: '.
           05  O-SUB-SALES-CTR     PIC Z,ZZ9.
           05  FILLER              PIC X(37)     VALUE SPACES.
           05  O-SUB-TOTAL-SALES   PIC $$$$,$$$,$$$.99.

       
       01  GRANDTOTAL-LINE.
           05  FILLER              PIC X(23)     VALUE SPACES.
           05  FILLER              PIC X(12)     VALUE 'GRAND TOTALS'.
           05  FILLER              PIC X(25)     VALUE SPACES.
           05  FILLER              PIC X(13)     VALUE 'NUMBER SOLD: '.
           05  O-SALES-CTR         PIC ZZ,ZZ9.
           05  FILLER              PIC X(34)     VALUE SPACES.
           05  O-GT-TOTAL-SALES    PIC $$$,$$$,$$$,$$$.99.

       01  BLANK-LINE.
           05  FILLER              PIC X(132)     VALUE SPACES.


       PROCEDURE DIVISION.

       0000-CBLANL02.
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
           PERFORM 9100-HEADING.
          

       2000-MAINLINE.
           If H-BOAT-TYPE <> I-BOAT-TYPE
               PERFORM 9200-SUBTOTAL
           END-IF.

           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

           
       2200-OUTPUT.

           EVALUATE I-ACCESS-PACKAGE
               WHEN 1
                       MOVE 'ELECTRONICS' TO O-ACCESS-PACKAGE
               WHEN 2
                       MOVE 'SKI PACKAGE' TO O-ACCESS-PACKAGE
               WHEN 3
                       MOVE 'FISHING PACKAGE' TO O-ACCESS-PACKAGE
           END-EVALUATE.



           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-PREP-DELIVER-COST TO O-PREP-DELIVER-COST.
           MOVE I-PURCHASE-DD TO O-PURCHASE-DD.
           MOVE I-PURCHASE-MM TO O-PURCHASE-MM.
           MOVE I-PURCHASE-YY TO O-PURCHASE-YY.
          
           MOVE C-TOTAL-SALES TO O-TOTAL-SALES.
          

           WRITE PRTLINE
               FROM DETAIL-LINE
                   AFTER ADVANCING 1 LINE
                       AT EOP
                           PERFORM 9100-HEADING.

       9300-BOAT-TYPE.

            EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE 'BASS BOAT' TO O-BOAT-TYPE
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
           

       2100-CALCS.
           COMPUTE C-TOTAL-SALES = I-BOAT-COST + I-PREP-DELIVER-COST.

           COMPUTE C-SUB-SALES-CTR = C-SUB-SALES-CTR + 1.
           COMPUTE C-SUB-TOTAL-SALES = C-SUB-TOTAL-SALES + 
                                                          C-TOTAL-SALES.

       9200-SUBTOTAL.
           MOVE C-SUB-SALES-CTR TO O-SUB-SALES-CTR.
           MOVE C-SUB-TOTAL-SALES TO O-SUB-TOTAL-SALES.


           MOVE O-BOAT-TYPE TO O-SUB-BOAT-TYPE.


           PERFORM 9300-BOAT-TYPE.


           WRITE PRTLINE
               FROM SUBTOTAL-LINE
                   AFTER ADVANCING 2 LINES.

           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.


           IF MORE-RECS = 'YES'
               WRITE PRTLINE
                   FROM BOAT-TYPE-LINE
                       AFTER ADVANCING 2 LINES
               WRITE PRTLINE
                   FROM BLANK-LINE
                       AFTER ADVANCING 1 LINE.

      *    do grand total calcs
           COMPUTE C-GT-TOTAL-SALES = C-GT-TOTAL-SALES +
                                                      C-SUB-TOTAL-SALES.
           COMPUTE C-SALES-CTR = C-SALES-CTR + C-SUB-SALES-CTR.


           COMPUTE C-SUB-SALES-CTR = 0.
           COMPUTE C-SUB-TOTAL-SALES = 0.
           

       3200-GRAND-TOTAL.

           MOVE C-SALES-CTR TO O-SALES-CTR.
           MOVE C-GT-TOTAL-SALES TO O-GT-TOTAL-SALES.

           WRITE PRTLINE
               FROM GRANDTOTAL-LINE
                   AFTER ADVANCING 2 LINES.


       3000-CLOSING.

           PERFORM 9200-SUBTOTAL.
           PERFORM 3200-GRAND-TOTAL.

           CLOSE BOAT-MASTER.
           CLOSE PRTOUT.


       9000-READ.

           READ BOAT-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.

       9100-HEADING.

           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           PERFORM 9300-BOAT-TYPE.

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
