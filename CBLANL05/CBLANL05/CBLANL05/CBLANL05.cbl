       IDENTIFICATION DIVISION.
	   PROGRAM-ID.			CBLANL05
	   AUTHOR.				ASHLEY LINDQUIST
       DATE-WRITTEN.		1/28/19
	   DATE-COMPILED.		1/04/19
		

      ***************************************************************
      * THIS PROGRAM IS CASE PROBLEM #5.                            *
      * PERFORM ACCURATE DECISIONS AND VALIDATIONS.                 *
      * VALID AND INVALID REPORTS WILL BE GENERATED.                *
      ***************************************************************
		
		
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

       SELECT POP-MASTER
			    ASSIGN TO "C:\COBOL\PROJECTS\CBLANL05\CBLPOPSL.DAT"
				ORGANIZATION IS LINE SEQUENTIAL.
				
	   SELECT PRTOUT
			    ASSIGN TO "C:\COBOL\PROJECTS\CBLANL05\CBLPOPSL.PRT"
				ORGANIZATION IS RECORD SEQUENTIAL.
       SELECT PRTOUTERR
                ASSIGN TO "C:\COBOL\PROJECTS\CBLANL05\CBLPOPER.PRT"
                ORGANIZATION IS RECORD SEQUENTIAL.
				
	   DATA DIVISION.
	   FILE SECTION.
		    
	   FD POP-MASTER
	      LABEL RECORD IS STANDARD
		  RECORD CONTAINS 71 CHARACTERS
	      DATA RECORD IS I-REC.
			
	   01  I-REC.
		   05  I-LNAME         PIC X(15).
           05  I-FNAME         PIC X(15).
           05  I-ADDRESS       PIC X(15).
           05  I-CITY          PIC X(10).
           05  I-STATE         PIC XX.
               88  VAL-STATE   VALUE 'IA' 'IL' 'MI' 'MO' 'NE' 'WI'.
           05  I-ZIP.           
               10  I-ZIP-1     PIC 99999.
               10  I-ZIP-2     PIC 9999.
           05  I-POP-TYPE      PIC 99.
               88  VAL-POP     VALUE 01 THRU 06.
           05  I-NUM-CASES     PIC 99.
           05  I-TEAM          PIC X.
               88  VAL-TEAM    VALUE 'A' 'B' 'C' 'D' 'E'.

			
	   FD PRTOUT
		    LABEL RECORD IS OMITTED
		    RECORD CONTAINS 132 CHARACTERS
		    LINAGE IS 60 WITH FOOTING AT 52
		    DATA RECORD IS PRTLINE.

       01  PRTLINE			       PIC X(132).

       FD PRTOUTERR
            LABEL RECORD IS OMITTED
            RECORD CONTAINS 132 CHARACTERS
            LINAGE IS 60 WITH FOOTING AT 55
            DATA RECORD IS PRTLINE-ERR.

	  
       01  PRTLINE-ERR             PIC X(132).
       
	   WORKING-STORAGE SECTION.
       01  WORK-AREA.

           05  C-DEPOSIT           PIC 9(9)V99     VALUE ZERO.
           05  C-TOTAL             PIC 9(9)V99     VALUE ZERO.

           05  C-COKE-CASES        PIC 9(5)        VALUE ZERO.
           05  C-DC-CASES          PIC 9(5)        VALUE ZERO.
           05  C-MY-CASES          PIC 9(5)        VALUE ZERO.
           05  C-CHERRY-C-CASES    PIC 9(5)        VALUE ZERO.
           05  C-DC-COKE-CASES     PIC 9(5)        VALUE ZERO.
           05  C-SPRITE-CASES      PIC 9(5)        VALUE ZERO.
   
           05  C-TEAM-A            PIC 9(9)V99      VALUE ZERO.
           05  C-TEAM-B            PIC 9(9)V99      VALUE ZERO.
           05  C-TEAM-C            PIC 9(9)V99      VALUE ZERO.
           05  C-TEAM-D            PIC 9(9)V99      VALUE ZERO.
           05  C-TEAM-E            PIC 9(9)V99      VALUE ZERO.

           05  C-PCTR              PIC 99           VALUE ZERO.
           05  C-ERR-PCTR          PIC 99           VALUE ZERO.
           05  C-ERR-CTR           PIC 9(4)         VALUE ZERO.

           05  ERR-SWITCH          PIC XXX.
           05  MORE-RECS           PIC XXX          VALUE "YES".
		
	   01  I-DATE.
	       05  I-YEAR           PIC 9(4).
           05  I-MONTH		    PIC 99.
	       05  I-DAY			PIC 99.

	   01  COMPANY-TITLE.
	       05  FILLER			PIC X(6)	VALUE "DATE:".
           05  O-MONTH			PIC 99.
	       05  FILLER			PIC X		VALUE "/".
	       05  O-DAY			PIC 99.
	       05  FILLER			PIC X		VALUE "/".
	       05  O-YEAR 			PIC 9(4).
	       05  FILLER   		PIC X(36) 	VALUE SPACES.
	       05  FILLER			PIC X(28)	VALUE 'ALBIA SOCCER CLUB FUN
      -                                                       'DRAISER'.
	       05  FILLER			PIC X(44)	VALUE SPACES.
	       05  FILLER			PIC X(6)	VALUE "PAGE:".
           05  O-PCTR			PIC Z9.

	   01  DIVISION-TITLE.
	       05  FILLER			PIC X(8)	VALUE "CBLANL05".
	       05  FILLER			PIC X(49)	VALUE SPACES.
	       05  FILLER			PIC X(18)	VALUE "LINDQUIST DIVISION".
	       05  FILLER   		PIC X(56) 	VALUE SPACES.

	   01  REPORT-TITLE.
	       05  FILLER			PIC X(60)	VALUE SPACES.
	       05  FILLER			PIC X(12)	VALUE "SALES REPORT".
	       05  FILLER   		PIC X(59) 	VALUE SPACES.

	   01  DETAIL-TITLE.
           05  FILLER           PIC X(3)    VALUE SPACES.
           05  FILLER           PIC X(9)    VALUE 'LAST NAME'.
           05  FILLER           PIC X(8)    VALUE SPACES.
           05  FILLER           PIC X(10)   VALUE 'FIRST NAME'.
           05  FILLER           PIC X(7)    VALUE SPACES.
           05  FILLER           PIC X(4)    VALUE 'CITY'.
           05  FILLER           PIC X(8)    VALUE SPACES.
           05  FILLER           PIC X(14)   VALUE 'STATE ZIP CODE'.
           05  FILLER           PIC X(4)    VALUE SPACES.
           05  FILLER           PIC X(8)    VALUE 'POP TYPE'.
           05  FILLER           PIC X(13)   VALUE SPACES.
           05  FILLER           PIC X(8)    VALUE 'QUANTITY'.
           05  FILLER           PIC X(6)    VALUE SPACES.
           05  FILLER           PIC X(11)   VALUE 'DEPOSIT AMT'.
           05  FILLER           PIC X(6)    VALUE SPACES.
           05  FILLER           PIC X(13)   VALUE 'TOTAL SALES'.

       01  DETAIL-LINE.
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  O-LNAME             PIC X(15).
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-FNAME             PIC X(15).
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-CITY              PIC X(10).
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  O-STATE             PIC XX.
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  O-ZIP-1             PIC 99999.
           05  FILLER              PIC X           VALUE '-'.
           05  O-ZIP-2             PIC 9999.
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-POP-TYPE          PIC X(16).
           05  FILLER              PIC X(8)        VALUE SPACES.
           05  O-NUM-CASES         PIC Z9.
           05  FILLER              PIC X(11)       VALUE SPACES.
           05  O-DEPOSIT-AMT       PIC $$$$.99.
           05  FILLER              PIC X(9)        VALUE SPACES.
           05  O-TOTAL-SALES       PIC $$,$$$.99.
           05  FILLER              PIC X(4)        VALUE SPACES.


       01  GRANDTOTAL-LINE.
           05  FILLER              PIC X(14)     VALUE 'GRAND TOTALS: '.
           05  FILLER              PIC X(118)    VALUE SPACES.

       01  GT-POP-LINE.
           05  FILLER              PIC X(3)      VALUE SPACES.
           05  O-POP-1             PIC X(16).
           05  FILLER              PIC X(1)      VALUE SPACES.
           05  O-NUM-CASES-1       PIC ZZZ,ZZ9. 
           05  FILLER              PIC X(6)      VALUE SPACES.
           05  O-POP-2             PIC X(16).
           05  FILLER              PIC X(1)      VALUE SPACES.
           05  O-NUM-CASES-2       PIC ZZZ,ZZ9. 
           05  FILLER              PIC X(6)      VALUE SPACES.
           05  O-POP-3             PIC X(16).
           05  FILLER              PIC X(1)      VALUE SPACES.
           05  O-NUM-CASES-3       PIC ZZZ,ZZ9. 
           05  FILLER              PIC X(44)     VALUE SPACES.

       01  TEAMTOTAL-LINE.
           05  FILLER              PIC X(12)     VALUE 'TEAM TOTALS: '.
           05  FILLER              PIC X(120)    VALUE SPACES.

       01  TT-GT-LINE.
           05  FILLER              PIC X(3)      VALUE SPACES.
           05  O-TEAM              PIC X.
           05  FILLER              PIC X         VALUE SPACES.
           05  O-TEAM-TOTAL        PIC $$$$,$$$,$$$.99.
           05  FILLER              PIC X(112)    VALUE SPACES.


      *            ERROR PRINTOUT      ========>>
	   01  ERROR-TITLE.
	       05  FILLER		    PIC X(60)	 VALUE SPACES.
	       05  FILLER		    PIC X(12)	 VALUE 'ERROR REPORT'.
	       05  FILLER           PIC X(59)    VALUE SPACES.

       01  ERROR-LINE.
           05  FILLER           PIC X(12)     VALUE 'ERROR RECORD'.
           05  FILLER           PIC X(60)     VALUE SPACES.
           05  FILLER           PIC X(17)     VALUE 'ERROR DESCRIPTION'.
           05  FILLER           PIC X(43)     VALUE SPACES.

       01  ERROR-RECORD.
           05  O-RECORD         PIC X(71).
           05  FILLER           PIC X         VALUE SPACES.
           05  O-ERR-MSG        PIC X(59).

       01  ERROR-TOTAL.
           05  FILLER           PIC X(13)     VALUE 'TOTAL ERRORS '.
           05  O-ERR-CTR        PIC Z,ZZ9.
           05  FILLER           PIC X(113)    VALUE SPACES.



       PROCEDURE DIVISION.

       0000-CBLANL05.
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

           OPEN INPUT POP-MASTER.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT PRTOUTERR.

           PERFORM 9000-READ.
           PERFORM 9100-ERR-HEADING.
           PERFORM 9900-HEADING.


       2000-MAINLINE.
           PERFORM 2100-VALIDATION THRU 2100-EXIT.
               IF ERR-SWITCH = "YES"
                   PERFORM 2200-ERROR-PRT
               ELSE
                   PERFORM 2300-CALCS
                   PERFORM 2400-OUTPUT
               END-IF

           PERFORM 9000-READ.

           
       2100-VALIDATION.

           MOVE "YES" TO ERR-SWITCH.

               IF I-LNAME = SPACES 
                   MOVE "LAST NAME REQUIRED." TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF I-FNAME = SPACES 
                   MOVE "FIRST NAME REQUIRED." TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF I-ADDRESS = SPACES 
                   MOVE "ADDRESS REQUIRED." TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF I-CITY = SPACES 
                   MOVE "CITY REQUIRED." TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF NOT VAL-STATE
                   MOVE 'CHOOSE: IA, IL, MI, MO, NE, WI.' TO O-ERR-MSG
                   GO TO 2100-EXIT                                      
               END-IF.

               IF I-ZIP NOT NUMERIC 
                   MOVE 'ZIP MUST BE NUMERIC.' TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF I-POP-TYPE NOT NUMERIC
                   MOVE 'POP TYPE MUST BE NUMERIC.' TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF NOT VAL-POP
                   MOVE 'CHOOSE: 1-6' TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF I-NUM-CASES NOT NUMERIC 
                   MOVE 'NUMBER OF CASES MUST BE NUMERIC.' TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF I-NUM-CASES = ZERO 
                   MOVE 'MINIMUM OF 1 CASE.' TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

               IF NOT VAL-TEAM
                   MOVE 'CHOOSE A - E.' TO O-ERR-MSG
                   GO TO 2100-EXIT
               END-IF.

           MOVE "NO" TO ERR-SWITCH.


       2100-EXIT.
           EXIT.


       2200-ERROR-PRT.

           MOVE I-REC TO O-RECORD.

           WRITE PRTLINE-ERR
               FROM ERROR-RECORD
                   AFTER ADVANCING 2 LINES
                       AT EOP 
                           PERFORM 9100-ERR-HEADING.

           ADD 1 TO C-ERR-CTR.
      

       2300-CALCS.
      
           IF I-STATE = 'IA' OR 'NE' OR 'WI'
               COMPUTE C-DEPOSIT = 1.2 * I-NUM-CASES
           END-IF.

           IF I-STATE = 'MI'
               COMPUTE C-DEPOSIT = 2.4 * I-NUM-CASES
           END-IF.

           EVALUATE I-POP-TYPE
               WHEN 1
                   MOVE 'COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-COKE-CASES     
               WHEN 2
                   MOVE 'DIET COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-DC-CASES
               WHEN 3
                   MOVE 'MELLO YELLO' TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-MY-CASES 
               WHEN 4
                   MOVE 'CHERRY COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-CHERRY-C-CASES 
               WHEN 5
                   MOVE 'DIET CHERRY COKE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-DC-COKE-CASES 
               WHEN 6
                   MOVE 'SPRITE' TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-SPRITE-CASES 
           END-EVALUATE.


           COMPUTE C-TOTAL = (18.71 * I-NUM-CASES) + C-DEPOSIT.

           EVALUATE I-TEAM
               WHEN 'A'
                   COMPUTE C-TEAM-A = C-TEAM-A + C-TOTAL
               WHEN 'B'
                   COMPUTE C-TEAM-B = C-TEAM-B + C-TOTAL
               WHEN 'C'
                   COMPUTE C-TEAM-C = C-TEAM-C + C-TOTAL
               WHEN 'D'
                   COMPUTE C-TEAM-D = C-TEAM-D + C-TOTAL
               WHEN 'E'
                   COMPUTE C-TEAM-E = C-TEAM-E + C-TOTAL
           END-EVALUATE.


       2400-OUTPUT.
       
           MOVE I-LNAME TO O-LNAME.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-CITY TO O-CITY.
           MOVE I-STATE TO O-STATE.
           MOVE I-ZIP-1 TO O-ZIP-1.
           MOVE I-ZIP-2 TO O-ZIP-2.
           MOVE I-NUM-CASES TO O-NUM-CASES.

           MOVE C-DEPOSIT TO O-DEPOSIT-AMT.
           MOVE C-TOTAL TO O-TOTAL-SALES.

          
           WRITE PRTLINE
               FROM DETAIL-LINE
                   AFTER ADVANCING 2 LINES
                       AT EOP
                           PERFORM 9900-HEADING.


           MOVE 0 TO C-DEPOSIT.
           MOVE 0 TO C-TOTAL.

          
       3000-CLOSING.

           PERFORM 3100-GRAND-TOTAL.
           PERFORM 3200-ERROR-TOTAL.

           CLOSE POP-MASTER.
           CLOSE PRTOUT.
           CLOSE PRTOUTERR.



       3100-GRAND-TOTAL.
  
           PERFORM 9900-HEADING.

           WRITE PRTLINE
               FROM GRANDTOTAL-LINE
                   AFTER ADVANCING 3 LINES.

           
           MOVE 'COKE' TO O-POP-1.
           MOVE C-COKE-CASES TO O-NUM-CASES-1.

           MOVE 'DIET COKE' TO O-POP-2.
           MOVE C-DC-CASES TO O-NUM-CASES-2.

           MOVE 'MELLO YELLO' TO O-POP-3.
           MOVE C-MY-CASES TO O-NUM-CASES-3.

           WRITE PRTLINE     
               FROM GT-POP-LINE
                   AFTER ADVANCING 2 LINES.


           MOVE 'CHERRY COKE' TO O-POP-1.
           MOVE C-CHERRY-C-CASES TO O-NUM-CASES-1.

           MOVE 'DIET CHERRY COKE' TO O-POP-2.
           MOVE C-DC-COKE-CASES TO O-NUM-CASES-2 .

           MOVE 'SPRITE' TO O-POP-3.
           MOVE C-SPRITE-CASES TO O-NUM-CASES-3.

           WRITE PRTLINE
               FROM GT-POP-LINE
                   AFTER ADVANCING 2 LINES.

           PERFORM 3110-TEAMTOTALS.


       3110-TEAMTOTALS.

           WRITE PRTLINE
               FROM TEAMTOTAL-LINE
                   AFTER ADVANCING 3 LINES.


           MOVE 'A' TO O-TEAM.
           MOVE C-TEAM-A TO O-TEAM-TOTAL.

           WRITE PRTLINE
               FROM TT-GT-LINE
                   AFTER ADVANCING 2 LINES.

           MOVE 'B' TO O-TEAM.
           MOVE C-TEAM-B TO O-TEAM-TOTAL.

           WRITE PRTLINE
               FROM TT-GT-LINE
                   AFTER ADVANCING 2 LINES.

           MOVE 'C' TO O-TEAM.
           MOVE C-TEAM-C TO O-TEAM-TOTAL.

           WRITE PRTLINE
               FROM TT-GT-LINE
                   AFTER ADVANCING 2 LINES.

           MOVE 'D' TO O-TEAM.
           MOVE C-TEAM-D TO O-TEAM-TOTAL.

           WRITE PRTLINE
               FROM TT-GT-LINE
                   AFTER ADVANCING 2 LINES.

           MOVE 'E' TO O-TEAM.
           MOVE C-TEAM-E TO O-TEAM-TOTAL.

           WRITE PRTLINE
               FROM TT-GT-LINE
                   AFTER ADVANCING 2 LINES.


       3200-ERROR-TOTAL.

           MOVE C-ERR-CTR TO O-ERR-CTR.

           WRITE PRTLINE-ERR
               FROM ERROR-TOTAL
                   AFTER ADVANCING 3 LINES.
           

       9000-READ.

           READ POP-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.


       9100-ERR-HEADING.

           ADD 1 TO C-ERR-PCTR.
           MOVE C-ERR-PCTR TO O-PCTR.

           WRITE PRTLINE-ERR
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE-ERR
               FROM DIVISION-TITLE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE-ERR
               FROM ERROR-TITLE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE-ERR
               FROM ERROR-LINE
                   AFTER ADVANCING 2 LINES.


       9900-HEADING.

           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE
               FROM DIVISION-TITLE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM REPORT-TITLE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM DETAIL-TITLE
                   AFTER ADVANCING 2 LINES.