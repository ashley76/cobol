       IDENTIFICATION DIVISION.
       PROGRAM-ID.         COBANL01
       AUTHOR.
       DATE-WRITTEN.
       DATE-COMPILED.


      *******************************





       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT.



       DATA DIVISION.
       FILE SECTION.

       FD.


       01  LICENSE-REC.
           05  REC-TYPE            PIC X.
           05  HDR-REC.
               10  L-TYPE          PIC 99.
               10  L-DESC          PIC X(34).
       01  DETAIL-REC.
           05  FILLER              PIC X.
           05  D-CUSTID            PIC X(9).
           05  D-CUST-BDAY         PIC 9(8)    COMP-3.
           05  D-CUST-SAFETY-NO    PIC X(10).
           05  D-L-TYPE            PIC 99.
           05  D-SEASON            PIC 9.
           05  D-RESIDENT          PIC X.
           05  D-FEE               PIC 9(3)V99 COMP-3.

       FD PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE.

       01 PRTLINE              PIC X(132).


       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR          PIC 99      VALUE ZERO.
           05  MORE-RECS       PIC XXX     VALUE 'YES'.

           05  X               PIC 9       VALUE ZERO  COMP.
           05  Y               PIC 9       VALUE ZERO  COMP.

           05  GT-D-LIC-CTR    PIC 9(9)    VALUE ZERO.
           05  RES-CTR         PIC 9(9)    VALUE ZERO.
           05  NONRES-CTR      PIC 9(9)    VALUE ZERO.
           05  GT-RES-CTR      PIC 9(9)    VALUE ZERO.
           05  GT-NONRES-CTR   PIC 9(9)    VALUE ZERO.

           05  GT-D-TOT-FEES   PIC 9(9)    VALUE ZERO.

       01  SYS-DATE.
           05  I-YEAR          PIC 9(4).
           05  I-MONTH         PIC 99.
           05  I-DAY           PIC 99.



       01  COMPANY-TITLE-LINE.
           05  FILLER          PIC X(6)    VALUE 'DATE:'.
           05  O-DATE          PIC X(10).
           05  FILLER          PIC X(44)   VALUE SPACES.
           05  FILLER          PIC X(20)   VALUE 'CP1A HUNTING LICENSE'.
           05  FILLER          PIC X(44)   VALUE SPACES.
           05  FILLER          PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR          PIC Z9.

       01  DETAIL-HEADING-LINE.
           05  FILLER          PIC X(8)    VALUE 'COBANL01'.
           05  FILLER          PIC X(55)   VALUE SPACES.
           05  FILLER          PIC X(13)   VALUE 'DETAIL REPORT'.
           05  FILLER          PIC X(56)   VALUE SPACES.

       01  SUMMARY-HEADING-LINE.
           05  FILLER          PIC X(8)    VALUE 'COBANL01'.
           05  FILLER          PIC X(55)   VALUE SPACES.
           05  FILLER          PIC X(14)   VALUE 'SUMMARY REPORT'.
           05  FILLER          PIC X(55)   VALUE SPACES.

       01  AUTHOR-HEADING-LINE.
           05  FILLER          PIC X(62)   VALUE SPACES.
           05  FILLER          PIC X(16)   VALUE 'ASHLEY LINDQUIST'.
           05  FILLER          PIC X(54)   VALUE SPACES.

       01  DETAIL-COLUMN-HEADING.
           05  FILLER          PIC XXX     VALUE SPACES.
           05  FILLER          PIC XX      VALUE 'ID'.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'BIRTHDAY'.
           05  FILLER          PIC X(7)    VALUE SPACES.
           05  FILLER          PIC X(13)   VALUE 'SAFETY NUMBER'.
           05  FILLER          PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE 'LICENSE TYPE'.
           05  FILLER          PIC X(26)   VALUE SPACES.
           05  FILLER          PIC X(6)    VALUE 'SEASON'.
           05  FILLER          PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(21)   VALUE 'RESIDENT/NON-RESIDENT'
      -                                                                .
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC XXX     VALUE 'FEE'.

       01  SUMMARY-COLUMN-HEADING.
           05  FILLER          PIC XXX     VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE 'LICENSE TYPE'.
           05  FILLER          PIC X(27)   VALUE SPACES.
           05  FILLER          PIC X(14)   VALUE 'RESIDENT TOTAL'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(18)   VALUE 'NON-RESIDENT TOTAL'.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  FILLER          PIC X(13)   VALUE 'LICENSES SOLD'.
           05  FILLER          PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(14)   VALUE 'FEES COLLECTED'.

       01  DETAIL-LINE.
           05  FILLER              PIC XXX     VALUE SPACES.
           05  O-CUSTID            PIC 9(9).
           05  FILLER              PIC X(5)    VALUE SPACES.
      *    BIRTHDAY?
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  O-CUST-SAFETY-NO    PIC 9(10).
           05  FILLER              PIC X(8)    VALUE SPACES.
           05  O-D-L-TYPE          PIC X(34).
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  O-SEASON            PIC 9.
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  O-RESIDENT          PIC X(12).
           05  FILLER              PIC X(14)   VALUE SPACES.
           05  O-FEE               PIC $$$$.99.

       01  SUMMARY-LINE.
           05  FILLER              PIC XXX     VALUE SPACES.
           05  O-S-L-TYPE          PIC X(34).
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  O-RES-CTR           PIC ZZ,ZZ9.
           05  FILLER              PIC X(21)   VALUE SPACES.
           05  O-NONRES-CTR        PIC ZZ,ZZ9.
           05  FILLER              PIC X(19)   VALUE SPACES.
           05  O-LICENSE-CTR       PIC ZZ,ZZ9.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  O-TOT-FEES          PIC $$$$,$$$.99.

       01  DETAIL-GT-LINE.
           05  FILLER              PIC X(78)   VALUE SPACES.
           05  FILLER              PIC X(21)   VALUE 'TOTAL LICENSES SOL
      -                                                            'D'.
           05  O-GT-D-LIC-CTR      PIC ZZ9.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  O-GT-D-TOT-FEES     PIC $$,$$$,$$$.99.

       01  SUMMARY-GT-LINE.
           05  FILLER              PIC X(33)   VALUE SPACES.
           05  O-GT-RES-TOT        PIC Z,ZZZ,ZZ9.
           05  FILLER              PIC X(18)   VALUE SPACES.
           05  O-GT-NONRES-TOT     PIC Z,ZZZ,ZZ9.
           05  FILLER              PIC X(16)   VALUE SPACES.
           05  O-GT-S-LIC-CTR      PIC Z,ZZZ,ZZ9.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  O-GT-S-TOT-FEES     PIC $$$,$$$,$$$.99.


      *    ==== HEADER INPUT TABLE ====
       01  HEADER-TABLE.
           05  T-HDR-REC    OCCURS 15 TIMES.
               10  T-L-TYPE        PIC 99.   
               10  T-L-DESC        PIC X(34).


       PROCEDURE DIVISION.

       0000-COBANL01.

           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.



       1000-INIT.

           MOVE FUNCTION CURRENT-DATE TO SYS-DATE.

           STRING I-MONTH '/' I-DAY '/' I-YEAR DELIMITED BY SIZE 
             INTO O-DATE.
      

      *    OPEN INPUT ***
      *    OPEN OUTPUT ***
  

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 15
               MOVE ZERO TO T-L-TYPE(X)
               MOVE SPACES TO T-L-DESC(X)
           END-PERFORM.



           PERFORM 9000-READ.
           PERFORM 9900-DETAIL-HEADING.



       2000-MAINLINE.

           PERFORM 2100-CALCS.
           
           IF REC-TYPE NOT EQUAL TO 'H'
               PERFORM 2200-DETAIL-OUTPUT
           END-IF.
           
           PERFORM 9000-READ.


            
       2100-CALCS.

           IF REC-TYPE = 'H' 
               MOVE L-TYPE TO T-L-TYPE(L-TYPE)
               MOVE L-DESC TO T-L-DESC(L-TYPE)
           ELSE
               ADD 1 TO GT-D-LIC-CTR
           END-IF.


           
           IF D-RESIDENT = 'R' 
               MOVE 'RESIDENT' TO O-RESIDENT
               ADD 1 TO RES-CTR
               ADD 1 TO GT-RES-CTR
           END-IF.

           IF D-RESIDENT = 'N'
               MOVE 'NON-RESIDENT' TO O-RESIDENT
               ADD 1 TO NONRES-CTR
               ADD 1 TO GT-NONRES-CTR
           END-IF.
           


       2200-DETAIL-OUTPUT.

           MOVE D-CUSTID TO O-CUSTID.

      *    MOVE D BIRTHDAY?

           MOVE D-CUST-SAFETY-NO TO O-CUST-SAFETY-NO.
           MOVE T-L-DESC(D-L-TYPE) TO O-D-L-TYPE.
           MOVE D-SEASON TO O-SEASON.
           MOVE D-FEE TO O-FEE.




       3000-CLOSING.

           PERFORM 3100-DETAIL-GT

      *    MOVE .

           PERFORM 3200-SUM-OUTPUT.


      *    CLOSE MASTER***
      *    CLOSE PRT***

       3100-DETAIL-GT.

           MOVE GT-D-LIC-CTR TO O-GT-D-LIC-CTR.
           MOVE GT-D-TOT-FEES TO O-GT-D-TOT-FEES.





       3200-SUM-OUTPUT.

      *    PRINT OUT THE TABLE USING A PERFORM VARYING?


      *    // -- SUMMARY GRAND TOTAL -- //
        
           MOVE GT-D-LIC-CTR TO O-GT-S-LIC-CTR.

           WRITE PRTLINE
               FROM SUMMARY-GT-LINE
                   AFTER ADVANCING 2 LINES.

   
          

       9000-READ.

           READ HUNT
               AT END  
                   MOVE 'NO' TO MORE-RECS.


       9900-DETAIL-HEADING.

           ADD 1 TO C-PCTR. 
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE
               FROM COMPANY-TITLE-LINE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE
               FROM DETAIL-HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE   
               FROM AUTHOR-HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM DETAIL-COLUMN-HEADING
                   AFTER ADVANCING 2 LINES.

       9910-SUM-HEADING.

           MOVE 1 TO C-PCTR. 
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE
               FROM COMPANY-TITLE-LINE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE
               FROM SUMMARY-HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE   
               FROM AUTHOR-HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM SUMMARY-COLUMN-HEADING
                   AFTER ADVANCING 2 LINES.
