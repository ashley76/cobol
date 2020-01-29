      



      *DETAIL LINES
      *
      *SUBTOTAL BY ATTEND DATE MONTH
      *    - ATTEND DATE MONTH AND TOTAL SALES
      *
      *GRAND TOTAL OF TOTAL SALES


       IDENTIFICATION DIVISION.
       PROGRAM-ID.             COBANL2C
       AUTHOR.                 Ashley Lindquist
       DATE-WRITTEN.           7-22-19
       DATE-COMPILED.


      ***********************************************
      *  - Amusement Park Ticket Sales - 2C -       *
      * Program utilizes 2D Hard-Coded Table.       *
      * Uses sorted Park Dat, Subtotaling By Month. *
      ***********************************************




       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT PARK-MASTER
           ASSIGN TO PRKDAT3
           ORGANIZATION IS SEQUENTIAL.

           SELECT PARK-PRTOUT
           ASSIGN TO SUBPRT3.


       DATA DIVISION.
       FILE SECTION.

       FD  PARK-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS DETAIL-REC
           RECORD CONTAINS 14 CHARACTERS
           RECORDING MODE F.


       01  DETAIL-REC.
           05  I-ATTEND-DATE       PIC 9(8)    COMP-3.
           05  I-PARK              PIC 99.
           05  I-DISCOUNT-CODE     PIC X.
           05  I-ADULT-TICKETS     PIC 99.
           05  I-JR-TICKETS        PIC 99.
           05  I-SENIOR-TICKETS    PIC 99.
     



       FD PARK-PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE
           RECORDING MODE F.

       01 PRTLINE              PIC X(132).


       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR          PIC 99        VALUE ZERO.
           05  MORE-RECS       PIC XXX       VALUE 'YES'.

           05  MONTH-HOLD      PIC 99        VALUE ZERO.

           05  C-SUBTOTAL      PIC 9(6)V99   VALUE ZERO.
           05  I-DISCOUNT      PIC 9V99      VALUE ZERO.
           05  C-DISCOUNT      PIC 9(2)V99   VALUE ZERO.
           05  C-TOTAL-COST    PIC 9(9)V99   VALUE ZERO.

           05  C-AMUSE-SUB     PIC 9(9)V99   VALUE ZERO.

           05  C-GT            PIC 9(11)V99  VALUE ZERO.

           05  PACKED-ATTEND-DATE.
               10  ATTEND-YEAR     PIC 9(4).
               10  ATTEND-MONTH    PIC 99.
               10  ATTEND-DAY      PIC 99.
           05  ATTEND-DATE  PIC 9(8)  REDEFINES  PACKED-ATTEND-DATE.

           

       01  AMUSEMENT-ARRAY.
           05  FILLER    PIC X(25)    VALUE 'ADVENTURELAND            '.
           05  FILLER    PIC 9(3)V99  VALUE 11.10.
           05  FILLER    PIC 9(3)V99  VALUE 12.10.
           05  FILLER    PIC 9(3)V99  VALUE 13.10.
           05  FILLER    PIC X(25)    VALUE 'ZOMBIELAND               '.
           05  FILLER    PIC 9(3)V99  VALUE 14.10.
           05  FILLER    PIC 9(3)V99  VALUE 15.10.
           05  FILLER    PIC 9(3)V99  VALUE 16.10.
           05  FILLER    PIC X(25)    VALUE 'WONDERLAND               '.
           05  FILLER    PIC 9(3)V99  VALUE 17.10.
           05  FILLER    PIC 9(3)V99  VALUE 18.10.
           05  FILLER    PIC 9(3)V99  VALUE 19.10.
           05  FILLER    PIC X(25)    VALUE 'DISNEYLAND               '.
           05  FILLER    PIC 9(3)V99  VALUE 20.10.
           05  FILLER    PIC 9(3)V99  VALUE 21.10.
           05  FILLER    PIC 9(3)V99  VALUE 22.10.
           05  FILLER    PIC X(25)    VALUE 'DISNEY WORLD             '.
           05  FILLER    PIC 9(3)V99  VALUE 23.10.
           05  FILLER    PIC 9(3)V99  VALUE 24.10.
           05  FILLER    PIC 9(3)V99  VALUE 25.10.
           05  FILLER    PIC X(25)    VALUE 'ALIEN PLANET             '.
           05  FILLER    PIC 9(3)V99  VALUE 26.10.
           05  FILLER    PIC 9(3)V99  VALUE 27.10.
           05  FILLER    PIC 9(3)V99  VALUE 28.10.
           05  FILLER    PIC X(25)    VALUE 'SIX FLAGS                '.
           05  FILLER    PIC 9(3)V99  VALUE 29.10.
           05  FILLER    PIC 9(3)V99  VALUE 30.10.
           05  FILLER    PIC 9(3)V99  VALUE 31.10.
           05  FILLER    PIC X(25)    VALUE 'WHITE MOUNTAINS          '.
           05  FILLER    PIC 9(3)V99  VALUE 32.10.
           05  FILLER    PIC 9(3)V99  VALUE 33.10.
           05  FILLER    PIC 9(3)V99  VALUE 34.10.
           05  FILLER    PIC X(25)    VALUE 'SPLASH ZONE              '.
           05  FILLER    PIC 9(3)V99  VALUE 35.10.
           05  FILLER    PIC 9(3)V99  VALUE 36.10.
           05  FILLER    PIC 9(3)V99  VALUE 37.10.
           05  FILLER    PIC X(25)    VALUE 'AMUSE-A-ME               '.
           05  FILLER    PIC 9(3)V99  VALUE 38.10.
           05  FILLER    PIC 9(3)V99  VALUE 39.10.
           05  FILLER    PIC 9(3)V99  VALUE 40.10.
           05  FILLER    PIC X(25)    VALUE 'BUILD-A-BEER             '.
           05  FILLER    PIC 9(3)V99  VALUE 41.10.
           05  FILLER    PIC 9(3)V99  VALUE 42.10.
           05  FILLER    PIC 9(3)V99  VALUE 43.10.
           05  FILLER    PIC X(25)    VALUE 'SKY ROCKETS              '.
           05  FILLER    PIC 9(3)V99  VALUE 44.10.
           05  FILLER    PIC 9(3)V99  VALUE 45.10.
           05  FILLER    PIC 9(3)V99  VALUE 46.10. 
           05  FILLER    PIC X(25)    VALUE 'ANIMAL PLANET            '.
           05  FILLER    PIC 9(3)V99  VALUE 47.10.
           05  FILLER    PIC 9(3)V99  VALUE 48.10.
           05  FILLER    PIC 9(3)V99  VALUE 49.10.
           05  FILLER    PIC X(25)    VALUE 'SURVIVE THE JUNGLE       '.
           05  FILLER    PIC 9(3)V99  VALUE 50.10.
           05  FILLER    PIC 9(3)V99  VALUE 51.10.
           05  FILLER    PIC 9(3)V99  VALUE 52.10.
           05  FILLER    PIC X(25)    VALUE 'JOURNEY TO THE END OF NIM'.
           05  FILLER    PIC 9(3)V99  VALUE 53.10.
           05  FILLER    PIC 9(3)V99  VALUE 54.10.
           05  FILLER    PIC 9(3)V99  VALUE 55.10.
       01  AMUSEMENT-PARKS   REDEFINES  AMUSEMENT-ARRAY.
           05  PARK-PRICES-ARRAY   OCCURS 15 TIMES.
               10  AMUSE-PARK     PIC X(25).
               10  AMUSE-PRICE    PIC 9(3)V99   OCCURS  3 TIMES.



       01  SYS-DATE.
           05  I-YEAR       PIC 9(4).
           05  I-MONTH      PIC 99.
           05  I-DAY        PIC 99.



       01  COMPANY-TITLE-LINE.
           05  FILLER       PIC X(6)    VALUE 'DATE:'.
           05  O-DATE       PIC X(10).
           05  FILLER       PIC X(44)   VALUE SPACES.
           05  FILLER       PIC X(22)   VALUE 'CP2C - AMUSEMENT PARKS'.
           05  FILLER       PIC X(42)   VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR       PIC Z9.

       01  DETAIL-HEADING-LINE.
           05  FILLER       PIC X(8)    VALUE 'COBANL2C'.
           05  FILLER       PIC X(55)   VALUE SPACES.
           05  FILLER       PIC X(19)   VALUE 'SUBTOTAL SALES REPORT'.
           05  FILLER       PIC X(42)   VALUE SPACES.

       01  AUTHOR-HEADING-LINE.
           05  FILLER       PIC X(62)   VALUE SPACES.
           05  FILLER       PIC X(16)   VALUE 'ASHLEY LINDQUIST'.
           05  FILLER       PIC X(54)   VALUE SPACES.

       01  DETAIL-COLUMN-HEADING-1.
           05  FILLER       PIC X(14)   VALUE 'AMUSEMENT PARK'.
           05  FILLER       PIC X(13)   VALUE SPACES.
           05  FILLER       PIC X(10)   VALUE 'ATTENDANCE'.
           05  FILLER       PIC X(2)    VALUE SPACES.
           05  FILLER       PIC X(5)    VALUE 'ADULT'.
           05  FILLER       PIC X(6)    VALUE SPACES.
           05  FILLER       PIC X(5)    VALUE 'ADULT'.
           05  FILLER       PIC X(4)    VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'JUNIOR'.
           05  FILLER       PIC X(3)    VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'JUNIOR'.
           05  FILLER       PIC X(4)    VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'SENIOR'.
           05  FILLER       PIC X(3)    VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'SENIOR'.
           05  FILLER       PIC X(4)    VALUE SPACES.
           05  FILLER       PIC X(8)    VALUE 'SUBTOTAL'.
           05  FILLER       PIC X(3)    VALUE SPACES.
           05  FILLER       PIC X(8)    VALUE 'DISCOUNT'.
           05  FILLER       PIC X(11)   VALUE SPACES.
           05  FILLER       PIC X(5)    VALUE 'TOTAL'.

       01  DETAIL-COLUMN-HEADING-2.
           05  FILLER       PIC X(33)   VALUE SPACES.
           05  FILLER       PIC X(4)    VALUE 'DATE'.
           05  FILLER       PIC X(2)    VALUE SPACES.
           05  FILLER       PIC X(7)    VALUE 'TICKETS'.
           05  FILLER       PIC X(5)    VALUE SPACES.
           05  FILLER       PIC X(4)    VALUE 'COST'.
           05  FILLER       PIC X(3)    VALUE SPACES.
           05  FILLER       PIC X(7)    VALUE 'TICKETS'.
           05  FILLER       PIC X(5)    VALUE SPACES.
           05  FILLER       PIC X(4)    VALUE 'COST'.
           05  FILLER       PIC X(5)    VALUE SPACES.
           05  FILLER       PIC X(7)    VALUE 'TICKETS'.
           05  FILLER       PIC X(6)    VALUE SPACES.
           05  FILLER       PIC X(4)    VALUE 'COST'.
           05  FILLER       PIC X(35)   VALUE SPACES.
           05  FILLER       PIC X(4)    VALUE 'COST'.

       01  DETAIL-LINE.
           05  O-AMUSE-PARK        PIC X(25).
           05  FILLER              PIC XX      VALUE SPACES.
           05  O-ATTEND-DATE       PIC X(10).
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  O-ADULT-T           PIC Z9.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  O-ADULT-COST        PIC $$$.99.
           05  FILLER              PIC X(8)    VALUE SPACES.
           05  O-JR-T              PIC Z9.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  O-JR-COST           PIC $$$.99.
           05  FILLER              PIC X(8)    VALUE SPACES.
           05  O-SENIOR-T          PIC Z9.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  O-SENIOR-COST       PIC $$$.99.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  O-SUBTOTAL          PIC $$,$$$.99.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  O-DISCOUNT          PIC $$$.99.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  O-TOTAL-COST        PIC $$,$$$,$$$.99.

       01  SUB-LINE.
           05  FILLER             PIC X(4)    VALUE '--- '.
           05  FILLER             PIC X(17)   VALUE 'ATTENDANCE MONTH:'.
           05  O-ATTEND-MONTH     PIC Z9.
           05  FILLER             PIC X(3)    VALUE SPACES.
           05  FILLER             PIC X(13)   VALUE 'TOTAL SALES: '.
           05  O-AMUSE-SUB        PIC $$$$,$$$,$$$.99.
           05  FILLER             PIC X(89)   VALUE SPACES.


       01  GT-LINE.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE 'GRAND TOTAL: '.
           05  O-GT                PIC $$$$,$$$,$$$.99.
           05  FILLER              PIC X(101)  VALUE SPACES.

       01  BLANK-LINE.
           05  FILLER              PIC X(132)  VALUE SPACES.



       PROCEDURE DIVISION.

       0000-COBANL2C.

           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.



       1000-INIT.

           MOVE FUNCTION CURRENT-DATE TO SYS-DATE.

           STRING I-MONTH '/' I-DAY '/' I-YEAR DELIMITED BY SIZE 
             INTO O-DATE.
      

           OPEN INPUT PARK-MASTER.
           OPEN OUTPUT PARK-PRTOUT.
 


           PERFORM 9000-READ.

           MOVE I-ATTEND-DATE TO PACKED-ATTEND-DATE.
           MOVE ATTEND-MONTH TO MONTH-HOLD.

           PERFORM 9900-DETAIL-HEADING.



       2000-MAINLINE.

           IF MONTH-HOLD NOT EQUAL TO ATTEND-MONTH
               PERFORM 2100-SUBTOTAL
           END-IF.

           PERFORM 2200-CALCS.
           PERFORM 2300-DETAIL-OUTPUT.
           PERFORM 9000-READ.

           MOVE I-ATTEND-DATE TO PACKED-ATTEND-DATE.
           MOVE ATTEND-MONTH TO MONTH-HOLD.



            
       2100-SUBTOTAL.

           MOVE MONTH-HOLD TO O-ATTEND-MONTH.

           MOVE C-AMUSE-SUB TO O-AMUSE-SUB.

           WRITE PRTLINE
               FROM SUB-LINE
                   AFTER ADVANCING 2 LINES.

           MOVE ZERO TO C-AMUSE-SUB.

      *    // -- Starts Out Fresh With Zero Park Subtotal -- //




       2200-CALCS.

           COMPUTE C-SUBTOTAL = I-ADULT-TICKETS * AMUSE-PRICE(I-PARK 1)
             + I-JR-TICKETS * AMUSE-PRICE(I-PARK 2) 
             + I-SENIOR-TICKETS * AMUSE-PRICE(I-PARK 3).

           IF I-DISCOUNT-CODE = SPACES 
               MOVE 1 TO I-DISCOUNT.
      *    -----------------------------------------
           IF I-DISCOUNT-CODE = 'A'
               MOVE .15 TO I-DISCOUNT.
           IF I-DISCOUNT-CODE = 'G'
               MOVE .10 TO I-DISCOUNT.
           IF I-DISCOUNT-CODE = 'W'
               MOVE .08 TO I-DISCOUNT.
           IF I-DISCOUNT-CODE = 'R'
               MOVE .22 TO I-DISCOUNT.
      
           COMPUTE C-DISCOUNT = C-SUBTOTAL * I-DISCOUNT.
      
           COMPUTE C-TOTAL-COST = C-SUBTOTAL - C-DISCOUNT.
   
           ADD C-TOTAL-COST TO C-AMUSE-SUB.

           ADD C-TOTAL-COST TO C-GT.



       2300-DETAIL-OUTPUT.

           STRING ATTEND-MONTH '/' ATTEND-DAY '/' ATTEND-YEAR DELIMITED 
               BY SIZE INTO O-ATTEND-DATE.

           MOVE AMUSE-PARK(I-PARK) TO O-AMUSE-PARK of DETAIL-LINE.

           MOVE I-ADULT-TICKETS TO O-ADULT-T.
           MOVE AMUSE-PRICE(I-PARK 1) TO O-ADULT-COST.
           MOVE I-JR-TICKETS TO O-JR-T.
           MOVE AMUSE-PRICE(I-PARK 2) TO O-JR-COST.
           MOVE I-SENIOR-TICKETS TO O-SENIOR-T.
           MOVE AMUSE-PRICE(I-PARK 3) TO O-SENIOR-COST.

           MOVE C-SUBTOTAL TO O-SUBTOTAL.
           MOVE C-DISCOUNT TO O-DISCOUNT.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.

           MOVE ZEROS TO C-SUBTOTAL.
           MOVE ZEROS TO C-DISCOUNT.
           MOVE ZEROS TO C-TOTAL-COST.

           WRITE PRTLINE 
               FROM DETAIL-LINE
                   AFTER ADVANCING 1 LINE.

                                      

       3000-CLOSING.

           MOVE C-GT TO O-GT

           WRITE PRTLINE
               FROM GT-LINE
                   AFTER ADVANCING 3 LINES.


           CLOSE PARK-MASTER.
           CLOSE PARK-PRTOUT.




       9000-READ.

           READ PARK-MASTER
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
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM DETAIL-COLUMN-HEADING-1
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM DETAIL-COLUMN-HEADING-2
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.