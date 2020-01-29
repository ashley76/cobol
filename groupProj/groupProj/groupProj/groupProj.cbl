       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GROUPPROJ
       AUTHOR.                 Ashley Lindquist
       DATE-WRITTEN.           8-4-19
       DATE-COMPILED.


      ***********************************************
      *  - GROUP PROJECT - FARM ANIMALS -           *
      * Program utilizes simple structure.          *
      * COBOL 3 - SUMMER 2019                       *
      ***********************************************



       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT FARM-MASTER
           ASSIGN TO 'C:\USERS\ASHLEY NICOLE\DESKTOP\COBOL 3\FARM.DAT'
      *    ASSIGN TO INFILE
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FARM-PRTOUT
           ASSIGN TO 'C:\USERS\ASHLEY NICOLE\DESKTOP\COBOL 3\PRTOUT'.
      *    ASSIGN TO OUTPRT.

           SELECT FARM-DUMMY
           ASSIGN TO DMYOUT.



       DATA DIVISION.
       FILE SECTION.

       FD  FARM-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS DETAIL-REC
           RECORD CONTAINS 13 CHARACTERS.


       01  DETAIL-REC.
           05  FARM-ANIMAL       PIC X(11).
           05  QUANTITY          PIC 99.

     

       FD FARM-PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE.

       01 PRTLINE              PIC X(132).

       FD FARM-DUMMY
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE.

       01 DMYLINE              PIC X(132).



       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR          PIC 99        VALUE ZERO.
           05  MORE-RECS       PIC XXX       VALUE 'YES'.
          


       01  SYS-DATE.
           05  I-YEAR       PIC 9(4).
           05  I-MONTH      PIC 99.
           05  I-DAY        PIC 99.



       01  COMPANY-TITLE-LINE.
           05  FILLER       PIC X(6)    VALUE 'DATE:'.
           05  O-DATE       PIC X(10).
           05  FILLER       PIC X(44)   VALUE SPACES.
           05  FILLER       PIC X(22)   VALUE 'FARM ANIMALS'.
           05  FILLER       PIC X(42)   VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR       PIC Z9.

       01  DETAIL-HEADING-LINE.
           05  FILLER       PIC X(13)   VALUE 'GROUP-PROJECT'.
           05  FILLER       PIC X(47)   VALUE SPACES.
           05  FILLER       PIC X(13)   VALUE 'DETAIL REPORT'.
           05  FILLER       PIC X(40)   VALUE SPACES.

       01  AUTHOR-HEADING-LINE.
           05  FILLER       PIC X(43)   VALUE SPACES.
           05  FILLER       PIC X(49)   VALUE 'ASHLEY LINDQUIST, SUSSANA
      -    ' KWABI, DEVEN WOUDENBERG'.
           05  FILLER       PIC X(40)   VALUE SPACES.

       01  DETAIL-COLUMN-HEADING.
           05  FILLER       PIC X(10)   VALUE SPACES.
           05  FILLER       PIC X(11)   VALUE 'FARM ANIMAL'.
           05  FILLER       PIC X(5)    VALUE SPACES.
           05  FILLER       PIC X(8)    VALUE 'QUANTITY'.
           05  FILLER       PIC X(108)  VALUE SPACES.

       01  DETAIL-LINE.
           05  FILLER              PIC X(10)     VALUE SPACES.
           05  O-FARM-ANIMAL       PIC X(11).
           05  FILLER              PIC X(8)      VALUE SPACES.
           05  O-QUANTITY          PIC Z9.
           05  FILLER              PIC X(108)    VALUE SPACES.


       01  BLANK-LINE.
           05  FILLER              PIC X(132)    VALUE SPACES.



       PROCEDURE DIVISION.

       0000-GROUPPROJ.

           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.



       1000-INIT.

           MOVE FUNCTION CURRENT-DATE TO SYS-DATE.

           STRING I-MONTH '/' I-DAY '/' I-YEAR DELIMITED BY SIZE 
             INTO O-DATE.
      

           OPEN INPUT FARM-MASTER.
           OPEN OUTPUT FARM-PRTOUT.

           PERFORM 9000-READ.

           PERFORM 9900-DETAIL-HEADING.



       2000-MAINLINE.

           PERFORM 2100-DETAIL-OUTPUT.
           PERFORM 9000-READ.


            
       2100-DETAIL-OUTPUT.

           MOVE FARM-ANIMAL TO O-FARM-ANIMAL.
           MOVE QUANTITY TO O-QUANTITY.

           WRITE PRTLINE 
               FROM DETAIL-LINE 
                   AFTER ADVANCING 1 LINE.
    
                                      

       3000-CLOSING.

           CLOSE FARM-MASTER.
           CLOSE FARM-PRTOUT.



       9000-READ.

           READ FARM-MASTER
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
               FROM DETAIL-COLUMN-HEADING
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.