       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GROUPERR
       AUTHOR.                 Ashley Lindquist
       DATE-WRITTEN.           8-5-19
       DATE-COMPILED.


      ***********************************************
      *  - GROUP PROJECT - ERROR FARM ANIMALS -     *
      * Program utilizes simple structure.          *
      * COBOL 3 - SUMMER 2019                       *
      ***********************************************



       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT FARM-ERROR
           ASSIGN TO 'C:\USERS\ASHLEY NICOLE\DESKTOP\COBOL 3\ERRPRT'.
      *    ASSIGN TO ERRPRT.


       DATA DIVISION.
       FILE SECTION.

       FD FARM-ERROR
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE.

       01  ERRLINE              PIC X(132).



       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR       PIC 99        VALUE ZERO.


       01  SYS-DATE.
           05  I-YEAR       PIC 9(4).
           05  I-MONTH      PIC 99.
           05  I-DAY        PIC 99.



       01  COMPANY-TITLE-LINE.
           05  FILLER       PIC X(6)    VALUE 'DATE:'.
           05  O-DATE       PIC X(10).
           05  FILLER       PIC X(44)   VALUE SPACES.
           05  FILLER       PIC X(11)   VALUE '-- ERROR --'.
           05  FILLER       PIC X(53)   VALUE SPACES.
           05  FILLER       PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR       PIC Z9.

       01  HEADING-LINE.
           05  FILLER       PIC X(13)   VALUE 'GROUP-PROJECT'.
           05  FILLER       PIC X(47)   VALUE SPACES.
           05  FILLER       PIC X(13)   VALUE 'ERROR REPORT'.
           05  FILLER       PIC X(40)   VALUE SPACES.

       01  AUTHOR-HEADING-LINE.
           05  FILLER       PIC X(43)   VALUE SPACES.
           05  FILLER       PIC X(49)   VALUE 'ASHLEY LINDQUIST, SUSSANA
      -    ' KWABI, DEVEN WOUDENBERG'.
           05  FILLER       PIC X(40)   VALUE SPACES.

       01  ERROR-LINE.
           05  FILLER       PIC X(10)   VALUE SPACES.
           05  FILLER       PIC X(37)   VALUE 
      -    '*** ERROR IN FARM ANIMAL PROGRAM! ***'.
           05  FILLER       PIC X(85)   VALUE SPACES.
                                                           


       01  BLANK-LINE.
           05  FILLER              PIC X(132)    VALUE SPACES.



       PROCEDURE DIVISION.

       0000-GROUPERR.

           PERFORM 1000-INIT.
           PERFORM 2000-CLOSING.
           STOP RUN.



       1000-INIT.

           MOVE FUNCTION CURRENT-DATE TO SYS-DATE.

           STRING I-MONTH '/' I-DAY '/' I-YEAR DELIMITED BY SIZE 
             INTO O-DATE.
      

           OPEN OUTPUT FARM-ERROR.

           PERFORM 9900-DETAIL-HEADING.


                                      

       2000-CLOSING.

           CLOSE FARM-ERROR.



       9900-DETAIL-HEADING.

           ADD 1 TO C-PCTR. 
           MOVE C-PCTR TO O-PCTR.

           WRITE ERRLINE
               FROM COMPANY-TITLE-LINE
                   AFTER ADVANCING PAGE.
           WRITE ERRLINE
               FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE ERRLINE   
               FROM AUTHOR-HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE ERRLINE 
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE ERRLINE
               FROM ERROR-LINE
                   AFTER ADVANCING 1 LINE.
           WRITE ERRLINE
               FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.