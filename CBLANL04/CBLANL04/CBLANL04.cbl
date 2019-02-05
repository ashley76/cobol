       IDENTIFICATION DIVISION.
	   PROGRAM-ID.			CBLANL04
	   AUTHOR.				ASHLEY LINDQUIST
       DATE-WRITTEN.		1/20/19
	   DATE-COMPILED.		1/22/19
		

      ***************************************************************
      * THIS PROGRAM IS CASE PROBLEM #4.                            *
      * PREFORM ACCURATE DECISIONS.                                 *
      * RENT REPORT WILL BE GENERATED.                              *
      ***************************************************************
		
		
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

       SELECT RENT-MASTER
			    ASSIGN TO "C:\COBOL\PROJECTS\CBLANL04\MONBILLS.DAT"
				ORGANIZATION IS LINE SEQUENTIAL.
				
	   SELECT PRTOUT
			    ASSIGN TO "C:\COBOL\PROJECTS\RENT.PRT"
				ORGANIZATION IS RECORD SEQUENTIAL.
				
	   DATA DIVISION.
	   FILE SECTION.
		    
	   FD RENT-MASTER
	      LABEL RECORD IS STANDARD
		  RECORD CONTAINS 24 CHARACTERS
	      DATA RECORD IS I-REC.
			
	   01  I-REC.
		   05  I-BLD-CODE        PIC XX.
           05  I-UNIT            PIC 99.
           05  I-TENENTS         PIC 9.
           05  I-ELECTRIC        PIC 999V99.
           05  I-GAS             PIC 999V99.
           05  I-WATER           PIC 999V99.
           05  I-GARBAGE         PIC 99V99.

			
	   FD PRTOUT
		    LABEL RECORD IS OMITTED
		    RECORD CONTAINS 132 CHARACTERS
		    LINAGE IS 60 WITH FOOTING AT 55
		    DATA RECORD IS PRTLINE.

	   01 PRTLINE			       PIC X(132).
       
	   WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  BASE-RENT           PIC 9(7)V99     VALUE ZERO.
           05  TENANT-CHRG         PIC 9(7)V99     VALUE ZERO.
           05  C-SUBTOTAL          PIC 9(7)V99     VALUE ZERO.
           05  C-PREM-DIS          PIC S9(7)V99    VALUE ZERO.
           05  C-TOTAL-UTIL        PIC 9(7)V99     VALUE ZERO.
           05  C-RENT-DUE          PIC 9(7)V99     VALUE ZERO.
           05  C-GT-BASE-RENT      PIC 9(7)V99     VALUE ZERO.
           05  C-GT-TENANT-CHRG    PIC 9(10)V99    VALUE ZERO.
           05  C-GT-PREM-DIS       PIC 9(10)V99    VALUE ZERO.
           05  C-GT-SUBTOTAL       PIC 9(7)V99     VALUE ZERO.
           05  C-GT-TOTAL-UTIL     PIC 9(10)V99    VALUE ZERO.
           05  C-GT-RENT-DUE       PIC 9(10)V99    VALUE ZERO.

           05  C-GT-PREM-CTR       PIC 999         VALUE ZERO.
           05  C-GT-DIS-CTR        PIC 999         VALUE ZERO.
           05  C-PCTR              PIC 99          VALUE ZERO.
           05  MORE-RECS           PIC XXX         VALUE "YES".
		
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
	       05  FILLER   		PIC X(38) 	VALUE SPACES.
	       05  FILLER			PIC X(33)	VALUE "LINDQUIST'S RENTALS".
	       05  FILLER			PIC X(51)	VALUE SPACES.
	       05  FILLER			PIC X(6)	VALUE "PAGE:".
           05  O-PCTR			PIC Z9.

	   01  COMPANY-TITLE-2.
	       05  FILLER			PIC X(8)	VALUE "CBLANL04".
	       05  FILLER			PIC X(45)	VALUE SPACES.
	       05  FILLER			PIC X(16)	VALUE "BILLABLE RENT - ".
	       05  SYSTEM-MONTH	    PIC X(9).
	       05  FILLER   		PIC X(53) 	VALUE SPACES.

	   01  COLUMN-HEADING-1.
           05  FILLER           PIC X(23)   VALUE SPACES.
           05  FILLER           PIC X(4)    VALUE 'BASE'.
           05  FILLER           PIC X(2)    VALUE SPACES.
           05  FILLER           PIC X(6)    VALUE 'TENANT'.
           05  FILLER           PIC X(2)    VALUE SPACES.
           05  FILLER           PIC X(6)    VALUE 'TENANT'.
           05  FILLER           PIC X(5)    VALUE SPACES.
           05  FILLER           PIC X(8)    VALUE 'PREMIUM/'.
           05  FILLER           PIC X(75)   VALUE SPACES.
         
	   01  COLUMN-HEADING-2.
	       05  FILLER  	 	   PIC X(23)   VALUE 'RENTAL BUILDING UNIT'.
           05  FILLER          PIC X(4)    VALUE 'RENT'.
           05  FILLER          PIC X(2)    VALUE SPACES.
           05  FILLER          PIC X(6)    VALUE 'NUMBER'.
           05  FILLER          PIC X(2)    VALUE SPACES.
           05  FILLER          PIC X(6)    VALUE 'CHARGE'.
           05  FILLER          PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'DISCOUNT'.
           05  FILLER          PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'SUBTOTAL'.
           05  FILLER          PIC X(2)    VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'ELECTRIC'.
           05  FILLER          PIC X(4)    VALUE SPACES.
           05  FILLER          PIC X(3)    VALUE 'GAS'.
           05  FILLER          PIC X(4)    VALUE SPACES.
           05  FILLER          PIC X(5)    VALUE 'WATER'.
           05  FILLER          PIC X(2)    VALUE SPACES.
           05  FILLER          PIC X(7)    VALUE 'GARBAGE'.
           05  FILLER          PIC X(3)    VALUE SPACES.
           05  FILLER          PIC X(9)    VALUE 'UTILITIES'.
           05  FILLER          PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'RENT DUE'.

       01  DETAIL-LINE.
           05  O-BLD               PIC X(15).
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-UNIT              PIC Z9.
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-BASE-RENT         PIC $$$$.99.
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  O-TENANT-NUM        PIC 9.
           05  FILLER              PIC X(4)        VALUE SPACES.
           05  O-TENANT-CHRG       PIC $$$$.99.
           05  FILLER              PIC X(4)        VALUE SPACES.
           05  O-PREM-DIS          PIC $$,$$$.99+.
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  O-SUBTOTAL          PIC $$,$$$.99.
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-ELECTRIC          PIC $$$$.99.
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-GAS               PIC $$$$.99.
           05  FILLER              PIC X(2)       VALUE SPACES.
           05  O-WATER             PIC $$$$.99.
           05  FILLER              PIC X(2)        VALUE SPACES.
           05  O-GARBAGE           PIC $$$.99.
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  O-TOTAL-UTIL        PIC $$,$$$.99.
           05  FILLER              PIC X(4)        VALUE SPACES.
           05  O-RENT-DUE          PIC $$,$$$.99.
           05  FLAG                PIC XXX.

       01  DISCOUNT-LINE.
           05  FILLER              PIC X(34)     VALUE SPACES.
           05  FILLER              PIC X(8)      VALUE 'RENTALS '.
           05  FILLER              PIC X(11)     VALUE 'DISCOUNTED '.
           05  O-GT-DISCOUNT       PIC ZZ9.
           05  FILLER              PIC X(75)     VALUE SPACES.

       01  PREMIUM-LINE.
           05  FILLER              PIC X(37)     VALUE SPACES.
           05  FILLER              PIC X(8)      VALUE 'PREMIUM'.
           05  FILLER              PIC X(8)      VALUE 'RENTALS '.
           05  O-GT-PREMIUM        PIC ZZ9.
           05  FILLER              PIC X(75)     VALUE SPACES.    
       
       01  GRANDTOTAL-LINE.
           05  FILLER              PIC X(18)     VALUE 'GRAND TOTALS: '.
           05  O-GT-BASE-RENT      PIC $$$,$$$.99.
           05  FILLER              PIC X(5)      VALUE SPACES.
           05  O-GT-TENANT-CHRG    PIC $$$,$$$.99.
           05  FILLER              PIC X(2)      VALUE SPACES.
           05  O-GT-PREM-DIS       PIC $$$$,$$$.99+.
           05  FILLER              PIC X(1)      VALUE SPACES.
           05  O-GT-SUBTOTAL       PIC $$$$,$$$.99.
           05  FILLER              PIC X(36)      VALUE SPACES.
           05  O-GT-TOTAL-UTIL     PIC $$$$,$$$.99.
           05  FILLER              PIC X(2)      VALUE SPACES.
           05  O-GT-RENT-DUE       PIC $$$$,$$$.99.


       PROCEDURE DIVISION.

       0000-CBLANL04.
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

           EVALUATE I-MONTH
               WHEN 01
                   MOVE 'JANUARY' TO SYSTEM-MONTH
               WHEN 02
                   MOVE 'FEBUARY' TO SYSTEM-MONTH
               WHEN 03
                   MOVE 'MARCH' TO SYSTEM-MONTH
               WHEN 04
                   MOVE 'APRIL' TO SYSTEM-MONTH
               WHEN 05
                   MOVE 'MAY' TO SYSTEM-MONTH
               WHEN 06
                   MOVE 'JUNE' TO SYSTEM-MONTH
               WHEN 07
                   MOVE 'JULY' TO SYSTEM-MONTH
               WHEN 08
                   MOVE 'AUGUST' TO SYSTEM-MONTH
               WHEN 09
                   MOVE 'SEPTEMBER' TO SYSTEM-MONTH
               WHEN 10
                   MOVE 'OCTOBER' TO SYSTEM-MONTH
               WHEN 11
                   MOVE 'NOVEMBER' TO SYSTEM-MONTH
               WHEN 12
                   MOVE 'DECEMBER' TO SYSTEM-MONTH
           END-EVALUATE.


           OPEN INPUT RENT-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM 9000-READ.
           PERFORM 9900-HEADING.


       2000-MAINLINE.

           PERFORM 2100-EVALUATE.
           PERFORM 2200-CALCS.
           PERFORM 2300-OUTPUT.
           PERFORM 9000-READ.

           
       2100-EVALUATE.

           EVALUATE I-UNIT
               WHEN 1 THRU 8
                   MOVE 650.00 TO BASE-RENT
                   EVALUATE I-TENENTS
                       WHEN 1
                           MOVE 0.00 TO TENANT-CHRG
                       WHEN 2
                           MOVE 25.00 TO TENANT-CHRG
                       WHEN 3
                           MOVE 50.00 TO TENANT-CHRG
                       WHEN 4
                           MOVE 75.00 TO TENANT-CHRG
                       WHEN > 4
                           MOVE 83.45 TO TENANT-CHRG
                   END-EVALUATE
               WHEN 9 THRU 16
                   MOVE 700.00 TO BASE-RENT
                   EVALUATE I-TENENTS
                       WHEN 1
                           MOVE 0.00 TO TENANT-CHRG
                       WHEN 2
                           MOVE 35.55 TO TENANT-CHRG
                       WHEN 3
                           MOVE 71.10 TO TENANT-CHRG
                       WHEN 4
                           MOVE 106.65 TO TENANT-CHRG
                       WHEN > 4
                           MOVE 135.00 TO TENANT-CHRG
                   END-EVALUATE
               WHEN 17 THRU 25
                   MOVE 825.00 TO BASE-RENT
                   EVALUATE I-TENENTS
                       WHEN 1
                           MOVE 0.00 TO TENANT-CHRG
                       WHEN 2
                           MOVE 50.00 TO TENANT-CHRG
                       WHEN 3
                           MOVE 100.00 TO TENANT-CHRG
                       WHEN 4 
                           MOVE 150.00 TO TENANT-CHRG
                       WHEN > 4
                           MOVE 185.60 TO TENANT-CHRG
                   END-EVALUATE
           END-EVALUATE.


           EVALUATE I-BLD-CODE
               WHEN 'AA'
                   MOVE 'PALACE PLACE' TO O-BLD
               WHEN 'GG'
                   MOVE 'GEORGIA' TO O-BLD
               WHEN 'PP'
                   MOVE 'PARK PLACE' TO O-BLD
                   IF I-UNIT = 23 OR 25
                       COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG)
                       * .12
                       COMPUTE C-GT-PREM-CTR = C-GT-PREM-CTR + 1
                   END-IF
               WHEN 'IA'
                   MOVE 'IOWA CONDO' TO O-BLD
                   IF SYSTEM-MONTH = 'JANUARY' OR 'DECEMBER'
                       COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG)  
                       *-.5
                   END-IF
               WHEN 'MS'
                   MOVE 'MARKET STREET' TO O-BLD
               WHEN 'HH'
                   MOVE 'HIGH TOWER' TO O-BLD
               WHEN 'R7'
                   MOVE 'UPTOWN CONDOS' TO O-BLD
                   IF I-UNIT = 23 OR 25
                       COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG)
                       * .12
                       COMPUTE C-GT-PREM-CTR = C-GT-PREM-CTR + 1
                   END-IF
               WHEN 'GM'
                   MOVE 'GANDER MOUNTAIN' TO O-BLD
               WHEN 'BP'
                   MOVE 'BENTON PLACE' TO O-BLD
                   COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG) * -.33
                   COMPUTE C-GT-DIS-CTR = C-GT-DIS-CTR + 1
               WHEN 'GA'
                   MOVE 'GRAND AVENUE' TO O-BLD
               WHEN 'JK'
                   MOVE "JACK'S PLACE" TO O-BLD
                   IF SYSTEM-MONTH = 'JANUARY' OR 'DECEMBER'
                       COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG)  
                       * -.5
                       COMPUTE C-GT-DIS-CTR = C-GT-DIS-CTR + 1
                   END-IF
               WHEN 'UN'
                   MOVE 'UNDERGROUND SAM' TO O-BLD
               WHEN 'YD'
                   MOVE 'YANKEE DOODLE' TO O-BLD
               WHEN 'YT'
                   MOVE 'YAHTEE AVE' TO O-BLD
                   IF I-UNIT = 23 OR 25
                      COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG)
                      * .12
                      COMPUTE C-GT-PREM-CTR = C-GT-PREM-CTR + 1
                   END-IF
               WHEN 'CP'
                   MOVE 'COURT PLACE' TO O-BLD
               WHEN 'NZ'
                   MOVE 'NEW ZOO' TO O-BLD
               WHEN 'VV'
                   MOVE 'VERMONT' TO O-BLD
               WHEN 'CT'
                   MOVE 'CHINA TOWN' TO O-BLD
                   COMPUTE C-PREM-DIS = (BASE-RENT + TENANT-CHRG) * -.33
                   COMPUTE C-GT-DIS-CTR = C-GT-DIS-CTR + 1
               WHEN 'YS'
                   MOVE 'YORKSHIRE' TO O-BLD
               WHEN 'ME'
                   MOVE 'MAINE APT' TO O-BLD
           END-EVALUATE.

       2200-CALCS.
      
           COMPUTE C-SUBTOTAL = BASE-RENT + TENANT-CHRG + C-PREM-DIS.
           COMPUTE C-TOTAL-UTIL = I-ELECTRIC + I-GAS + I-WATER + 
                                                        I-GARBAGE.
           COMPUTE C-RENT-DUE = C-SUBTOTAL + C-TOTAL-UTIL.

           IF C-RENT-DUE > 1000
               MOVE '***' TO FLAG
           END-IF.

           COMPUTE C-GT-BASE-RENT = C-GT-BASE-RENT + BASE-RENT.
           COMPUTE C-GT-TENANT-CHRG = C-GT-TENANT-CHRG + TENANT-CHRG.
           COMPUTE C-GT-PREM-DIS = C-GT-PREM-DIS + C-PREM-DIS.
           COMPUTE C-GT-SUBTOTAL = C-GT-SUBTOTAL + C-SUBTOTAL.
           COMPUTE C-GT-TOTAL-UTIL = C-GT-TOTAL-UTIL + C-TOTAL-UTIL.
           COMPUTE C-GT-RENT-DUE = C-GT-RENT-DUE + C-RENT-DUE.


       2300-OUTPUT.
       
           MOVE I-UNIT TO O-UNIT.
           MOVE BASE-RENT TO O-BASE-RENT.
           MOVE I-TENENTS TO O-TENANT-NUM.
           MOVE TENANT-CHRG TO O-TENANT-CHRG.
           MOVE C-PREM-DIS TO O-PREM-DIS.
           MOVE C-SUBTOTAL TO O-SUBTOTAL. 
           MOVE I-ELECTRIC TO O-ELECTRIC.
           MOVE I-GAS TO O-GAS.
           MOVE I-WATER TO O-WATER.
           MOVE I-GARBAGE TO O-GARBAGE.

           MOVE C-TOTAL-UTIL TO O-TOTAL-UTIL.
           MOVE C-RENT-DUE TO O-RENT-DUE.
          
           WRITE PRTLINE
               FROM DETAIL-LINE
                   AFTER ADVANCING 2 LINES
                       AT EOP
                           PERFORM 9900-HEADING.

           MOVE 0 TO BASE-RENT.
           MOVE 0 TO TENANT-CHRG.
           MOVE 0 TO C-PREM-DIS.
           MOVE 0 TO C-SUBTOTAL.
           MOVE 0 TO C-TOTAL-UTIL.
           MOVE 0 TO C-RENT-DUE.

          
       3000-CLOSING.

           PERFORM 3100-GRAND-TOTAL.

           CLOSE RENT-MASTER.
           CLOSE PRTOUT.


       3100-GRAND-TOTAL.

           MOVE C-GT-BASE-RENT TO O-GT-BASE-RENT.
           MOVE C-GT-TENANT-CHRG TO O-GT-TENANT-CHRG.
           MOVE C-GT-PREM-DIS TO O-GT-PREM-DIS.
           MOVE C-GT-SUBTOTAL TO O-GT-SUBTOTAL.
           MOVE C-GT-TOTAL-UTIL TO O-GT-TOTAL-UTIL.
           MOVE C-GT-RENT-DUE TO O-GT-RENT-DUE.

           MOVE C-GT-PREM-CTR TO O-GT-PREMIUM.
           MOVE C-GT-DIS-CTR TO O-GT-DISCOUNT.

           WRITE PRTLINE
               FROM GRANDTOTAL-LINE
                   AFTER ADVANCING 3 LINES.
           WRITE PRTLINE   
               FROM DISCOUNT-LINE
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE   
               FROM PREMIUM-LINE
                   AFTER ADVANCING 1 LINE.


       9000-READ.

           READ RENT-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.
          

       9900-HEADING.

           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE
               FROM COMPANY-TITLE-2
                   AFTER ADVANCING 1 LINES.
           WRITE PRTLINE
               FROM COLUMN-HEADING-1
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
               FROM COLUMN-HEADING-2
                   AFTER ADVANCING 1 LINE.