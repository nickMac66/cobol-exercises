      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
      
       PROGRAM-ID.   REPORT.
       AUTHOR.       NICK MACDONALD.
       DATE-WRITTEN. 2024-12-01.
      
      ******************************************************************
      *                                                                *
      *   PURPOSE ===> WRITE A CHRISTMAS WISH LIST REPORT              * 
      *                                                                *
      ******************************************************************     
      
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
      
      ******************************************************************
       CONFIGURATION SECTION.
      ******************************************************************
	  
       SPECIAL-NAMES.
     
      *** ASSIGN A NAME TO CHANNEL 1
           C01 IS TOP-PAGE.
      
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
	  
       FILE-CONTROL.      
           SELECT WISH-FILE      ASSIGN TO UT-S-WISH.
           SELECT REPORT-FILE    ASSIGN TO UT-S-REPORT.
      /

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      
       FILE SECTION.      
       FD  WISH-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS WISH-RECORD.
      *
       01  WISH-RECORD.
           05  FILLER                       PIC X(80).
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REPORT-RECORD.
      *
       01  REPORT-RECORD                    PIC X(132).
      /
      
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
	  
       01 WS-EYECATCHER                     PIC X(47) VALUE
           "*** BEGINNING OF WORKING-STORAGE ***".
      *
       01 WS-PROGRAM-INDICATORS.
          05 WS-MORE-RECORDS                PIC X.
             88 MORE-RECORDS                           VALUE 'Y'.
             88 NO-MORE-RECORDS                        VALUE 'N'.
      *
       01 WS-VARIABLES.
      *** DEFINE VARIABLES FOR LINE COUNT AND PAGE COUNT
          05 WS-LINE                        PIC S999     PACKED-DECIMAL.
          05 WS-PAGE                        PIC S999     PACKED-DECIMAL.
          05 WS-TOTAL                       PIC S9(9)V99 PACKED-DECIMAL.
          05 WS-DATE                        PIC 9(8).
          05 WS-DATE-X REDEFINES WS-DATE.
             10 WS-YYYY                     PIC 9(4).
             10 WS-MM                       PIC 99.
             10 WS-DD                       PIC 99.
      *
       01 WS-CONSTANTS.
      *** DEFINE A CONSTANT FOR THE NUMBER OF LINES PER PAGE
          05 WS-LINES-PER-PAGE              PIC S999     PACKED-DECIMAL
                                                         VALUE +10.
      *
       01 WS-WISH-RECORD.
          05 WISH-CLIENT-ID                 PIC X(5).
          05 WISH-CLIENT-NAME               PIC X(25).
          05 WISH-CLIENT-DOB                PIC X(10).
          05 WISH-ITEM-NUMBER               PIC 9(5).
          05 WISH-ITEM-COST                 PIC 9(7)V99.
          05 WISH-NAUGHTY-RATING            PIC XX.
          05 FILLER                         PIC X(24).
      *
      *** CODE RECORD DESCRIPTIONS FOR HEADING AND DETAIL LINES
       01 WS-HEADING-LINE-1.
          05 HEADING-YYYY                   PIC 9(4).
          05 FILLER                         PIC X     VALUE '-'.
          05 HEADING-MM                     PIC 99.
          05 FILLER                         PIC X     VALUE '-'.
          05 HEADING-DD                     PIC 99.
          05 FILLER                         PIC X(26) VALUE SPACES.
          05 FILLER                         PIC X(45) VALUE
             "CHRISTMAS WISH LIST".
          05 FILLER                         PIC X(6)  VALUE 'PAGE:'.
          05 HEADING-PAGE                   PIC ZZ9.
      *
       01 WS-HEADING-LINE-2.
          05 FILLER                         PIC X(40) VALUE
             " CLIENT  NAME".
          05 FILLER                         PIC X(50) VALUE
             "DOB     ITEM         COST     NAUGHTY RATING".
      *
       01 WS-DETAIL-LINE.
          05 FILLER                         PIC X.
          05 DETAIL-CLIENT-ID               PIC X(8).
          05 DETAIL-CLIENT-NAME             PIC X(27).
          05 DETAIL-CLIENT-DOB              PIC X(12).
          05 DETAIL-ITEM-NUMBER             PIC 9(5)B.
          05 DETAIL-ITEM-COST               PIC ZZZ,ZZZ,ZZ9.99BB.
          05 DETAIL-NAUGHTY-RATING          PIC X(20).
      /
      
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      
      ******************************************************************
       MAINLINE.
      ******************************************************************
	  
      *----------------------------------------------------------------*
      * PROGRAM MAINLINE                                               *
      *----------------------------------------------------------------*
      
           PERFORM A0000-INITIALIZATION
           PERFORM B0000-OPEN-FILES
           PERFORM C0000-READ-RECORD
      *
           PERFORM D0000-PROCESS-RECORDS
              UNTIL NO-MORE-RECORDS
      *
           PERFORM E0000-WRITE-TOTALS
           PERFORM F0000-CLOSE-FILES
      *
           STOP RUN
           .
      
      ******************************************************************
       A0000-INITIALIZATION.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * INITIALIZE VARIABLES USED IN THE PROGRAM                      *
      *---------------------------------------------------------------*
      
           SET  MORE-RECORDS      TO TRUE
      *** INITIALIZE LINE COUNT, PAGE COUNT, AND TOTAL VARIABLES
           MOVE ZERO              TO WS-PAGE,
                                     WS-TOTAL
           MOVE WS-LINES-PER-PAGE TO WS-LINE
      *
           ACCEPT WS-DATE         FROM DATE YYYYMMDD
      *
           MOVE WS-DATE(1:4)      TO HEADING-YYYY
           MOVE WS-DATE(5:2)      TO HEADING-DD
           MOVE WS-DATE(7:2)      TO HEADING-MM
           .
      
      ******************************************************************
       B0000-OPEN-FILES.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * OPEN FILES USED IN THE PROGRAM                                *
      *---------------------------------------------------------------*
      *
           OPEN INPUT  WISH-FILE
                OUTPUT REPORT-FILE
           .
      
      ******************************************************************
       C0000-READ-RECORD.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * READ A RECORD FROM THE INPUT FILE                             *
      *---------------------------------------------------------------*
      
           READ WISH-FILE INTO WS-WISH-RECORD
              AT END
                 SET NO-MORE-RECORDS TO TRUE
           END-READ
           .
      
      ******************************************************************
       D0000-PROCESS-RECORDS.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * PROCESS ONE RECORD, THEN READ THE NEXT ONE                    *
      *---------------------------------------------------------------*
      
           ADD WISH-ITEM-COST TO WS-TOTAL
      *
           PERFORM D1000-WRITE-DETAIL
           PERFORM C0000-READ-RECORD
           .
      
      ******************************************************************
       D1000-WRITE-DETAIL.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * WRITE A DETAIL RECORD TO THE REPORT                           *
      *---------------------------------------------------------------*
      
      ***  CALL THE HEADING ROUTINE IF LINES PER PAGE EXCEEDED
           IF WS-LINE >= WS-LINES-PER-PAGE
              PERFORM D1100-WRITE-HEADINGS
           END-IF
      *
      ***  BUILD AND PRINT THE DETAIL LINE
           ADD  1                     TO WS-LINE
           MOVE WISH-CLIENT-ID        TO DETAIL-CLIENT-ID
           MOVE WISH-CLIENT-NAME      TO DETAIL-CLIENT-NAME
           MOVE WISH-CLIENT-DOB       TO DETAIL-CLIENT-DOB
           MOVE WISH-ITEM-NUMBER      TO DETAIL-ITEM-NUMBER
           MOVE WISH-ITEM-COST        TO DETAIL-ITEM-COST
           MOVE WISH-NAUGHTY-RATING   TO DETAIL-NAUGHTY-RATING
      *
           WRITE REPORT-RECORD        FROM WS-DETAIL-LINE
              AFTER 1
           .
      
      ******************************************************************
       D1100-WRITE-HEADINGS.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * WRITE HEADING LINES ON THE REPORT                             *
      *---------------------------------------------------------------*
      
      *** CODE LOGIC TO HANDLE HEADINGS HERE
           ADD  1              TO WS-PAGE
           MOVE WS-PAGE        TO HEADING-PAGE
           MOVE ZERO           TO WS-LINE
           MOVE SPACES         TO WS-DETAIL-LINE
      *
           WRITE REPORT-RECORD FROM WS-HEADING-LINE-1
              AFTER TOP-PAGE
           WRITE REPORT-RECORD FROM WS-HEADING-LINE-2
              AFTER 1
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
              AFTER 1
           .
      
      ******************************************************************
       E0000-WRITE-TOTALS.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * WRITE THE TOTAL LINE TO THE REPORT                            *
      *---------------------------------------------------------------*
      
      *** BUILD AND PRINT THE TOTAL LINE
           IF WS-LINE >= WS-LINES-PER-PAGE - 1
              PERFORM D1100-WRITE-HEADINGS
           END-IF
      *
           MOVE SPACES                TO WS-DETAIL-LINE
           MOVE "*** GRAND TOTAL ***" TO DETAIL-CLIENT-ID
           MOVE WS-TOTAL              TO DETAIL-ITEM-COST
      *
           WRITE REPORT-RECORD        FROM WS-DETAIL-LINE
              AFTER 2
           .
      
      ******************************************************************
       F0000-CLOSE-FILES.
      ******************************************************************
	  
      *---------------------------------------------------------------*
      * CLOSE FILES USED IN THE PROGRAM                               *
      *---------------------------------------------------------------*
      
           CLOSE WISH-FILE
                 REPORT-FILE
           .