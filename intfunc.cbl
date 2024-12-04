      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
      
       PROGRAM-ID.   INTFUNC.
       AUTHOR.       NICK MACDONALD.
       DATE-WRITTEN. 2024-11-26.
      
      ******************************************************************
      *                                                                *
      *   PURPOSE ===> INTRINSIC FUNCTION EXERCISE                     * 
      *                                                                *
      ******************************************************************   

      ******************************************************************   
       ENVIRONMENT DIVISION.
      ******************************************************************   
      
      ******************************************************************   
       CONFIGURATION SECTION.
      ******************************************************************   
       
	   SPECIAL-NAMES.
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
      
      ******************************************************************   
       FILE SECTION.
      ******************************************************************   
      
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
           "*** BEGINNING OF WORKING-STORAGE FOR UNIT14 ***".
      *
       01 WS-PROGRAM-INDICATORS.
          05 WS-MORE-RECORDS                PIC X.
             88 MORE-RECORDS                          VALUE 'Y'.
             88 NO-MORE-RECORDS                       VALUE 'N'.
          05 WS-FIRST-RECORD-SW             PIC X.
             88 FIRST-RECORD                          VALUE 'Y'.
             88 FIRST-RECORD-NOT                      VALUE 'N'.
      *
       01 WS-VARIABLES.
          05 WS-CURRENT-DATE.
             10 WS-DATE                     PIC 9(8).
             10 WS-DATE-X REDEFINES WS-DATE.
                15 WS-YYYY                  PIC 9(4).
                15 WS-MM                    PIC 99.
                15 WS-DD                    PIC 99.
             10 FILLER                      PIC X(13).
          05 WS-INTEGER                     PIC S9(9) BINARY.
          05 WS-LINE                        PIC S9(3) PACKED-DECIMAL.
          05 WS-PAGE                        PIC S9(3) PACKED-DECIMAL.
          05 WS-CLIENT-TOTAL                PIC S9(9)V99
                                                      PACKED-DECIMAL.
          05 WS-GRAND-TOTAL                 PIC S9(9)V99
                                                      PACKED-DECIMAL.
          05 WS-PREV-CLIENT-ID              PIC X(5).
      *
       01 WS-CONSTANTS.
          05 WS-LINES-PER-PAGE              PIC S9(3) PACKED-DECIMAL
                                                      VALUE +60.
      *
       01 WS-ITEM-TABLE.
          05 WS-ITEM-MAX                    PIC S9(4) BINARY VALUE +19.
          05 WS-ITEM-FILLER.
             10 FILLER                      PIC X(30) VALUE
                '00004SHOTGUN                  '.
             10 FILLER                      PIC X(30) VALUE
                '00005BIRD SEED                '.
             10 FILLER                      PIC X(30) VALUE
                '00006A MILLION BUCKS          '.
             10 FILLER                      PIC X(30) VALUE
                '00044SHOTGUN SHELLS           '.
             10 FILLER                      PIC X(30) VALUE
                '00068HALF A MILLION BUCKS     '.
             10 FILLER                      PIC X(30) VALUE
                '00081GIANT RUBBER BANDS       '.
             10 FILLER                      PIC X(30) VALUE
                '00083ROCKET SUIT              '.
             10 FILLER                      PIC X(30) VALUE
                '00086INSTANT HOLES            '.
             10 FILLER                      PIC X(30) VALUE
                '00094STEEL CARROT             '.
             10 FILLER                      PIC X(30) VALUE
                '00107GIGANTIC MAGNET          '.
             10 FILLER                      PIC X(30) VALUE
                '00214REVOLVER                 '.
             10 FILLER                      PIC X(30) VALUE
                '00215SABER                    '.
             10 FILLER                      PIC X(30) VALUE
                '01234CARROTS                  '.
             10 FILLER                      PIC X(30) VALUE
                '09833PERFUME                  '.
             10 FILLER                      PIC X(30) VALUE
                '10000DOG TOYS                 '.
             10 FILLER                      PIC X(30) VALUE
                '12000SHEEP SUIT               '.
             10 FILLER                      PIC X(30) VALUE
                '12875BACON                    '.
             10 FILLER                      PIC X(30) VALUE
                '55555RUNNING SHOES            '.
             10 FILLER                      PIC X(30) VALUE
                '88777SHEET MUSIC              '.
          05 WS-ITEM
             REDEFINES WS-ITEM-FILLER
             OCCURS 19 TIMES
             ASCENDING KEY IS WS-ITEM-NUMBER
             INDEXED BY WS-IDX.
             10 WS-ITEM-NUMBER              PIC X(5).
             10 WS-ITEM-DESCRIPTION         PIC X(25).
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
       01 WS-REPORT-HEADING-1.
          05 WS-HEADING-YYYY                PIC 9(4).
          05 FILLER                         PIC X     VALUE '-'.
          05 WS-HEADING-MM                  PIC 99.
          05 FILLER                         PIC X     VALUE '-'.
          05 WS-HEADING-DD                  PIC 99.
          05 FILLER                         PIC X(29) VALUE SPACES.
          05 FILLER                         PIC X(31) VALUE
             'CHRISTMAS WISH LIST FOR CLIENT'.
          05 WS-HEADING-CLIENT-ID           PIC X(5).
          05 FILLER                         PIC X(30) VALUE SPACES.
          05 FILLER                         PIC X(6)  VALUE 'PAGE:'.
          05 WS-HEADING-PAGE                PIC ZZ9.
      *
       01 WS-REPORT-HEADING-2.
          05 FILLER                         PIC X(34) VALUE
             '  NAME'.
          05 FILLER                         PIC X(50) VALUE
             'DOB      ITEM    DESCRIPTION'.
          05 FILLER                         PIC X(24) VALUE
             'COST      NAUGHTY RATING'.
      *
       01 WS-REPORT-DETAIL.
          05 FILLER                         PIC XX.
          05 DETAIL-CLIENT-NAME             PIC X(25).
          05 FILLER                         PIC X(3).
          05 DETAIL-CLIENT-DOB              PIC X(10).
          05 FILLER                         PIC X(3).
          05 DETAIL-ITEM-NUMBER             PIC 9(5).
          05 FILLER                         PIC X(3).
          05 DETAIL-ITEM-DESCRIPTION        PIC X(25).
          05 FILLER                         PIC X.
          05 DETAIL-ITEM-COST               PIC ZZZ,ZZZ,ZZ9.99.
          05 FILLER                         PIC X(3).
          05 DETAIL-NAUGHTY-RATING          PIC X(20).
      /

      ******************************************************************   
       PROCEDURE DIVISION.
      ******************************************************************   
      
      ******************************************************************   
       MAINLINE.
      ******************************************************************   

           PERFORM A0000-INITIALIZATION
           PERFORM B0000-OPEN-FILES
           PERFORM X1000-READ-RECORD
      *
           PERFORM C0000-PROCESS-RECORDS
              UNTIL NO-MORE-RECORDS
      *
           PERFORM D0000-WRITE-TOTALS
           PERFORM E0000-CLOSE-FILES
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
           MOVE ZERO              TO WS-PAGE,
                                     WS-CLIENT-TOTAL
                                     WS-GRAND-TOTAL
           MOVE WS-LINES-PER-PAGE TO WS-LINE
           MOVE LOW-VALUES        TO WS-PREV-CLIENT-ID
      *
           MOVE FUNCTION CURRENT-DATE
                                  TO WS-CURRENT-DATE
      *
      * CALCULATE YESTERDAY'S DATE
      *
           COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER (
                                FUNCTION INTEGER-OF-DATE (WS-DATE) - 1)
      *
           MOVE WS-YYYY           TO WS-HEADING-YYYY
           MOVE WS-MM             TO WS-HEADING-MM
           MOVE WS-DD             TO WS-HEADING-DD
           .
      *
      ******************************************************************   
       B0000-OPEN-FILES.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * OPEN FILES USED IN THE PROGRAM                                *
      *---------------------------------------------------------------*
      
           OPEN INPUT  WISH-FILE
                OUTPUT REPORT-FILE
           .
      
      ******************************************************************   
       C0000-PROCESS-RECORDS.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * PROCESS ONE RECORD, THEN READ THE NEXT ONE                    *
      *---------------------------------------------------------------*
      *
           IF WISH-CLIENT-ID NOT = WS-PREV-CLIENT-ID
              PERFORM C1000-CLIENT-BREAK
           END-IF
      *
           ADD WISH-ITEM-COST TO WS-CLIENT-TOTAL
                                 WS-GRAND-TOTAL
      *
           PERFORM C2000-WRITE-DETAIL
           PERFORM X1000-READ-RECORD
           .
      *
      ******************************************************************   
       C1000-CLIENT-BREAK.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * HANDLE CONTROL BREAK PROCESSING WHEN A NEW CLIENT ID IS FOUND *
      *---------------------------------------------------------------*
      
           IF WS-PREV-CLIENT-ID > LOW-VALUES
              MOVE SPACES                 TO WS-REPORT-DETAIL
              MOVE '*** CLIENT TOTAL ***' TO DETAIL-CLIENT-NAME
              MOVE WS-CLIENT-TOTAL        TO DETAIL-ITEM-COST
              WRITE REPORT-RECORD         FROM WS-REPORT-DETAIL
                 AFTER 2
           END-IF
      *
           MOVE ZERO                      TO WS-CLIENT-TOTAL
           MOVE WISH-CLIENT-ID            TO WS-PREV-CLIENT-ID
           SET  FIRST-RECORD              TO TRUE
           MOVE WS-LINES-PER-PAGE         TO WS-LINE
           .
      
      ******************************************************************   
       C2000-WRITE-DETAIL.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * WRITE A DETAIL RECORD TO THE REPORT                           *
      *---------------------------------------------------------------*
      
           IF WS-LINE >= WS-LINES-PER-PAGE
              PERFORM C2100-WRITE-HEADINGS
           END-IF
      *
           ADD  1                TO WS-LINE
           MOVE SPACES           TO WS-REPORT-DETAIL
           MOVE WISH-ITEM-NUMBER TO DETAIL-ITEM-NUMBER
           MOVE WISH-ITEM-COST   TO DETAIL-ITEM-COST
      *
           PERFORM C2200-FIND-ITEM-DESCRIPTION      
	  *
      * POPULATE THESE FIELDS ONLY ON THE FIRST LINE FOR A CLIENT
      *
           IF FIRST-RECORD
              SET  FIRST-RECORD-NOT        TO TRUE
              MOVE FUNCTION DISPLAY-OF (
                      FUNCTION UPPER-CASE (
                         FUNCTION NATIONAL-OF (WISH-CLIENT-NAME)))
                                           TO DETAIL-CLIENT-NAME
              MOVE WISH-CLIENT-DOB         TO DETAIL-CLIENT-DOB
      *
              PERFORM C2300-TRANSLATE-RATING
           END-IF
      *
           WRITE REPORT-RECORD FROM WS-REPORT-DETAIL
              AFTER 1
           .
      
      ******************************************************************   
       C2100-WRITE-HEADINGS.
      ******************************************************************   
	  	 
      *---------------------------------------------------------------*
      * WRITE HEADING LINES ON THE REPORT                             *
      *---------------------------------------------------------------*
      
           ADD  1              TO WS-PAGE
           MOVE ZERO           TO WS-LINE
           MOVE WS-PAGE        TO WS-HEADING-PAGE
           MOVE SPACES         TO WS-REPORT-DETAIL
           MOVE WISH-CLIENT-ID TO WS-HEADING-CLIENT-ID
      *
           WRITE REPORT-RECORD FROM WS-REPORT-HEADING-1
              AFTER TOP-PAGE
           WRITE REPORT-RECORD FROM WS-REPORT-HEADING-2
              AFTER 1
           WRITE REPORT-RECORD FROM WS-REPORT-DETAIL
              AFTER 1
           .
      
      ******************************************************************   
       C2200-FIND-ITEM-DESCRIPTION.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * FIND THE ITEM DESCRIPTION IN THE TABLE                        *
      *---------------------------------------------------------------*
      
           SEARCH ALL WS-ITEM
              AT END
                 MOVE '* NO DESCRIPTION FOUND'     TO
                      DETAIL-ITEM-DESCRIPTION
              WHEN  WS-ITEM-NUMBER (WS-IDX) = WISH-ITEM-NUMBER
                 MOVE WS-ITEM-DESCRIPTION (WS-IDX) TO
                      DETAIL-ITEM-DESCRIPTION
           END-SEARCH
           .
      
      ******************************************************************   
       C2300-TRANSLATE-RATING.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * TRANSLATE THE NAUGHTY RATING TO A TEXT VALUE FOR PRINTING     *
      *---------------------------------------------------------------*
      
           EVALUATE WISH-NAUGHTY-RATING
            WHEN '01'
              MOVE 'CONVICTED         ' TO DETAIL-NAUGHTY-RATING
            WHEN '02'
              MOVE 'NAUGHTY           ' TO DETAIL-NAUGHTY-RATING
            WHEN '03'
              MOVE 'NEUTRAL           ' TO DETAIL-NAUGHTY-RATING
            WHEN '04'
              MOVE 'NICE              ' TO DETAIL-NAUGHTY-RATING
            WHEN '05'
              MOVE 'SAINT             ' TO DETAIL-NAUGHTY-RATING
            WHEN OTHER
              MOVE '* INVALID RATING *' TO DETAIL-NAUGHTY-RATING
           END-EVALUATE
           .
      *
      ******************************************************************   
       D0000-WRITE-TOTALS.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * WRITE THE TOTAL LINE TO THE REPORT                            *
      *---------------------------------------------------------------*
      
           MOVE  SPACES                 TO WS-REPORT-DETAIL
           MOVE  '*** CLIENT TOTAL ***' TO DETAIL-CLIENT-NAME
           MOVE  WS-CLIENT-TOTAL        TO DETAIL-ITEM-COST
      *
           WRITE REPORT-RECORD          FROM WS-REPORT-DETAIL
              AFTER 2
      *
           MOVE  '*** GRAND TOTAL ***'  TO DETAIL-CLIENT-NAME
           MOVE  WS-GRAND-TOTAL         TO DETAIL-ITEM-COST
      *
           WRITE REPORT-RECORD          FROM WS-REPORT-DETAIL
              AFTER 2
           .
      
      ******************************************************************   
       E0000-CLOSE-FILES.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * CLOSE FILES USED IN THE PROGRAM                               *
      *---------------------------------------------------------------*
      
           CLOSE WISH-FILE
                 REPORT-FILE
           .
      
      ******************************************************************   
       X1000-READ-RECORD.
      ******************************************************************   
	  
      *---------------------------------------------------------------*
      * READ A RECORD FROM THE INPUT FILE                             *
      *---------------------------------------------------------------*
      
           READ WISH-FILE INTO WS-WISH-RECORD
              AT END
                 SET NO-MORE-RECORDS TO TRUE
           END-READ
           .