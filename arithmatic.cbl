      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************       
	   PROGRAM-ID.   ARIT.                                             
       AUTHOR.       NICK MACDONALD.                                    
       DATE-WRITTEN. 2024-11-22.     
       
      ******************************************************************
      *                                                                *
      *   PURPOSE ===> CALCULATE TIP AMOUNT AND YOUR SHARE OF THE      *
      *                RESTAURANT BILL                                 *
      *                                                                *
      ******************************************************************                                                       
	  
      ******************************************************************
	   DATA DIVISION.                                                   
      ******************************************************************
	 
      ******************************************************************
       WORKING-STORAGE SECTION.                                         
      ******************************************************************      
       01 WS-EYECATCHER                     PIC X(46) VALUE             
           "*** BEGINNING OF WORKING-STORAGE ***".            
      *                                                                 
       01 WS-PROGRAM-VARIABLES.                                         
          05 WS-BILL                        PIC S999V99   COMP-3.        
          05 WS-BILL-OUT                    PIC ZZ9.99.                 
          05 WS-DINERS                      PIC S999      COMP-3.         
          05 WS-DINERS-OUT                  PIC ZZ9.                    
          05 WS-TAX                         PIC S999V99   COMP-3.         
          05 WS-TIP                         PIC S99V99    COMP-3.         
          05 WS-TIP-OUT                     PIC Z9.99.                  
          05 WS-TOTAL                       PIC S9(5)V99  COMP-3.        
          05 WS-TOTAL-OUT                   PIC ZZ,ZZ9.99.              
          05 WS-SHARE                       PIC S999V99   COMP-3.         
          05 WS-SHARE-OUT                   PIC ZZ9.99.                 
      *                                                                 
       01 WS-PROGRAM-INDICATORS.                                        
          05 WS-VALID-INPUT-SW              PIC X.                      
             88 WS-VALID-INPUT                            VALUE 'Y'.      
             88 WS-VALID-INPUT-NO                         VALUE 'N'.      
      *                                                                 
       01 WS-PROGRAM-CONSTANT.                                          
          05 WS-TAX-RATE                    PIC S9V99     COMP-3          
             VALUE +0.13.                                               
          05 WS-TIP-PERCENT                 PIC S9V99     COMP-3          
             VALUE +0.15.                                               
      /                                                                       
      ******************************************************************	  
       LINKAGE SECTION.                                                 
      ******************************************************************                                                                 
       01 LS-PARMS.                                                     
          05 LS-PARM-LENGTH                 PIC S9(4)  COMP.            
          05 LS-BILL-DOLLARS-X.                                         
             10 LS-BILL-DOLLARS             PIC 999.                    
          05 LS-BILL-DECIMAL                PIC X.                      
          05 LS-BILL-CENTS-X.                                           
             10 LS-BILL-CENTS               PIC 99.                     
          05 LS-COMMA                       PIC X.                      
          05 LS-DINERS-X.                                               
             10 LS-DINERS                   PIC 99.                     
          05 FILLER                         PIC X(91).                  
      /                                                                 
      ******************************************************************	  
       PROCEDURE DIVISION USING LS-PARMS.                               
      ******************************************************************                                                                      

      ******************************************************************
	   PROGRAM-MAINLINE.                                                
      ******************************************************************
           PERFORM A0000-INITIALIZATION                                 
      *                                                                 
           IF WS-VALID-INPUT                                            
              PERFORM B0000-CALCULATE-MY-SHARE                          
              PERFORM C0000-WRAP-UP                                     
           END-IF                                                       
      *                                                                 
           STOP RUN                                                     
           .                                                            
                                                                     
      ******************************************************************                                                              
       A0000-INITIALIZATION.                                            
      ******************************************************************

      *---------------------------------------------------------------* 
      * VALIDATE THE INPUT PARAMETER                                  * 
      *---------------------------------------------------------------* 
           IF LS-PARM-LENGTH    = +9       AND                          
              LS-BILL-DECIMAL   = '.'      AND                          
              LS-COMMA          = ','      AND                          
              LS-BILL-DOLLARS-X IS NUMERIC AND                          
              LS-BILL-CENTS-X   IS NUMERIC AND                          
              LS-DINERS-X       IS NUMERIC                              
      *                                                                 
              SET WS-VALID-INPUT    TO TRUE                             
              COMPUTE WS-BILL =                                         
                 LS-BILL-DOLLARS + (LS-BILL-CENTS / 100)                
              MOVE LS-DINERS       TO WS-DINERS                         
           ELSE                                                         
              SET WS-VALID-INPUT-NO TO TRUE                             
              DISPLAY 'ERROR, THE PROGRAM REQUIRES AN INPUT '           
                      'PARAMETER IN THE FORMAT NNN.NN,DD'               
              DISPLAY ' '                                               
              DISPLAY 'WHERE: '                                         
              DISPLAY ' '                                               
              DISPLAY '   NNN.NN IS THE BILL AMOUNT, AND'               
              DISPLAY '   DD    IS THE NUMBER OF DINERS'                
              DISPLAY ' '                                               
      *                                                                 
              IF LS-PARM-LENGTH > 0                                     
                 DISPLAY 'FOUND: '                                      
                          LS-PARMS(3:LS-PARM-LENGTH)                    
              END-IF                                                    
           END-IF                                                       
           .                                                            
      
      ******************************************************************                                                                                                                              
       B0000-CALCULATE-MY-SHARE.                                        
      ******************************************************************                                                              
      
      *---------------------------------------------------------------* 
      * CALCULATE THE TIP, AND EACH DINER'S PORTION OF THE BILL       * 
      *---------------------------------------------------------------* 
      
      *
      * CACLULATE THE AMOUNT OF TAX ON THE BILL                         	  
      *    COMPUTE ...                                                  
           COMPUTE WS-TAX ROUNDED =                                     
              WS-BILL * WS-TAX-RATE / (1 + WS-TAX-RATE)                 

      *
      * CALCULATE THE TIP AS 15% OF THE BEFORE-TAX AMOUNT OF THE BILL   
      *    COMPUTE ...                                                  
           COMPUTE WS-TIP ROUNDED =                                     
              (WS-BILL - WS-TAX) * WS-TIP-PERCENT                       
              SIZE ERROR                                                
                 MOVE +99.99 TO WS-TIP                                  
           END-COMPUTE                                                  

      *
      * CALCULATE THE TOTAL AMOUNT OWED                                 
      *    ADD ...                                                      
           ADD WS-TIP, WS-BILL                                          
              GIVING WS-TOTAL                                           

      *
      * CALCULATE THE TOTAL AMOUNT PER DINER                            
      *    DIVIDE ...                                                   
           DIVIDE WS-TOTAL BY WS-DINERS                                 
              GIVING WS-SHARE ROUNDED                                   
           .    
		   
      ******************************************************************                                                              
       C0000-WRAP-UP.                                                   
      ******************************************************************                                                              
	  
      *---------------------------------------------------------------* 
      * CLOSE THE FILES                                               * 
      *---------------------------------------------------------------*       
           MOVE WS-BILL   TO WS-BILL-OUT                                
           MOVE WS-TIP    TO WS-TIP-OUT                                 
           MOVE WS-TOTAL  TO WS-TOTAL-OUT                               
           MOVE WS-DINERS TO WS-DINERS-OUT                              
           MOVE WS-SHARE  TO WS-SHARE-OUT                               
      *                                                                 
           DISPLAY '     '    WS-BILL-OUT   ' BILL'                     
           DISPLAY '      '   WS-TIP-OUT    ' PLUS TIP'                 
           DISPLAY '  ---------'                                        
           DISPLAY '  '       WS-TOTAL-OUT  ' TOTAL'                    
           DISPLAY '        ' WS-DINERS-OUT ' DIVIDED BY DINERS'        
           DISPLAY '  ---------'                                        
           DISPLAY '     '    WS-SHARE-OUT  ' YOUR SHARE'               
           .                                                            