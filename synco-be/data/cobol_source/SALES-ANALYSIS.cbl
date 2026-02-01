       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-ANALYSIS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SALES-TABLE.
           05  REGION OCCURS 4 TIMES.
               10  MONTH-SALES PIC 9(5) OCCURS 3 TIMES.
       
       01  WS-REGION-TOT       PIC 9(6).
       01  WS-GRAND-TOT        PIC 9(7) VALUE 0.
       01  I                   PIC 9.
       01  J                   PIC 9.
       01  REGION-NAMES.
           05  FILLER PIC X(5) VALUE "NORTH".
           05  FILLER PIC X(5) VALUE "SOUTH".
           05  FILLER PIC X(5) VALUE "EAST ".
           05  FILLER PIC X(5) VALUE "WEST ".
       01  R-NAME-TABLE REDEFINES REGION-NAMES.
           05  R-NAME PIC X(5) OCCURS 4 TIMES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 100 TO MONTH-SALES(1, 1). MOVE 150 TO MONTH-SALES(1, 2). MOVE 120 TO MONTH-SALES(1, 3).
           MOVE 200 TO MONTH-SALES(2, 1). MOVE 210 TO MONTH-SALES(2, 2). MOVE 220 TO MONTH-SALES(2, 3).
           MOVE 050 TO MONTH-SALES(3, 1). MOVE 060 TO MONTH-SALES(3, 2). MOVE 055 TO MONTH-SALES(3, 3).
           MOVE 300 TO MONTH-SALES(4, 1). MOVE 310 TO MONTH-SALES(4, 2). MOVE 350 TO MONTH-SALES(4, 3).

           DISPLAY "QUARTERLY SALES ANALYSIS (3 MONTHS)".
           DISPLAY "REGION   M1    M2    M3    TOTAL"
           DISPLAY "----------------------------------".

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               MOVE 0 TO WS-REGION-TOT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   ADD MONTH-SALES(I, J) TO WS-REGION-TOT
               END-PERFORM
               ADD WS-REGION-TOT TO WS-GRAND-TOT
               
               DISPLAY R-NAME(I) "    " MONTH-SALES(I, 1) "   " 
                  MONTH-SALES(I, 2) "   " MONTH-SALES(I, 3) "   " 
                  WS-REGION-TOT
           END-PERFORM.

           DISPLAY "----------------------------------".
           DISPLAY "GRAND TOTAL: " WS-GRAND-TOT.
           STOP RUN.
