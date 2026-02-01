       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUBBLE-SORT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM-ARRAY.
           05  NUM-VAL         PIC 9(3) OCCURS 10 TIMES.
       01  I                   PIC 99.
       01  J                   PIC 99.
       01  TEMP                PIC 9(3).
       01  ARRAY-SIZE          PIC 99 VALUE 10.
       01  SWAPPED             PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 045 TO NUM-VAL(1).
           MOVE 012 TO NUM-VAL(2).
           MOVE 089 TO NUM-VAL(3).
           MOVE 001 TO NUM-VAL(4).
           MOVE 023 TO NUM-VAL(5).
           MOVE 099 TO NUM-VAL(6).
           MOVE 005 TO NUM-VAL(7).
           MOVE 067 TO NUM-VAL(8).
           MOVE 032 TO NUM-VAL(9).
           MOVE 010 TO NUM-VAL(10).

           DISPLAY "Unsorted Array:".
           PERFORM PRINT-ARRAY.

           DISPLAY "Sorting...".
           
           PERFORM UNTIL SWAPPED = 'N'
             MOVE 'N' TO SWAPPED
             PERFORM VARYING I FROM 1 BY 1 UNTIL I >= ARRAY-SIZE
                COMPUTE J = I + 1
                IF NUM-VAL(I) > NUM-VAL(J)
                    MOVE NUM-VAL(I) TO TEMP
                    MOVE NUM-VAL(J) TO NUM-VAL(I)
                    MOVE TEMP TO NUM-VAL(J)
                    MOVE 'Y' TO SWAPPED
                END-IF
             END-PERFORM
           END-PERFORM.

           DISPLAY "Sorted Array:".
           PERFORM PRINT-ARRAY.
           STOP RUN.

       PRINT-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY "Element " I ": " NUM-VAL(I)
           END-PERFORM.
