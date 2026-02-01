       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI-GEN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N-TERM           PIC 9(4) VALUE 20.
       01  TERM-1           PIC 9(18) VALUE 0.
       01  TERM-2           PIC 9(18) VALUE 1.
       01  NEXT-TERM        PIC 9(18).
       01  I                PIC 9(4).
       01  DISPLAY-NUM      PIC Z(17)9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "FIBONACCI GEN (First " N-TERM " nums)".
           DISPLAY "--------------------------------------------------".

           DISPLAY "Term 1 : " TERM-1.
           DISPLAY "Term 2 : " TERM-2.

           PERFORM VARYING I FROM 3 BY 1 UNTIL I > N-TERM
               COMPUTE NEXT-TERM = TERM-1 + TERM-2
               MOVE NEXT-TERM TO DISPLAY-NUM
               DISPLAY "Term " I " : " DISPLAY-NUM
               MOVE TERM-2 TO TERM-1
               MOVE NEXT-TERM TO TERM-2
           END-PERFORM.

           DISPLAY "--------------------------------------------------".
           DISPLAY "GENERATION COMPLETE.".
           STOP RUN.
