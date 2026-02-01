       IDENTIFICATION DIVISION.
       PROGRAM-ID. PALINDROME-CHECK.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-STRING        PIC X(20) VALUE "RACECAR".
       01  CLEAN-STRING        PIC X(20).
       01  TEMP-STRING         PIC X(20).
       01  LEN                 PIC 99.
       01  I                   PIC 99.
       01  J                   PIC 99.
       01  IS-PALINDROME       PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Checking: " INPUT-STRING.
           
           MOVE INPUT-STRING TO CLEAN-STRING.
           MOVE 0 TO LEN.
           INSPECT CLEAN-STRING TALLYING LEN FOR CHARACTERS
               BEFORE INITIAL " ".
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
               COMPUTE J = LEN - I + 1
               IF CLEAN-STRING(I:1) NOT = CLEAN-STRING(J:1)
                   MOVE 'N' TO IS-PALINDROME
               END-IF
           END-PERFORM.

           IF IS-PALINDROME = 'Y'
               DISPLAY "Result: VALID PALINDROME"
           ELSE
               DISPLAY "Result: NOT A PALINDROME"
           END-IF.
           STOP RUN.
