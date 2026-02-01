       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAX-CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMPLOYEE-NAME       PIC X(20) VALUE "ALICE WONDER".
       01  ANNUAL-INCOME       PIC 9(8)V99 VALUE 75000.00.
       01  TAX-AMOUNT          PIC 9(8)V99.
       01  NET-INCOME          PIC 9(8)V99.
       01  TAX-RATE            PIC V99.
       01  DISP-TAX            PIC $$,$$$,$$9.99.
       01  DISP-NET            PIC $$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "TAX CALCULATION SYSTEM".
           DISPLAY "Employee: " EMPLOYEE-NAME.
           DISPLAY "Income  : " ANNUAL-INCOME.

           EVALUATE TRUE
               WHEN ANNUAL-INCOME <= 10000
                   MOVE 0.00 TO TAX-RATE
                   DISPLAY "Tax Bracket: 0%"
               WHEN ANNUAL-INCOME <= 40000
                   MOVE 0.10 TO TAX-RATE
                   DISPLAY "Tax Bracket: 10%"
               WHEN ANNUAL-INCOME <= 80000
                   MOVE 0.20 TO TAX-RATE
                   DISPLAY "Tax Bracket: 20%"
               WHEN OTHER
                   MOVE 0.30 TO TAX-RATE
                   DISPLAY "Tax Bracket: 30%"
           END-EVALUATE.

           COMPUTE TAX-AMOUNT = ANNUAL-INCOME * TAX-RATE.
           COMPUTE NET-INCOME = ANNUAL-INCOME - TAX-AMOUNT.

           MOVE TAX-AMOUNT TO DISP-TAX.
           MOVE NET-INCOME TO DISP-NET.

           DISPLAY "Tax Due : " DISP-TAX.
           DISPLAY "Net Pay : " DISP-NET.
           STOP RUN.
