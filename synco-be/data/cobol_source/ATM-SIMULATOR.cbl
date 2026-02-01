       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-SIMULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BALANCE             PIC S9(5)V99 VALUE 1000.00.
       01  TXN-AMOUNT          PIC 9(5)V99.
       01  OPTION              PIC 9.
       01  DISP-BAL            PIC $$,$$9.99.
       01  SIMULATED-INPUTS    PIC X(3) VALUE "123".
       01  SIM-IDX            PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "ATM SIMULATOR STARTED".
           DISPLAY "Initial Balance: $" BALANCE.

           PERFORM PROCESS-TRANSACTION 3 TIMES.
           
           DISPLAY "SESSION ENDED. THANK YOU.".
           STOP RUN.

       PROCESS-TRANSACTION.
           MOVE SIMULATED-INPUTS(SIM-IDX:1) TO OPTION.
           ADD 1 TO SIM-IDX.

           DISPLAY "-----------------".
           DISPLAY "Menu: 1.Deposit 2.Withdraw 3.Exit".
           DISPLAY "User Selected: " OPTION.

           EVALUATE OPTION
               WHEN 1
                   MOVE 500.00 TO TXN-AMOUNT
                   ADD TXN-AMOUNT TO BALANCE
                   DISPLAY "Deposited: $" TXN-AMOUNT
               WHEN 2
                   MOVE 200.00 TO TXN-AMOUNT
                   IF BALANCE >= TXN-AMOUNT
                       SUBTRACT TXN-AMOUNT FROM BALANCE
                       DISPLAY "Withdrew: $" TXN-AMOUNT
                   ELSE
                       DISPLAY "Insufficient Funds!"
                   END-IF
               WHEN 3
                   DISPLAY "Exiting..."
               WHEN OTHER
                   DISPLAY "Invalid Option"
           END-EVALUATE.

           MOVE BALANCE TO DISP-BAL.
           DISPLAY "Current Balance: " DISP-BAL.
