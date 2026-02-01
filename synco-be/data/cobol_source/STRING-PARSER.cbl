       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-PARSER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RAW-DATA            PIC X(50) VALUE "John;Doe;Dev;IT;50000".
       01  WS-ID               PIC X(10).
       01  WS-FNAME            PIC X(15).
       01  WS-LNAME            PIC X(15).
       01  WS-ROLE             PIC X(15).
       01  WS-DEPT             PIC X(10).
       01  WS-SALARY           PIC X(10).
       01  PTR                 PIC 99 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "STRING PARSING DEMO".
           DISPLAY "Raw Data: " RAW-DATA.
           DISPLAY "--------------------".

           UNSTRING RAW-DATA DELIMITED BY ";"
               INTO WS-FNAME, WS-LNAME, WS-ROLE, WS-DEPT, WS-SALARY
               WITH POINTER PTR.

           DISPLAY "Parsed Fields:".
           DISPLAY "First Name: " WS-FNAME.
           DISPLAY "Last Name : " WS-LNAME.
           DISPLAY "Role      : " WS-ROLE.
           DISPLAY "Department: " WS-DEPT.
           DISPLAY "Salary    : " WS-SALARY.

           INSPECT WS-ROLE REPLACING ALL "e" BY "E".
           DISPLAY "Uppercase 'E' in Role: " WS-ROLE.

           STOP RUN.
