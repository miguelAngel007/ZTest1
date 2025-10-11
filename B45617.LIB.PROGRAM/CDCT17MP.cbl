       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCT17MP.
       AUTHOR.     MACDOM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-OUTPUT ASSIGN TO MPRFOUT
               FILE STATUS IS FS-MPRF.
       DATA DIVISION.
       FILE SECTION.
       FD  FILE-OUTPUT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 REG-OUTPUT.
         05 CO_MEDI_PAGO_OUT      PIC X(3).
         05 NO_MEDI_PAGO_OUT      PIC X(30).
         05 NC_MEDI_PAGO_OUT      PIC X(4).
         05 FILL-OUT              PIC X(43) VALUE  SPACES .

       WORKING-STORAGE SECTION.

           EXEC SQL INCLUDE SQLCA END-EXEC.


       01 FS-MPRF     PIC 99.
       01 WS_CO_MEDI_PAGO   PIC X(3).
       01 WS_NO_MEDI_PAGO   PIC X(30).
       01 WS_NC_MEDI_PAGO   PIC X(4).

       01 SW-EC-MP.
         05 SW-EC-MP-END     PIC XX VALUE 'NO'.
           88 SW-EC-MP-FIN-OK VALUE 'SI'.
           88 SW-EC-MP-FIN-NO VALUE 'NO'.

      *+-----------------------------------------------------------+
      *          SQL CURSOR
      *+-----------------------------------------------------------+
           EXEC SQL
                 DECLARE CURSOR_MP CURSOR FOR
                 SELECT
                 CO_MEDI_PAGO,
                 NO_MEDI_PAGO,
                 NC_MEDI_PAGO
                 FROM I.TA_MEDI_PAGOX

           END-EXEC.



       PROCEDURE DIVISION.
           PERFORM  START-PROGRAM
           PERFORM  PROCESS-MP
           PERFORM  FINALIZE.

       START-PROGRAM.
           INITIALIZE SW-EC-MP
           OPEN OUTPUT FILE-OUTPUT.

       OPEN-CURSOR-MP.
           DISPLAY 'STARTING CURSOR MP'.

           EXEC SQL
               OPEN CURSOR_MP
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR OPEN CURSOR MP: ' SQLCODE
               STOP RUN
           END-IF.

       PROCESS-MP.
           PERFORM OPEN-CURSOR-MP.
           IF (NOT SW-EC-MP-FIN-OK )
               PERFORM PROCESS-MP-RECORDS UNTIL SW-EC-MP-FIN-OK
           END-IF.

       PROCESS-MP-RECORDS.
           PERFORM READ-CURSOR-MP
           IF (NOT SW-EC-MP-FIN-OK)
               MOVE SPACES TO CO_MEDI_PAGO_OUT
               MOVE SPACES TO NO_MEDI_PAGO_OUT
               MOVE SPACES TO NC_MEDI_PAGO_OUT
               MOVE SPACES TO FILL-OUT

               MOVE WS_CO_MEDI_PAGO  TO CO_MEDI_PAGO_OUT
               MOVE WS_NC_MEDI_PAGO  TO NC_MEDI_PAGO_OUT
               MOVE WS_NO_MEDI_PAGO  TO NO_MEDI_PAGO_OUT

               WRITE REG-OUTPUT

           END-IF.

       READ-CURSOR-MP.
           EXEC SQL
                 FETCH CURSOR_MP INTO
                 :WS_CO_MEDI_PAGO,
                 :WS_NO_MEDI_PAGO,
                 :WS_NC_MEDI_PAGO

           END-EXEC

           IF SQLCODE = 0
               DISPLAY '----------------------------'
               DISPLAY 'CO_MEDI_PAGO : ' WS_CO_MEDI_PAGO
               DISPLAY 'NO_MEDI_PAGO : ' WS_NO_MEDI_PAGO
               DISPLAY 'NC_MEDI_PAGO : ' WS_NC_MEDI_PAGO

           ELSE
               IF SQLCODE = 100
                   SET SW-EC-MP-FIN-OK  TO TRUE
               ELSE
                   DISPLAY 'ERROR FETCH MP: ' SQLCODE
               END-IF.

       FINALIZE.
           CLOSE FILE-OUTPUT
            EXEC SQL
               CLOSE CURSOR_MP
           END-EXEC
           STOP RUN.

