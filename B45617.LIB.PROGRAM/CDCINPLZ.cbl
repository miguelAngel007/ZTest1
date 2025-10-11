       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCINPLZ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTAS-READ ASSIGN TO CUENTAS
             FILE STATUS IS FS-CTA-READ.
           SELECT TASAS-READ ASSIGN TO TASAS
                       FILE STATUS IS FS-TASAS-READ.
           SELECT CTAS-CALC ASSIGN TO CTASINT
                       FILE STATUS IS FS-CTAS-CALS.
       DATA DIVISION.
       FILE SECTION.
       FD CTAS-READ.
       01 CTAS-INPUT.
         05 UID                PIC X(3).
         05 NAME               PIC X(15).
         05 LASTNAME           PIC X(15).
         05 DEPOSIT-DATE.
             10 YEAR           PIC 9(4).
             10 MONTH          PIC 9(2).
             10 DAY-DD         PIC 9(2).
         05 CTA-AMOUNT         PIC 9(8)V99.
         05 FILLER             PIC X(29).

       FD TASAS-READ.
       01 TASAS-INPUT.
         05 MIN-RANGE-AMOUNT  PIC 9(8)V99.
         05 MAX-RANGE-AMOUNT   PIC 9(8)V99.
         05 RATE               PIC 9(2)V99.
         05 FILLER             PIC X(56).

       FD CTAS-CALC.
       01 CTAS-OUPUT.
         05 OUT-UID                PIC X(3).
         05 OUT-DEPOSIT-DATE.
           10 YEAR             PIC 9(4).
           10 MONTH            PIC 9(2).
           10 DAY-DD           PIC 9(2).
         05 OUT-NAME               PIC X(15).
         05 OUT-LASTNAME           PIC X(15).
         05 OUT-AMOUNT             PIC 9(8)V99.
         05 OUT-INTEREST           PIC 9(8)V99.
         05 OUT-TOTAL              PIC 9(8)V99.
         05 OUT-FILLER             PIC X(9).

       WORKING-STORAGE SECTION.

       01 FS-CTA-READ          PIC 99.
       01 FS-TASAS-READ        PIC 99.
       01 FS-CTAS-CALS         PIC 99.

       01 WS-RATE-IDX          PIC 99 VALUE 0.
       01 WS-I                 PIC 99 VALUE 0.

       01 RATE-AMOUNT          PIC 9(8)V99.
       01 CALC-CTA-AMOUNT      PIC 9(8)V99.
       01 CALC-INTERST         PIC 9(8)V99.
       01 TEM-INTERST          PIC 9(8)V99.
       01 CALC-MAX-AMOUNT      PIC 9(8)V99.
       01 TEMP-MIN-RANGE-AMT   PIC 9(8)V99.
       01 TEMP-MAX-RANGE-AMT   PIC 9(8)V99.
       01 TEMP-MAX-AMT   PIC 9(8)V99.

       01 TABLE-RATES.
         05 REG-RATE             OCCURS 15 INDEXED BY RATE-IDX.
           10 TB-MIN-RANGE-AMOUNT   PIC 9(8)V99.
           10 TB-MAX-RANGE-AMOUNT   PIC 9(8)V99.
           10 TB-RATE               PIC 9(2)V99.

       01 SW-CTAS.
         05 SW-CTAS-END                PIC XX VALUE 'NO'.
           88 SW-CTAS-FIN-OK           VALUE 'SI'.
           88 SW-CTAS-FIN-NO           VALUE 'NO'.

       01 SW-EOF-RATE.
         05 SW-EOF-RATES-END           PIC XX VALUE 'NO'.
           88 SW-EOF-RATE-FIN-OK       VALUE 'SI'.
           88 SW-EOF-RATE-FIN-NO       VALUE 'NO'.

       01 WS_NO_MEDI_PAGO_1   PIC X(30).
       01 WS_NO_MEDI_PAGO_2   PIC X(30).
       01 WS_NO_MEDI_PAGO_3   PIC X(30).
       01 WS_NO_MEDI_PAGO_4   PIC X(30).
       01 WS_NO_MEDI_PAGO_5   PIC X(30).
       01 WS_NO_MEDI_PAGO_6   PIC X(30).
       01 WS_NO_MEDI_PAGO_7   PIC X(30).

       PROCEDURE DIVISION.
           PERFORM START-PROGRAM
           PERFORM LOAD-RATES
           PERFORM PROCESS-CTA-FILE
           PERFORM CLOSE-FILES
           STOP RUN.

       START-PROGRAM.
           PERFORM OPEN-FILES
           .
       OPEN-FILES.
           OPEN INPUT CTAS-READ
           OPEN INPUT TASAS-READ
           OPEN OUTPUT CTAS-CALC
      *    VALIDAR ESTADO DE ARCHIVOS, SI SALE TERMINAR EL PROGRAMA
           .
       CLOSE-FILES.
           CLOSE CTAS-READ
           CLOSE TASAS-READ
           CLOSE CTAS-CALC.

       READ-CTAS-FILE.
           READ CTAS-READ
           IF FS-CTA-READ = 10
      *        PERFORM ESCRIBIR-REGISTRO-SALIDA
               DISPLAY 'FIN DEL ARCHIVO CUENTAS ' FS-CTA-READ
               SET SW-CTAS-FIN-OK TO TRUE
           ELSE
               IF FS-CTA-READ NOT = 0
                   DISPLAY 'ERROR LEYENDO CEUNTAS: ' FS-CTA-READ
                   SET SW-CTAS-FIN-OK TO TRUE
               END-IF
           END-IF.


       PROCESS-CTA-FILE.
           IF (NOT SW-CTAS-FIN-OK)
               PERFORM PROCESS-CTA-FILE-RECORDS UNTIL SW-CTAS-FIN-OK
           END-IF.

       PROCESS-CTA-FILE-RECORDS.
           PERFORM READ-CTAS-FILE
           IF (NOT SW-CTAS-FIN-OK)
               PERFORM CALC-INTEREST
           END-IF
           .

       CALC-INTEREST.
           MOVE 0 TO RATE-AMOUNT
           MOVE 0 TO CALC-INTERST
           MOVE 0 TO CALC-CTA-AMOUNT
           MOVE 0 TO TEMP-MIN-RANGE-AMT
           MOVE 0 TO TEMP-MAX-RANGE-AMT
           MOVE 0 TO TEMP-MAX-AMT

           MOVE CTA-AMOUNT TO CALC-CTA-AMOUNT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I = WS-RATE-IDX
               IF WS-I = 1
                   COMPUTE TEMP-MAX-AMT = TB-MAX-RANGE-AMOUNT(WS-I)
               ELSE
                   COMPUTE TEMP-MAX-AMT = TB-MAX-RANGE-AMOUNT(WS-I) -
                     TB-MAX-RANGE-AMOUNT(WS-I - 1)
               END-IF

               MOVE TB-MIN-RANGE-AMOUNT(WS-I) TO TEMP-MIN-RANGE-AMT
               MOVE TB-MAX-RANGE-AMOUNT(WS-I) TO TEMP-MAX-RANGE-AMT

               IF (CTA-AMOUNT > TEMP-MIN-RANGE-AMT)
                   MOVE 0 TO TEM-INTERST
                   IF (CALC-CTA-AMOUNT > TEMP-MAX-AMT)
                       MOVE TEMP-MAX-AMT TO CALC-MAX-AMOUNT
                   ELSE
                       MOVE CALC-CTA-AMOUNT TO CALC-MAX-AMOUNT
                   END-IF

                   COMPUTE TEM-INTERST ROUNDED = CALC-MAX-AMOUNT *
                     (TB-RATE(WS-I) / 100) / 360

      *            DISPLAY 'INTERES ITR ' TEM-INTERST

                   COMPUTE CALC-INTERST = CALC-INTERST + TEM-INTERST
                   COMPUTE CALC-CTA-AMOUNT =
                     CALC-CTA-AMOUNT - TEMP-MAX-AMT

               END-IF
           END-PERFORM
      *    DISPLAY CTAS-INPUT
      *    DISPLAY CALC-INTERST

      *    MOVE CTAS-INPUT TO CTAS-OUPUT
           MOVE UID TO OUT-UID
           MOVE DEPOSIT-DATE TO OUT-DEPOSIT-DATE
           MOVE NAME TO OUT-NAME
           MOVE LASTNAME TO OUT-LASTNAME
           MOVE CTA-AMOUNT TO OUT-AMOUNT
           MOVE CALC-INTERST TO OUT-INTEREST
           COMPUTE OUT-TOTAL = CTA-AMOUNT + CALC-INTERST
           MOVE SPACES TO OUT-FILLER

           WRITE CTAS-OUPUT
           .
       LOAD-RATES.
           PERFORM PROCESS-RATE-FILE.
       PROCESS-RATE-FILE.
           PERFORM PROCESS-RATE-FILE-RECORDS UNTIL SW-EOF-RATE-FIN-OK.
       PROCESS-RATE-FILE-RECORDS.
           PERFORM READ-RATES-FILE
           IF NOT SW-EOF-RATE-FIN-OK
               ADD 1 TO WS-RATE-IDX
               MOVE TASAS-INPUT TO REG-RATE(WS-RATE-IDX)
           END-IF.
       READ-RATES-FILE.
           READ TASAS-READ
           IF (FS-TASAS-READ = 10)
               SET SW-EOF-RATE-FIN-OK TO TRUE
           END-IF.

       END PROGRAM CDCINPLZ.
