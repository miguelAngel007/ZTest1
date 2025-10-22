       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCCGXY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ENTRADA ASSIGN TO IGXY.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-ENTRADA.
       01  REG-ENTRADA.
           05 ITEM       PIC X(20).
           05 VALOR-X    PIC 99.
           05 VALOR-Y    PIC 99.

       WORKING-STORAGE SECTION.

       01  GRAFICO.
           05 FILA OCCURS 41 TIMES INDEXED BY IDX-Y.
               10 COLUMNA OCCURS 80 TIMES INDEXED BY IDX-X.
                   15 PUNTO PIC X VALUE ' '.

       01  WS-FIN-ARCHIVO   PIC X VALUE 'N'.
       01  WS-LINEA         PIC X(80).
       01  WS-I             PIC 99.
       01  WS-J             PIC 99.
       01  WS-K             PIC 99.
       01  WS-AUX           PIC X(20).
       01  WS-AUX_1           PIC X(20).
       01  WS-AUX_2           PIC X(20).
       01  WS-AUX_3           PIC X(20).
       01  WS-AUX_4           PIC X(20).
       01  WS-AUX_5           PIC X(20).
       01  WS-AUX_6           PIC X(20).

       PROCEDURE DIVISION.
       INICIO.
           PERFORM INICIALIZAR-GRAFICO
           OPEN INPUT ARCHIVO-ENTRADA
           PERFORM UNTIL WS-FIN-ARCHIVO = 'S'
               READ ARCHIVO-ENTRADA
                   AT END
                       MOVE 'S' TO WS-FIN-ARCHIVO
                   NOT AT END
                       PERFORM MARCAR-PUNTO
               END-READ
           END-PERFORM
           CLOSE ARCHIVO-ENTRADA
           PERFORM MOSTRAR-GRAFICO
           STOP RUN.

       INICIALIZAR-GRAFICO.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 41
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 80
                   MOVE ' ' TO PUNTO(WS-I, WS-J)
               END-PERFORM
           END-PERFORM.

       MARCAR-PUNTO.
           IF VALOR-X >= 0 AND VALOR-X <= 80 AND
              VALOR-Y >= 0 AND VALOR-Y <= 41
               MOVE '@' TO PUNTO(VALOR-Y, VALOR-X)
           END-IF.

       MOSTRAR-GRAFICO.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 41
               MOVE SPACES TO WS-LINEA
               MOVE 1 TO WS-K
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 80
                   STRING PUNTO(WS-I,WS-J) DELIMITED BY SIZE
                          INTO WS-LINEA WITH POINTER WS-K
               END-PERFORM
               DISPLAY WS-LINEA
           END-PERFORM.
