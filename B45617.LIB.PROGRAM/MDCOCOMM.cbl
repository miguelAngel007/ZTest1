      ******************************************************************
      * 888888 21/02/24 AEC ADECUACIONES PARA GRABAR LOG DE MDCOCOMM   *
      * 102188 04/05/21 MRB COMPROBANTES ELECTRONICOS                  *
      * 903312 23/09/19 LHB ADICIONAR NUEVA FUNCION PARA VALIDAR R1    *
      * 900390 05/02/19 EBV NO AFECTAR PCL DE PRDS.:FOEC, FOEV, SPFO   *
      * 705275 14/11/17 EBV NO AFECTAR PCL DE PRDS.:FECO,FEVO,SPCE,SPOD*
      * 704657 28/09/17 EBV  AUTORIZACION DE CLIENTES R1              **
      * 601463 07/11/16 JMCH ACTUALIZAR MONTO SOBREGIRO PARA SPOM     **
      * 500417 11/11/15 RPTR IMPLANTACION DE LOG DB2 MDC_CABO         **
      * FO2515 07/04/10 CMLF VALIDACION DE CLIENTES ESPECIALES        **
      * FO2515 06/03/09 UCD  CAMBIA SEMILLA DE CAS                    **
      * FO2515 06/02/09 UCD  MANEJO DEL CODIGO DE PRODUCTO FCC        **
      * FO2515 21/10/08 UCD  CORRIGE 2530 TO 2520 Y RATIO EN ARBITRAJE**
      * FO2515 12/10/08 UCD  MUEVE TOTAL DE CARGOS A MEDIO PARA 430   **
      * FO2515 10/10/08 LALR SOPORTE A ENLACES CON COMMAREA DE TL91   **
      * FO2642 03/09/07 OLS CAMBIA LLAMADA A SAFE POR LLAMADA A TOLD  **
      * FO0628 28/11/05 OLS CAMBIA LLAMADA A SAFE POR LLAMADA A TOLD  **
      * FO0628 28/11/05 MDCLOG                                        **
      * FO2692 14/07/07 OLSA NEXT DAY REDUCCION TIEMPO (DB2)          *
      * FO2386 05/10/06 CEGB CAMBIA MODO TRANSF. IMP.MENORES A 15,000 **
      * UR0527 23/08/04 JPAZ LIQUIDAR OPERACIONES APROBADAS X TRANSF   *
      * FO1010 01/06/04 JPAZ ITF - EXONERACION TRX C/V Y TRANFERENCIAS *
      * UR0527 02/04/04 JFOF CONSIDERAR ITF PARA OPERACIONES 01 Y 02   *
      * UR0527 18/08/03 WPL  WI3-MONTO VALIDA SI ES NUMERIC            *
      * UR0489 18/08/03 JCTV CONDICION DE CABIO DE 3 X 1 LBTR          *
      * UR0484 12/08/03 JCTV ELIMINAR CONDICION POR N VERSION LBTR PRD *
      * FO0270 11/07/03 JAVP PARCHE PARA TRABAJAR CON VERSION LBTR PRD *
      ******************************************************************
*IDAPL*MDC
*OBJET******************************************************************
*OBJET*** INTERFACE DEL MDCLOG (PC) CON EL HOST PARA EFECTUAR OPERAC. **
*OBJET*** DE C/V, ABONOS, CARGOS, CAS Y LBTR                          **
*OBJET******************************************************************
       ID DIVISION.
      *============
       PROGRAM-ID.  MDCOCOMM.
      *AUTHOR.      INTERBANK.
       ENVIRONMENT DIVISION.
      *=====================
       DATA DIVISION.
      *==============
       WORKING-STORAGE SECTION.
      *------------------------
       77 WS-LENGREC                    PIC S9(4)  COMP.
       77 WK-RESP                       PIC S9(08) COMP VALUE +0.
       77 WK-MDCFTOLD                   PIC X(08)  VALUE 'MDCFTOLD'.
       77 WK-TRAN-TOLD                  PIC X(04)      VALUE '    '.
       77 WK-NUM-TOLD01                 PIC 9(07)      VALUE ZEROS.
       77 WK-NUM-TOLD02                 PIC 9(07)      VALUE ZEROS.
903312 77 SW-ERR-07                     PIC 9          VALUE 0.
903312* 77 II                            PIC 9(02)      VALUE ZEROS.
       77 I                             PIC 9(02)      VALUE ZEROS.
      * 77 W-I                           PIC 9(02)      VALUE ZEROS.
      * 77 W-J                           PIC 9(02)      VALUE ZEROS.
      * 77 WK-ULTIMO                     PIC X.
      * 77 WK-EXISTE                     PIC X.
      * 77 WK-ENCONTRADO                 PIC 9(01)      VALUE ZEROS.
       77 WK-HEAD-LENGTH                PIC S9(04) COMP.
       77 WK-HOST-LENGTH                PIC S9(04) COMP.
      * 77 WK-ITEM-TS                    PIC S9(04) COMP.
      * 77 WK-NROREG-C                   PIC S9(04) COMP.
      * 77 WK-ITEM                       PIC 9(04)      VALUE ZEROS.
      * 77 WK-FIN                        PIC 9(01)      VALUE ZEROS.
      * 77 WK-ERR-VERS                   PIC 9(01)      VALUE ZEROS.
      * 77 WK-ERR-LONG                   PIC 9(01)      VALUE ZEROS.
      * 77 WK-ERR-REG                    PIC 9(01)      VALUE ZEROS.
      * 77 WK-MAS-REG                    PIC 9(01)      VALUE ZEROS.
      * 77 WK-ERR-ENV                    PIC 9(01)      VALUE ZEROS.
      * 77 WK-POSIC                      PIC 9(04)      VALUE ZEROS.
      * 77 WK-SECUENCIA                  PIC X(02)      VALUE ZEROS.
       77 WK-APLICACION                 PIC X(04)      VALUE SPACES.
      * 77 WK-LONG-1                     PIC 9(04)      VALUE ZEROS.
      * 77 WK-NROREG                     PIC 9(04)      VALUE ZEROS.
      * 77 WK-REGTOT                     PIC 9(04)      VALUE ZEROS.
       77 WK-MONTO                      PIC 9(13)V9(02)
                                                       VALUE ZEROS.
       77 WK-MONTO-CAS                  PIC 9(13)V9(02)
                                                       VALUE ZEROS.
       77 WK-MONTO-SYS                  PIC 9(13)V9(02)
                                                       VALUE ZEROS.
       77 WK-COMISION-SYS               PIC 9(09)V9(04)
                                                       VALUE ZEROS.
       77 WK-TARIF                      PIC S9(03)V9(06) COMP-3.
      * 77 WK-FDL                        PIC X(03)      VALUE 'FDL'.
      * 77 WK-CICSID                     PIC X(04)      VALUE SPACES.
       77 WK-CODRET-CAS                 PIC X(01)      VALUE SPACES.
       77 WK-CODERR-CAS                 PIC 9(02)      VALUE ZEROS.
       77 WK-FLAG-RETEN                 PIC X(01)      VALUE SPACES.
       77 WK-FLAG-INI                   PIC X(01)      VALUE SPACES.
       77 WK-MENSAJE-CAS                PIC X(25)      VALUE SPACES.
       77 SW-EXISTE-CONSULTA-CAS        PIC 9(01)      VALUE ZEROS.
       77 WK-DETALLES-CONSULTA          PIC X(37)      VALUE SPACES.
       77 CR                            PIC 9(02)      VALUE ZEROS.
500417 77 WOR-IMP-CARGO-ABONO           PIC 9(13)V99   VALUE ZEROS.
500417 77 WOR-MONEDA-ORIGEN             PIC X(02)      VALUE SPACES.
500417 77 WOR-RC-TLD                    PIC X(02)      VALUE '00'.
500417 77 NUM-RC-TLD REDEFINES WOR-RC-TLD
                                        PIC 99.
500417 77 WOR-DE-RC-TLD                 PIC X(80)      VALUE SPACES.
500417 77 FLAG-LINK-OK                  PIC X(02)      VALUE 'NO'.
       77 FLAG-TERMINAL                 PIC X(02)      VALUE 'NO'.
       77 LCP-WS-ADDR-COMP              PIC S9(9) COMP.
       77 LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP USAGE POINTER.
       77 LCP-WS-ADDR-CHAR REDEFINES LCP-WS-ADDR-COMP
                                        PIC X(4).
      *---
       01 CODIGO-TRAN.                                                  00000820
          02 WS-X0430                   PIC   X(04)    VALUE '0430'.    00000830
          02 WS-X0832                   PIC   X(04)    VALUE '0832'.    00000830
          02 WS-X1832                   PIC   X(04)    VALUE '1832'.    00000830
          02 WS-X0892                   PIC   X(04)    VALUE '0892'.    00000830
          02 WS-X1892                   PIC   X(04)    VALUE '1892'.    00000830
          02 WS-X2310                   PIC   X(04)    VALUE '2310'.    00000830
          02 WS-X2320                   PIC   X(04)    VALUE '2320'.    00000830
          02 WS-X2410                   PIC   X(04)    VALUE '2410'.    00000840
          02 WS-X2420                   PIC   X(04)    VALUE '2420'.    00000840
          02 WS-X2510                   PIC   X(04)    VALUE '2510'.    00000830
          02 WS-X2520                   PIC   X(04)    VALUE '2520'.    00000830
          02 WS-X2610                   PIC   X(04)    VALUE '2610'.    00000840
          02 WS-X2620                   PIC   X(04)    VALUE '2620'.    00000840
          02 WS-X2900                   PIC   X(04)    VALUE '2900'.    00000840

500417 01 WOR-ESTRUCTURA-LOG-DB2.
500417    02 WOR-FEC-DB2.
500417       04 WOR-FEC-DB2-DD          PIC   X(02)    VALUE ZEROS.
500417       04 WOR-FEC-DB2-P1          PIC   X(01)    VALUE '.'.
500417       04 WOR-FEC-DB2-MM          PIC   X(02)    VALUE ZEROS.
500417       04 WOR-FEC-DB2-P2          PIC   X(01)    VALUE '.'.
500417       04 WOR-FEC-DB2-SA          PIC   X(04)    VALUE ZEROS.
      *
500417    02 WOR-HOR-DB2.
500417       04 WOR-HOR-DB2-HH          PIC   X(02)    VALUE ZEROS.
500417       04 WOR-HOR-DB2-P1          PIC   X(01)    VALUE '.'.
500417       04 WOR-HOR-DB2-MM          PIC   X(02)    VALUE ZEROS.
500417       04 WOR-HOR-DB2-P2          PIC   X(01)    VALUE '.'.
500417       04 WOR-HOR-DB2-SS          PIC   X(02)    VALUE ZEROS.
      *
500417    02 WOR-CONTROLES-CTA-IBK.
500417       05 WOR-CONTROL-BCO         PIC   9(02)    VALUE ZEROS.
500417       05 WOR-CONTROL-MON         PIC   9(03)    VALUE ZEROS.
500417       05 WOR-CONTROL-OFIC        PIC   9(03)    VALUE ZEROS.
500417       05 WOR-CONTROL-PROD        PIC   9(03)    VALUE ZEROS.
500417       05 WOR-NRO-CUENTA          PIC   X(10)    VALUE SPACES.
      *
500417 01 WOR-REG-SOLICITUDES-MDC.
500417    02 WOR-BC-CLAVE.
500417       04 WOR-BC-USUARIO          PIC   X(008)   VALUE SPACES.
500417       04 WOR-BC-NRO-SOLICITUD    PIC   9(010)   VALUE ZEROS.
500417    02 WOR-BC-FILLER              PIC   X(482)   VALUE SPACES.

       01 WI-INDICES.
          02 WI-I                       PIC S9(04) COMP.
          02 WI-J                       PIC S9(04) COMP.
      *
       01 WK-CLAVE.
          05 WK-USUARIO                 PIC X(8)       VALUE SPACES.
          05 WK-NRO-SOLICITUD           PIC 9(10)      VALUE ZEROS.
          05 WK-IND-OPERACION           PIC X(02)      VALUE SPACES.
       01 WK-FECHA.
          05 WK-SS                      PIC X(2)       VALUE ZEROS.
          05 WK-AA                      PIC X(2)       VALUE ZEROS.
          05 WK-MM                      PIC X(2)       VALUE ZEROS.
          05 WK-DD                      PIC X(2)       VALUE ZEROS.
       01 FORMATO                       PIC X(10)      VALUE
             'MM/DD/YYYY'.
       01 LCP-CURRENT-DATE-68.
          05 LCP-MONTH                  PIC X(2).
          05 FILLER                     PIC X          VALUE '/'.
          05 LCP-DAY1                   PIC X(2).
          05 FILLER                     PIC X          VALUE '/'.
          05 LCP-CENT                   PIC X(2).
          05 LCP-YEAR                   PIC X(2).
       01 WTIEMPO                       PIC X(6).

       01 WK-CTACTE.
          02 CTACTE-CTL1                PIC X(2).
          02 CTACTE-CTL2                PIC X(3).
          02 CTACTE-CTL3                PIC X(3).
          02 CTACTE-CTL4                PIC X(4).
          02 CTACTE-NRO                 PIC X(10).

       01 VARIABLES.
FO2515    02 WE-RESP                    PIC S9(08) COMP
                                                       VALUE ZEROS.
          02 WE-IND-MSG-CAS             PIC  X(1).
          02 WE-LONG-TRAMA              PIC  9(4).
          02 WK-RC                      PIC 9(02)      VALUE 0.
          02 WK-RETORNO-TOLD            PIC 9(02)      VALUE ZEROS.
          02 WK-FLAG-CODRET-COMIS       PIC X          VALUE 'N'.
          02 WK-RETORNO-TOLD-COMIS      PIC 9(02)      VALUE ZEROS.
          02 WK-RETORNO-RSYS            PIC 9(02)      VALUE ZEROS.
          02 WK-NRO-OPER-ITF            PIC 9(07)      VALUE ZEROS.
          02 WK-MENSAJE                 PIC X(60)      VALUE SPACES.
500417    02 WK-MENSAJER REDEFINES WK-MENSAJE.
500417       04 FILLER                  PIC X(46).
500417       04 WK-MENSAJE-TXT          PIC X(10).
500417       04 WK-MENSAJE-RC           PIC -Z99.
      *
          02 WK-MSG-RET-TOLD            PIC X(60)      VALUE SPACES.
FO        02 WK-TINX                    PIC X.
FO        02 WK-TINN REDEFINES WK-TINX  PIC 9.
FO        02 W-NUMERIC                  PIC 9(15)      VALUE ZEROS.
FO        02 W-NUMERIC-R REDEFINES W-NUMERIC
                                        PIC X(15).
FO        02 WE-ABSTIME                 PIC S9(15) COMP-3.
FO        02 WE-DATE                    PIC 9(08).
FO        02 WE-TIME                    PIC 9(06).
FO        02 WE-MSG-NOHAY               PIC X(70)      VALUE
FO              'MENSAJE NO EXISTE '.
FO        02 WE-MSG-ERRMSG              PIC X(70)      VALUE
FO              'ARCHIVO MENSAJES CON PROBLEMAS'.
FO        02 WE-MSG-ERRLOG              PIC X(70)      VALUE
FO              'REGISTRO DE LOG NO ENCONTRADO '.
FO    *
FO        02 WE-TRANSACCION             PIC X(01).
FO           88 TRANSOK                                VALUE '0'.
FO           88 ERROR-TRX                              VALUE '1'.
FO        02 WE-COD-ERROR               PIC 9(02).
FO        02 WE-COD-RETORNO             PIC 9(04).
FO        02 WE-RC-GEN                  PIC 9(02).
FO        02 WE-CERROR-APL              PIC X(03).
FO        02 WE-CONTROLES.
FO           05 WE-CONTROL-1            PIC 9(04).
FO           05 WE-CONTROL-2            PIC 9(04).
FO           05 WE-CONTROL-4            PIC 9(04).
FO        02 WE-NUMERO-CTA.
FO           05 WE-COD-TIENDA           PIC 9(03).
FO           05 WE-NRO-CUENTA           PIC 9(10).
          02 WK-TEXTO-TOLD-TOT.
             03 WK-TEXTO-BLANCO         PIC  X(8)      VALUE SPACES.
             03 WK-TEXTO-TOLD           PIC  X(80)     VALUE SPACES.
          02 WK-TEXTO                   PIC  X(80)     VALUE SPACES.
          02 WK-IDE-BLANKS              PIC  X(02)     VALUE SPACES.
          02 WK-CEROS-BINARIOS          PIC  X(02)     VALUE X'0000'.
          02 WK-NUM-OPER-SYSTEMAT       PIC  X(10).
          02 FILLER REDEFINES WK-NUM-OPER-SYSTEMAT.
             03 WK-NUM-OP-SYSTEMAT      PIC  9(10).
          02 WK-NUM-TOLD-DEF.
             03 WK-NUM-TOLD-CHAR        PIC  X(10).
          02 FILLER REDEFINES WK-NUM-TOLD-DEF.
             03 WK-NUM-TOLD             PIC  9(10).
          02 WK-NUM-OPER-DEF.
             03 WK-NUM-OPER-CHAR        PIC  X(10).
          02 FILLER REDEFINES WK-NUM-OPER-DEF.
             03 WK-NUM-OPER             PIC  9(10).
          02 WK-CODRET-DEF-SYS.
             03 WK-CODRET-CHAR-SYS      PIC  X(02).
          02 FILLER REDEFINES WK-CODRET-DEF-SYS.
             03 WK-CODRET-SYS           PIC  9(02).
          02 WK-CODRET-DEF.
             03 WK-CODRET-CHAR          PIC  X(02).
          02 FILLER REDEFINES WK-CODRET-DEF.
             03 WK-CODRET               PIC  9(02).
          02 WK-MONEDA-DEF.
             03 WK-MONEDA-CHAR          PIC  X(02).
          02 FILLER REDEFINES WK-MONEDA-DEF.
             03 WK-MONEDA               PIC  9(02).
          02 WK-CAJERO-DEF.
             03 WK-CAJERO               PIC  9(04).
          02 FILLER REDEFINES WK-CAJERO-DEF.
             03 WK-CAJERO-CHAR          PIC  X(04).
          02 WK-NOTA-DEF.
             03 WK-NOTA-CHAR            PIC  X(08).
          02 FILLER REDEFINES WK-NOTA-DEF.
             03 WK-NOTA                 PIC  9(08).
          02 WK-CODIGO-MEDIO-DEF.
             03 WK-CODIGO-MEDIO-CHAR    PIC X(02).
          02 FILLER REDEFINES WK-CODIGO-MEDIO-DEF.
             03 WK-CODIGO-MEDIO         PIC  9(02).
          02 WK-BANQUERO-DEF.
             03 WK-BANQUERO-CHAR        PIC  X(03).
          02 FILLER REDEFINES WK-BANQUERO-DEF.
             03 WK-BANQUERO             PIC  9(03).
          02 WK-CUENTA-DEF.
             03 WK-CUENTA-CHAR          PIC  X(13).
          02 FILLER REDEFINES WK-CUENTA-DEF.
             03 WK-CUENTA               PIC  9(13).
      * SEMILLAS PARA CONSULTAS CAS DESDE 30000001 HASTA 39999999.
          02 WX-ORD-CAS.
060309       04 WX-ORDC-SUFI            PIC  9(01)     VALUE 3.
060309       04 WX-ORDC-NOPE            PIC  9(07)     VALUE 3000000.
          02 WX-NUM-CAS REDEFINES WX-ORD-CAS
                                        PIC 9(08).
FO2692    02 WS-XFSUBIDA                PIC X(8).                          OEZA
FO2692    02 WS-NFSUBIDA REDEFINES WS-XFSUBIDA
                                        PIC 9(8).                          OEZA
       01 WX-NUMOPE                     PIC X(10)      VALUE ZEROS.
       01 FILLER.
          02 WS-TRANSAC.
             04 WX-TRANSDE              PIC  X(02).
             04 WS-TRANSIG              PIC  S9(01) COMP-3
                                                       VALUE +0.
          02 FILLER REDEFINES WS-TRANSAC.
             04 WS-TRANSDE              PIC  S9(04)V9 COMP-3.

       01 PUNTEROS.
          02 WP-PTR-GETMAIN-NEW         PIC S9(08) COMP.
          02 WP-ADDRESS-NEWDATA         PIC S9(08) COMP.
          02 WP-POINTER-NEWDATA REDEFINES WP-ADDRESS-NEWDATA
                USAGE POINTER.
          02 WP-ADDRESS-NEWD-CHAR REDEFINES WP-ADDRESS-NEWDATA.
             03 FILLER                  PIC X(04).

       01 REG-INPUT.
          02 WI-HOST-PARAMETROS.
             03 WI-CABECERA.
                04 WI-PARRAFO.
                   05 WI-CO-RED         PIC X(04).
                   05 WI-LONG           PIC X(04).
                04 WI-CODOPE            PIC X(02).
FO2515       03 WI-DATA                 PIC X(1990).
W18070       03 WI-DATA-ES REDEFINES WI-DATA.
W18070          04 WI-NUM-OCUR          PIC 9(02).
W18070          04 WI-USUARIO-C         PIC X(08).
W18070          04 WI-TABLA-BENE        PIC X(1980).
W18070          04 FILLER REDEFINES WI-TABLA-BENE OCCURS 20 TIMES.
W18070             05 WI-TIP-CLIE       PIC X(01).
W18070             05 WI-TIP-DOC        PIC X(02).
W18070             05 WI-NUM-DOC        PIC X(11).
W18070             05 WI-NOM-CLIE       PIC X(85).
             03 WI-DATA-01 REDEFINES WI-DATA.
                04 WI-CODRET            PIC X(02).
                04 WI-USUARIO           PIC X(08).
                04 WI-TRAN-TOLD         PIC X(04).
                04 WI-FLAG-SOBRE        PIC X(01).
                04 WI-FLAG-SUPER        PIC X(01).
                04 WI-NUM-OPER          PIC X(10).
                04 WI-NUM-CARGOS        PIC 9(03).
                04 WI-NUM-ABONOS        PIC 9(03).
                04 WI-MONTO-SOUS        PIC 9(17).
                04 WI-MONTO-SOUSD REDEFINES
                      WI-MONTO-SOUS     PIC 9(15)V99.
                04 WI-MONTO-ME          PIC 9(17).
                04 WI-MONTO-MED REDEFINES
                      WI-MONTO-ME       PIC 9(15)V99.
                04 WI-RATIO             PIC 9(11).
                04 WI-RATIOD REDEFINES
                      WI-RATIO          PIC 9(06)V9(05).
                04 WI-CLIENTE           PIC X(14).
                04 WI-TIPO-CLIENTE      PIC X(01).
                04 WI-IDEN-CLIENTE      PIC X(12).
                04 WI-NUM-TOLD          PIC 9(10).
                04 WI-CAJERO            PIC X(04).
                04 WI-MONEDA-ORI        PIC X(02).
                04 WI-MONEDA-DES        PIC X(02).
                04 WI-CODIGO-SUC        PIC X(03).
                04 WI-NOTA              PIC X(08).
                04 WI-ITF               PIC X(01).
                04 WI-TIPO-MEDIO        PIC X(01).
                04 WI-COD-MEDIO         PIC X(02).
                04 WI-MEDIO-SOUS        PIC 9(17).
                04 WI-MEDIO-SOUSD REDEFINES
                      WI-MEDIO-SOUS     PIC 9(15)V99.
                04 WI-MEDIO-ME          PIC 9(17).
                04 WI-MEDIO-MED REDEFINES WI-MEDIO-ME
                                        PIC 9(15)V99.
                04 WI-CUENTA            PIC X(13).
                04 FILLER REDEFINES WI-CUENTA.
                   06 WI-CUENTA-OFI     PIC X(03).
                   06 WI-CUENTA-NRO     PIC X(10).
                04 WI-CONTROLES         PIC X(12).
                04 WI-BANQUERO          PIC X(03).
                04 WI-MEDIO-MONEDA      PIC X(02).
                04 WI-CODRET-CAS        PIC X(01).
                04 WI-NUMOPE-CAS        PIC X(10).
                04 WI-FLAG-RETEN        PIC X(01).
060209          04 WI-PRODUCTO          PIC X(04).
                04 WI-IND-OPERACI       PIC X(02).
060209          04 WI-FILLER            PIC X(1771).
             03 WI-DATA-02 REDEFINES WI-DATA.
                04 WI2-CODRET           PIC X(02).
                04 WI2-USUARIO          PIC X(08).
                04 WI2-TRAN-TOLD        PIC X(04).
                04 WI2-FLAG-SOBRE       PIC X(01).
                04 WI2-FLAG-SUPER       PIC X(01).
                04 WI2-NUM-OPER         PIC X(10).
                04 WI2-MONTO            PIC 9(17).
                04 WI2-MONTO-D REDEFINES
                      WI2-MONTO         PIC 9(15)V99.
                04 WI2-CLIENTE          PIC X(14).
                04 WI2-TIPO-CLIENTE     PIC X(01).
                04 WI2-IDEN-CLIENTE     PIC X(12).
                04 WI2-NUM-TOLD01       PIC 9(10).
                04 WI2-CAJERO           PIC X(04).
                04 WI2-TIP-OPER         PIC X(01).
                04 WI2-BANCO            PIC X(02).
                04 WI2-MONEDA           PIC X(03).
                04 WI2-OFICINA          PIC X(03).
                04 WI2-PRODUCTO         PIC X(03).
                04 WI2-CUENTA           PIC X(10).
                04 WI2-NOTA             PIC X(08).
                04 WI2-CODRET-CAS       PIC X(01).
                04 WI2-NUMOPE-CAS       PIC X(10).
                04 WI2-FLAG-RETEN       PIC X(01).
                04 WI2-ITF              PIC X(01).
                04 WI2-COMISION         PIC 9(11).
                04 WI2-NUM-TOLD02       PIC X(10).
FO2515          04 WI2-FILLER           PIC X(1842).
             03 WI-DATA-03 REDEFINES WI-DATA.
                04 WI3-ACCION           PIC X(01).
                04 WI3-REFERENCIA       PIC X(11).
                04 WI3-CODOPER          PIC X(04).
                04 WI3-MODTRANS         PIC X(01).
                04 WI3-CTAORI           PIC X(06).
                04 WI3-CTAORIBAN        PIC X(04).
                04 WI3-SUBCTAORI        PIC X(08).
                04 WI3-CTAORIMON        PIC X(02).
                04 WI3-CTADES           PIC X(06).
                04 WI3-CTADESBAN        PIC X(04).
                04 WI3-SUBCTADES        PIC X(08).
                04 WI3-CTADESMON        PIC X(02).
                04 WI3-MONTO            PIC 9(17).
                04 WI3-INSTRUCCION      PIC X(80).
                04 WI3-CAJERO           PIC X(04).
                04 WI3-HORA             PIC 9(06).
                04 WI3-TERMINAL         PIC X(04).
FO2515          04 WI3-FILLER           PIC X(1822).
             03 WI-DATA-04 REDEFINES WI-DATA.
FO2515          04 FILLER               PIC X(1990).
             03 WI-DATA-05 REDEFINES WI-DATA.
                04 WI5-CODRET           PIC X(02).
                04 WI5-NUMOPE-CAS       PIC X(10).
FO2515          04 WI5-FILLER           PIC X(1978).
       01 REG-ESPECIAL.
          03 WX-DATA-01.
             04 WX-CODRET               PIC X(02).
             04 WX-USUARIO              PIC X(08).
             04 WX-TRAN-TOLD            PIC X(04).
             04 WX-FLAG-SOBRE           PIC X(01).
             04 WX-FLAG-SUPER           PIC X(01).
             04 WX-NUM-OPER             PIC X(10).
             04 WX-NUM-CARGOS           PIC 9(03).
             04 WX-NUM-ABONOS           PIC 9(03).
             04 WX-MONTO-SOUS           PIC 9(17).
             04 WX-MONTO-ME             PIC 9(17).
             04 WX-RATIO                PIC 9(11).
             04 WX-CLIENTE              PIC X(14).
             04 WX-TIPO-CLIENTE         PIC X(01).
             04 WX-IDEN-CLIENTE         PIC X(12).
             04 WX-NUM-TOLD             PIC 9(10).
             04 WX-CAJERO               PIC X(04).
             04 WX-MONEDA-ORI           PIC X(02).
             04 WX-MONEDA-DES           PIC X(02).
             04 WX-CODIGO-SUC           PIC X(03).
             04 WX-NOTA                 PIC X(08).
             04 WX-ITF                  PIC X(01).
             04 WX-TIPO-MEDIO           PIC X(01).
             04 WX-COD-MEDIO            PIC X(02).
             04 WX-MEDIO-SOUS           PIC 9(17).
             04 WX-MEDIO-ME             PIC 9(17).
             04 WX-CUENTA               PIC X(13).
             04 FILLER REDEFINES WX-CUENTA.
                06 WX-CUENTA-OFI        PIC X(03).
                06 WX-CUENTA-NRO        PIC X(10).
             04 WX-CONTROLES            PIC X(12).
             04 WX-BANQUERO             PIC X(03).
             04 WX-MEDIO-MONEDA         PIC X(02).
             04 WX-CODRET-CAS           PIC X(01).
             04 WX-NUMOPE-CAS           PIC X(10).
             04 WX-FLAG-RETEN           PIC X(01).
       01 REG-OUTPUT.
          02 WO-HOST-PARAMETROS.
             03 WO-CABECERA.
                04 WO-PARRAFO.
                   05 WO-CO-RED         PIC X(04).
                   05 WO-LONG           PIC 9(04).
                04 WO-CODOPE            PIC X(02).
FO2515       03 WO-DATA                 PIC X(1990).
903312       03 WO-DATA-ES REDEFINES WO-DATA.
W18070          04 WO-NUM-OCUR          PIC 9(02).
W18070          04 WO-USUARIO-C         PIC X(08).
903312          04 WO-TABLA-BENE-E      PIC X(1980).
903312          04 FILLER REDEFINES WO-TABLA-BENE-E OCCURS 20 TIMES.
903312             05 WO-TIP-CLIE-E     PIC X(01).
903312             05 WO-TIP-DOC-E      PIC X(02).
903312             05 WO-NUM-DOC-E      PIC X(11).
903312             05 WO-NOM-CLIE-E     PIC X(83).
903312             05 WO-CODRET-R1      PIC X(02).
             03 WO-DATA-01020305 REDEFINES WO-DATA.
                04 WO-CODRET            PIC X(02).
                04 WO-NUM-TOLD01        PIC X(10).
                04 WO-NUM-TOLD01R REDEFINES WO-NUM-TOLD01
                                        PIC 9(10).
                04 WO-CODERR-CAS        PIC 9(02).
                04 WO-CODRET-CAS        PIC X(01).
                04 WO-FLAG-RETEN        PIC X(01).
                04 WO-MENSAJE           PIC X(80).
                04 WO-MENSAJE-CAS       PIC X(25).
                04 WO-CODRET-COMIS      PIC X(02).
                04 WO-NUM-TOLD02        PIC X(10).
                04 WO-NUM-TOLD02R REDEFINES WO-NUM-TOLD02
                                        PIC 9(10).
FO2515          04 WO-FILLER            PIC X(1857).
             03 WO-DATA-04 REDEFINES WO-DATA.
                04 WO4-NUMREG           PIC 9(04).
                04 WO4-CODRET           PIC X(02).
                04 WO4-TABLA-OPER.
                   06 FILLER OCCURS 070 TIMES.
                      08 WO4-CODRET-CAS PIC X(01).
                      08 WO4-NUMOPE-CAS PIC X(10).
                      08 WO4-FLAG-RETEN PIC X(01).
                04 WO4-MENSAJE          PIC X(40).
FO2515          04 WO4-FILLER           PIC X(1104).

888888*INI
       01   REG-CAMFLOG.
            02 REG-FECHA-REAL         PIC X(8) VALUE SPACES.
            02 REG-HORAS-REAL         PIC X(6) VALUE SPACES.
            02 REG-LOG-MDCOCOMM       PIC X(2000).
            02 REG-FILLER             PIC X(2070).
888888 01   WC-VARIABLES.
888888      03  WC-LOG-LENGTH       PIC S9(4)  COMP VALUE +4084.
888888      03  WE-KEY-RBA          PIC 9(8)   COMP.
888888      03  WS-RESP             PIC S9(08) COMP.
888888      03  WS-RESP2            PIC S9(08) COMP.
888888 01  OUT-HEADER-TXT PIC X(6) VALUE SPACES.
888888*FIN

      *---
      *--- COMMAREA DE SUBRUTINAS IMPACS Y SAVINGS / TARIFARIO
      *---
           COPY SIMCDAT2.
           COPY SSTLACC.
           COPY TFRFTARI.
      *---
           COPY MDCXBAR.
           COPY MDCFPAR.
           COPY MDCFCAS.
           COPY CASCCA1.
           COPY CASCCA2.
           COPY MDCFTOLD.
FO2515*----COPY COMMAREA RUTINA CCER001
FO2515 01 WF-COMM-CCE.
FO2515     COPY CCEC001.
       01 SRMR031-DATA.
           COPY SRMCDOCU.
      *-------------------> COPY COMMAREA TRANSACCIONES AUTOMATICAS     00000580
FO0628      COPY  TLDCDD87.                                             00000100
      *-------------------> COPY MEDIOS DE PAGO                         00000580
FO0628      COPY  TLDCMPI.                                              00000100
      *-------------------> COPY COMMAREA + MEDIOS TOLD                 00000580
FO0628      COPY  TLDCDD05.                                             00000100
      *****  ARCHIVO DE MENSAJES
      *
           COPY TLDFDRMS.
FO2692*    COPY TLDFDDLG.
           COPY TLDCDD09.
           COPY TLDWDD02.
FO2692     EXEC SQL
FO2692        INCLUDE SQLCA
FO2692     END-EXEC.
FO2692     EXEC SQL
FO2692        INCLUDE DTLDDDLG
FO2692     END-EXEC.
      *
500417*    .....  BITACORA  PARA CARGOS Y ABONOS .....
500417     EXEC SQL INCLUDE DMDCLCAB   END-EXEC.
      *

       LINKAGE SECTION.
      *----------------*
FO2515 01 DFHCOMMAREA                   PIC X(2000).

       PROCEDURE DIVISION.
      *------------------*
           PERFORM  INICIAR.
           IF WK-RC EQUAL 0
W18070        IF WI-CODOPE = '06'
W18070           THEN
                 MOVE ZEROS TO I
W18070           MOVE SPACES TO WK-CODRET-CHAR
W18070           PERFORM VALIDA-DATOS-CLIE-ESPECIAL
W18070           INITIALIZE WO-HOST-PARAMETROS
W18070           MOVE 'MDCI' TO WO-CO-RED
W18070           MOVE 0023 TO WO-LONG
W18070           MOVE ZEROS TO WO-NUM-TOLD01R
W18070           MOVE LENGTH OF WO-CABECERA TO WK-HEAD-LENGTH
W18070           COMPUTE WK-HOST-LENGTH = 120 + WK-HEAD-LENGTH + 3
W18070
W18070           MOVE WI-CODOPE TO WO-CODOPE
W18070           IF WK-CODRET-CHAR = SPACES
W18070              THEN
                    MOVE ZEROS TO WK-CODRET-CHAR
W18070           END-IF
W18070           MOVE WK-CODRET-CHAR TO WO-CODRET
W18070           MOVE WK-MENSAJE TO WO-MENSAJE
W18070        ELSE
903312           IF WI-CODOPE = '07'
903312              THEN
903312              MOVE ZEROS TO I
903312              MOVE 0 TO SW-ERR-07
903312*                 MOVE SPACES TO WK-CODRET-CHAR
903312              PERFORM VALIDA-DATOS-CLIE-ESPECIAL
903312              MOVE WI-CABECERA TO WO-CABECERA
903312*                 MOVE WI-NUM-OCUR            TO  WO-NUM-OCUR
903312              MOVE WI-USUARIO-C TO WO-USUARIO-C
903312              MOVE 'MDCI' TO WO-CO-RED
903312*??               MOVE   0023                 TO  WO-LONG
903312*??               MOVE  ZEROS                 TO  WO-NUM-TOLD01R
903312              MOVE LENGTH OF WO-CABECERA TO WK-HEAD-LENGTH
903312              COMPUTE WK-HOST-LENGTH = 120 + WK-HEAD-LENGTH +     3
903312                 3                                                3
903312*                 IF SW-ERR-07 = 0                                3
903312              MOVE ZEROS TO WO-NUM-OCUR
903312*                 ELSE                                            3
903312*                    MOVE 02                  TO  WO-NUM-OCUR
903312*                 END-IF                                          3
903312*??               MOVE  WK-MENSAJE            TO  WO-MENSAJE
903312           ELSE
                    PERFORM PROCESAR-REGISTRO
903312           END-IF
              END-IF
              PERFORM TERMINAR.
           EXEC CICS
                RETURN
                END-EXEC.
           GOBACK.

W18070 VALIDA-DATOS-CLIE-ESPECIAL.
W18070*--------------------------*
W18070     PERFORM UNTIL I > WI-NUM-OCUR
W18070             ADD 1 TO I
W18070             IF WI-TIP-DOC(I) = '99'
W18070                THEN
                      MOVE WI-NUM-DOC(I)(2:10) TO WI-CLIENTE(5:10)
W18070                PERFORM CONSULTAR-DOCUMENTO
W18070                IF RM-RETURN-CODE = '00'
W18070                   PERFORM BUSCA-CCER001
W18070                END-IF
W18070             ELSE
W18070                IF WI-TIP-DOC(I) = '00'
W18070                   OR WI-TIP-DOC(I) > ZEROS
W18070                   THEN
                         INITIALIZE WF-COMM-CCE
W18070                   MOVE 'A' TO CCE-ACCESO
W18070                   MOVE WI-TIP-DOC(I) TO CCE-TIPO-DOC(1)
W18070                   MOVE WI-NUM-DOC(I) TO CCE-NRO-DOC(1)
W18070                   MOVE WI-NOM-CLIE(I) TO CCE-NOMB-PJ(1)
W18070                   PERFORM RUTINA-ALERTA
W18070                END-IF
W18070             END-IF
W18070     END-PERFORM.
W18070
       INICIAR.
      *--------*

           MOVE 00 TO WK-RETORNO-RSYS.
           MOVE SPACES TO WO-MENSAJE.
           MOVE 0 TO WK-RC

           CALL 'SISROFEC' USING LCP-CURRENT-DATE-68 FORMATO WTIEMPO.

           PERFORM RECIBIR-DATA
           INITIALIZE WO-HOST-PARAMETROS
           EXEC CICS
                ASKTIME
                ABSTIME(WE-ABSTIME)
                END-EXEC
           EXEC CICS
                FORMATTIME
                ABSTIME(WE-ABSTIME)
                YYYYMMDD(WE-DATE)
                TIME(WE-TIME)
                END-EXEC
           .
       RECIBIR-DATA.
      *------------*

           MOVE SPACES TO WI-HOST-PARAMETROS.

           MOVE LENGTH OF WI-HOST-PARAMETROS TO WS-LENGREC

FO2515     IF EIBCALEN NOT = ZEROS
FO2515        MOVE DFHCOMMAREA TO WI-HOST-PARAMETROS
888888        PERFORM  GRABACION-CAMFLOG
FO2515     ELSE
              EXEC CICS RECEIVE INTO (WI-HOST-PARAMETROS)
                   LENGTH(WS-LENGREC)
                   NOTRUNCATE
                   RESP(WK-RESP)
                   END-EXEC
              IF WK-RESP EQUAL DFHRESP(LENGERR)
                 MOVE 'EL' TO WK-CODRET-CHAR
                 MOVE '** ERROR RECEPCION MSG X HOST' TO WK-MENSAJE
                 MOVE 10 TO WK-RC
              END-IF
FO2515     END-IF
           .

       TERMINAR.
      **********

           IF WK-RC EQUAL 00
              PERFORM ENVIAR-MENSAJE-RESP

              MOVE 'OUTPUT' TO OUT-HEADER-TXT
              PERFORM GRABACION-CAMFLOG-OUT
           ELSE
              EXEC CICS
                   SYNCPOINT
                   ROLLBACK
                   END-EXEC
500417        IF FLAG-LINK-OK = 'SI'
500417           PERFORM 800-REGISTRO-BITACORA
500417        END-IF
              PERFORM ENVIAR-MENSAJE-ERR
           END-IF.

888888 GRABACION-CAMFLOG-OUT.
888888*-------------------*
888888*
888888     INITIALIZE REG-CAMFLOG.
888888     STRING LCP-CENT
888888            LCP-YEAR
888888            LCP-MONTH
888888            LCP-DAY1
888888        DELIMITED BY SIZE INTO REG-FECHA-REAL
888888
888888     MOVE OUT-HEADER-TXT          TO REG-HORAS-REAL
888888
888888     MOVE DFHCOMMAREA             TO  REG-LOG-MDCOCOMM
888888*
888888     EXEC CICS WRITE   DATASET ('CAMFLOG')
888888                       FROM    (REG-CAMFLOG)
888888                       LENGTH  (WC-LOG-LENGTH)
888888                       RBA
888888                       RIDFLD  (WE-KEY-RBA)
888888                       RESP  (WS-RESP)
888888                       RESP2 (WS-RESP2)
888888                       END-EXEC.
888888*

       PROCESAR-REGISTRO.
      *----------------*

           IF WI-CODOPE = '01'
              PERFORM PROCESAR-COMPRA-VENTA.
           IF WI-CODOPE = '02'
              PERFORM PROCESAR-TRANSFERENCIA.
           IF WI-CODOPE = '03'
              PERFORM PROCESAR-LBTR.
           IF WI-CODOPE = '04'
              PERFORM PROCESAR-CONSULTA.
           IF WI-CODOPE = '05'
              PERFORM PROCESAR-EXTORNO-CONS.
           PERFORM PREPARAR-RESPUESTA.


       PREPARAR-RESPUESTA.
      *-------------------*

           IF WI-CODOPE = '01' OR '02' OR '03'
              MOVE 0131 TO WO-LONG
              MOVE ZEROS TO WK-NUM-OPER
              IF WI-CODOPE = '01'
                 MOVE WK-NUM-TOLD01 TO WK-NUM-OPER
              END-IF
              IF WI-CODOPE = '02'
                 IF WK-RETORNO-RSYS NOT = 00
                    MOVE ZEROS TO WK-NUM-OPER
                 ELSE
                    MOVE WK-NUM-TOLD01 TO WK-NUM-OPER
                 END-IF
              END-IF
              MOVE WK-NUM-OPER TO WO-NUM-TOLD01
              IF ((WI-CODOPE = '02') AND (WI2-TIP-OPER = 'C') AND
                 (WI2-COMISION IS NUMERIC) AND (WI2-COMISION GREATER 0)
                 AND (WK-RETORNO-RSYS = 00))
                 MOVE WK-NUM-TOLD01 TO WO-NUM-TOLD01R
                 MOVE WK-NUM-TOLD02 TO WO-NUM-TOLD02R
              END-IF
           END-IF
           IF WI-CODOPE = '04' OR '05'
              IF WI-CODOPE = '04'
                 MOVE 0926 TO WO-LONG
              END-IF
              IF WI-CODOPE = '05'
                 MOVE 0131 TO WO-LONG
              END-IF
           END-IF.
           MOVE WI-CODOPE TO WO-CODOPE.
           MOVE 'MDCI' TO WO-CO-RED.
           IF WO-CODOPE = '01' OR WO-CODOPE = '02'
              IF WK-RETORNO-RSYS NOT = 00
                 MOVE ALL SPACES TO WO-NUM-TOLD01
                                    WO-NUM-TOLD02
                                    WO-CODRET
                                    WO-CODRET-COMIS
              ELSE
                 IF WK-FLAG-CODRET-COMIS EQUAL 'S'
                    MOVE WK-RETORNO-TOLD-COMIS TO WO-CODRET-COMIS
                 END-IF
                 MOVE WK-RETORNO-TOLD TO WO-CODRET
                 MOVE SPACES TO WK-TEXTO
              END-IF
              IF WI-TRAN-TOLD = WS-X2900
                 MOVE WK-TEXTO-TOLD TO WK-TEXTO
                 MOVE WK-TEXTO TO WO-MENSAJE
      * DEVUELVE RETENCION EN CASO DE EXTORNO
                 IF WK-RETORNO-TOLD = 00
                    MOVE LCP-DAY1 TO TT-DD
                    MOVE LCP-MONTH TO TT-MM
                    MOVE LCP-CENT TO TT-SS
                    MOVE LCP-YEAR TO TT-AA
                    IF WO-CODOPE = '01'
                       MOVE WI-NUMOPE-CAS TO TT-NUMOPE
                    ELSE
                       MOVE WI2-NUMOPE-CAS TO TT-NUMOPE
                    END-IF
                    PERFORM LEE-UPDATE-MDCFCAS
                    IF CR = DFHRESP(NORMAL)
                       IF TT-SIN-RETENCION AND
                          (TT-CODRET-CAS = 'A' OR 'C')
                          STRING '03'
                                 TT-MONEDA
                                 TT-OFICINA
                                 TT-PRODUCTO
                                 TT-CUENTA
                             DELIMITED BY SIZE INTO WK-CTACTE
                          MOVE TT-MONTO TO WK-MONTO
                          MOVE '0' TO CASCCA2-FLAG-RET-TRAMITE
                          PERFORM PROCESA-RETENCION-IM
                          MOVE '1' TO TT-RETENCION
                                      WO-FLAG-RETEN
                          MOVE TT-CODRET-CAS TO WO-CODRET-CAS
                          PERFORM REGRABA-MDCFCAS
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 IF WK-RETORNO-TOLD NOT EQUAL 00
                    MOVE WK-TEXTO-TOLD TO WK-TEXTO
                    MOVE WK-TEXTO TO WO-MENSAJE
                    IF WO-CODOPE = '01'
                       IF WK-RETORNO-TOLD = 08 AND
                          WI-TIPO-MEDIO = 'C' AND
                          WI-COD-MEDIO = '02' AND
                          WI-CODRET-CAS = ' '
                          IF SW-EXISTE-CONSULTA-CAS EQUAL 1
                             MOVE WK-CODRET-CAS TO WO-CODRET-CAS
                             MOVE WK-FLAG-RETEN TO WO-FLAG-RETEN
                          ELSE
                             PERFORM GENERA-TRX-CAS
                          END-IF
                       ELSE
                          MOVE WK-CODERR-CAS TO WO-CODERR-CAS
                          MOVE WK-MENSAJE-CAS TO WO-MENSAJE-CAS
                          MOVE WK-FLAG-RETEN TO WO-FLAG-RETEN
                          IF WK-CODRET-CAS NOT = WI-CODRET-CAS AND
                             WK-CODRET-CAS NOT = ' '
                             MOVE WK-CODRET-CAS TO WO-CODRET-CAS
                          ELSE
                             MOVE WI-CODRET-CAS TO WO-CODRET-CAS
                          END-IF
                       END-IF
                    ELSE
                       IF WK-RETORNO-TOLD = 08 AND
                          WI2-TIP-OPER = 'C' AND
                          WI2-PRODUCTO = '001' AND
                          WI2-CODRET-CAS = ' '
                          IF SW-EXISTE-CONSULTA-CAS EQUAL 1
                             MOVE WK-CODRET-CAS TO WO-CODRET-CAS
                             MOVE WK-FLAG-RETEN TO WO-FLAG-RETEN
                          ELSE
                             PERFORM GENERA-TRX-CAS
                          END-IF
                       ELSE
                          MOVE WK-CODERR-CAS TO WO-CODERR-CAS
                          MOVE WK-MENSAJE-CAS TO WO-MENSAJE-CAS
                          MOVE WK-FLAG-RETEN TO WO-FLAG-RETEN
                          IF WK-CODRET-CAS NOT = WI2-CODRET-CAS AND
                             WK-CODRET-CAS NOT = ' '
                             MOVE WK-CODRET-CAS TO WO-CODRET-CAS
                          ELSE
                             MOVE WI2-CODRET-CAS TO WO-CODRET-CAS
                          END-IF
                       END-IF
                    END-IF
                 ELSE
      * LEVANTA LA RETENCION SI CARGO ES OK
                    IF WK-RETORNO-TOLD = 00 AND WK-CODRET-CAS = 'A'
                       MOVE LCP-DAY1 TO TT-DD
                       MOVE LCP-MONTH TO TT-MM
                       MOVE LCP-CENT TO TT-SS
                       MOVE LCP-YEAR TO TT-AA
                       IF WO-CODOPE = '01'
                          MOVE WI-NUMOPE-CAS TO TT-NUMOPE
                       ELSE
                          MOVE WK-MONTO-CAS TO WK-MONTO
                          MOVE WI2-NUMOPE-CAS TO TT-NUMOPE
                       END-IF
                       PERFORM LEE-UPDATE-MDCFCAS
                       IF CR = DFHRESP(NORMAL)
                          IF TT-CON-RETENCION
                             MOVE '1' TO CASCCA2-FLAG-RET-TRAMITE
                             PERFORM PROCESA-RETENCION-IM
                             MOVE '0' TO TT-RETENCION
                                         WK-FLAG-RETEN
                                         WO-FLAG-RETEN
                             PERFORM REGRABA-MDCFCAS
                          END-IF
                       END-IF
                    END-IF
                    IF WO-CODOPE = '01'
                       MOVE WK-FLAG-RETEN TO WO-FLAG-RETEN
                       IF WK-CODRET-CAS NOT = WI-CODRET-CAS AND
                          WK-CODRET-CAS NOT = ' '
                          MOVE WK-CODRET-CAS TO WO-CODRET-CAS
                       ELSE
                          MOVE WI-CODRET-CAS TO WO-CODRET-CAS
                       END-IF
                    ELSE
                       MOVE WK-FLAG-RETEN TO WO-FLAG-RETEN
                       IF WK-CODRET-CAS NOT = WI2-CODRET-CAS AND
                          WK-CODRET-CAS NOT = ' '
                          MOVE WK-CODRET-CAS TO WO-CODRET-CAS
                       ELSE
                          MOVE WI2-CODRET-CAS TO WO-CODRET-CAS
                       END-IF
                    END-IF
                    MOVE WK-TEXTO TO WO-MENSAJE
                 END-IF
              END-IF
              IF (
                 (WK-RETORNO-RSYS = 09 OR 08)
                 AND (WI-CODOPE = '02')
                 AND (WI2-TIP-OPER = 'C')
                 AND (WI2-CODRET-CAS = ' ')
                 )
                 MOVE ALL SPACES TO WO-NUM-TOLD01
                                    WO-NUM-TOLD02
                                    WO-CODRET
                                    WO-CODRET-COMIS
                 MOVE 'COD.CTA/SALDO NO DISPONIBLE EN SYSTEMATIC '
                    TO WO-MENSAJE
                 MOVE ZEROS TO WO-NUM-TOLD01R
                 MOVE '08' TO WO-CODRET
              END-IF
           ELSE
              IF WO-CODOPE = '03'
                 IF XBA-ERROR-CODIGO = '0' OR XBA-ERROR-CODIGO = " "
                    MOVE '00' TO WO-CODRET
                    MOVE XBA-ERROR-DESCRIP TO WO-MENSAJE
                 ELSE
                    MOVE XBA-ERROR-CODIGO TO WO-CODRET
                    MOVE XBA-ERROR-DESCRIP TO WO-MENSAJE
                 END-IF
              END-IF
           END-IF.
           MOVE LENGTH OF WO-CABECERA TO WK-HEAD-LENGTH.
           IF WO-CODOPE NOT = '04'
      *       COMPUTE WK-HOST-LENGTH = 120 + WK-HEAD-LENGTH + 3
              COMPUTE WK-HOST-LENGTH = 130 + WK-HEAD-LENGTH + 3
           ELSE
              COMPUTE WK-HOST-LENGTH = 926 + WK-HEAD-LENGTH + 3
           END-IF.
       ENVIAR-MENSAJE-RESP.
      *********************
FO2515     IF EIBCALEN NOT = ZEROS
FO2515        MOVE WO-HOST-PARAMETROS TO DFHCOMMAREA
FO2515     ELSE
              EXEC CICS SEND FROM (WO-HOST-PARAMETROS)
                   LENGTH(WK-HOST-LENGTH)
                   FMH
                   ERASE
                   RESP(WK-RESP)
                   END-EXEC
              IF WK-RESP EQUAL DFHRESP(TERMERR)
                 MOVE 'ET' TO WK-CODRET-CHAR
                 MOVE '** ERROR TERMINAL            ' TO WK-MENSAJE     JE
                 MOVE 10 TO WK-RC
              END-IF
FO2515     END-IF
           .

       ENVIAR-MENSAJE-ERR.
      *------------------*

           INITIALIZE WO-HOST-PARAMETROS.
           MOVE 'MDCI' TO WO-CO-RED.
           MOVE 0023 TO WO-LONG.
           MOVE ZEROS TO WO-NUM-TOLD01R.
           MOVE LENGTH OF WO-CABECERA TO WK-HEAD-LENGTH.
           COMPUTE WK-HOST-LENGTH = 120 + WK-HEAD-LENGTH + 3.

           MOVE WI-CODOPE TO WO-CODOPE.
           MOVE WK-CODRET-CHAR TO WO-CODRET.
           MOVE WK-MENSAJE TO WO-MENSAJE.

W18070     IF WK-CODRET-CHAR = 'R1'
W18070        AND WK-RC = 10
W18070        THEN
              PERFORM RETORNAR
W18070     ELSE
FO2515        IF EIBCALEN NOT = ZEROS
FO2515           MOVE WO-HOST-PARAMETROS TO DFHCOMMAREA
FO2515        ELSE
                 EXEC CICS SEND FROM (WO-HOST-PARAMETROS)
                      LENGTH(WK-HOST-LENGTH)
                      FMH
                      ERASE
                      RESP(WK-RESP)
                      END-EXEC

                 IF WK-RESP EQUAL DFHRESP(TERMERR)
                    EXEC CICS
                         ABEND
                         ABCODE('TERM')
                         RESP(WK-RESP)
                         END-EXEC
                 END-IF
FO2515        END-IF
W18070     END-IF

           MOVE 'OUTERR' TO OUT-HEADER-TXT
           PERFORM GRABACION-CAMFLOG-OUT
           .

       PROCESAR-COMPRA-VENTA.
      *----------------------*
           MOVE WI-TRAN-TOLD TO WK-TRAN-TOLD
           EVALUATE WI-TRAN-TOLD
           WHEN WS-X2310
           WHEN WS-X2410
           WHEN WS-X2510
           WHEN WS-X2610
                MOVE WI-USUARIO TO WK-USUARIO
                MOVE WI-NUM-OPER TO WK-NRO-SOLICITUD
                MOVE WI-IND-OPERACI TO WK-IND-OPERACION
                MOVE 'S' TO WK-FLAG-INI
500417          PERFORM LEER-SOLICITUD
                IF WK-RC EQUAL 0
                   IF WK-RESP EQUAL DFHRESP(NOTFND)
                      PERFORM CREAR-SOLICITUD
                   ELSE
                      IF BC-IND-EXTO EQUAL 1
500417                   PERFORM LEER-SOLICITUD-UPD
                         PERFORM ELIMINAR-SOLICITUD
                         PERFORM CREAR-SOLICITUD
                      ELSE
                         MOVE 'EL' TO WK-CODRET-CHAR
                         MOVE '** TRANSACCION DEBE SER EXTORNADA   '
                            TO WK-MENSAJE
                         MOVE 10 TO WK-RC
                      END-IF
                   END-IF
                END-IF
                IF WK-RC EQUAL 0
                   MOVE WI-NUM-OPER TO WK-NUM-TOLD01
                ELSE
                   MOVE ZEROS TO WK-NUM-TOLD01
                END-IF
           WHEN WS-X2320
           WHEN WS-X2420
           WHEN WS-X2520
           WHEN WS-X2620
                MOVE WI-USUARIO TO WK-USUARIO
                MOVE WI-NUM-OPER TO WK-NRO-SOLICITUD
                MOVE WI-IND-OPERACI TO WK-IND-OPERACION
500417          PERFORM LEER-SOLICITUD
                IF WK-RC EQUAL 0
                   IF BC-IND-EXTO EQUAL 1
                      MOVE 'ER' TO WK-CODRET-CHAR
                      MOVE '** TRANSACCION ESTA EXTORNADA '
                         TO WK-MENSAJE
                      MOVE 10 TO WK-RC
                   END-IF
                END-IF
                IF WK-RC EQUAL 0
                   IF BC-IND-COMPLETADA EQUAL 1
                      MOVE 'ER' TO WK-CODRET-CHAR
                      MOVE '** OPERACION ESTABA COMPLETADA  '
                         TO WK-MENSAJE
                      MOVE 10 TO WK-RC
                   END-IF
                END-IF
                IF WK-RC EQUAL 0
                   IF BC-NUM-CARGOS-ABONOS EQUAL 20
                      MOVE 'ER' TO WK-CODRET-CHAR
                      MOVE '** NUMERO DE CARGOS/ABONOS SUPERO 20 '
                         TO WK-MENSAJE
                      MOVE 10 TO WK-RC
                   END-IF
                END-IF
                IF WK-RC EQUAL 0
                   PERFORM PROCESAR-CV-CARGOS-ABONOS
                END-IF
           WHEN WS-X2900
                MOVE WI-USUARIO TO WK-USUARIO
                MOVE WI-NUM-TOLD TO WK-NRO-SOLICITUD
                MOVE WI-IND-OPERACI TO WK-IND-OPERACION
500417          PERFORM LEER-SOLICITUD
                IF WK-RC EQUAL 0
                   IF BC-IND-EXTO EQUAL 1
                      MOVE 'ER' TO WK-CODRET-CHAR
                      MOVE '** TRANSACCION YA FUE EXTORNADA     '
                         TO WK-MENSAJE
                      MOVE 10 TO WK-RC
                   ELSE
                      PERFORM PROCESAR-CV-EXT-MASIVO
                   END-IF
                END-IF
           END-EVALUATE
           .
       PROCESAR-CV-CARGOS-ABONOS.
      ****************************
           IF WI-TRAN-TOLD EQUAL WS-X2320 OR WS-X2520
              IF WI-TIPO-MEDIO EQUAL 'C'
                 MOVE WI-MEDIO-MED TO WK-MONTO
              ELSE
                 MOVE WI-MEDIO-SOUSD TO WK-MONTO
              END-IF
           ELSE
              IF WI-TIPO-MEDIO EQUAL 'C'
                 MOVE WI-MEDIO-SOUSD TO WK-MONTO
              ELSE
                 MOVE WI-MEDIO-MED TO WK-MONTO
              END-IF
           END-IF
           PERFORM PROCESAR-CV-CARGOS-ABONOS-DET
           IF WK-RC EQUAL 0
              AND WK-RETORNO-TOLD EQUAL 0
              AND WK-RETORNO-TOLD-COMIS EQUAL 0
              COMPUTE BC-NUM-CARGOS-ABONOS = BC-NUM-CARGOS-ABONOS + 1
              MOVE BC-NUM-CARGOS-ABONOS TO I
              MOVE TLDCDD87-CTRANSID TO BC-COD-TRANS(I)
              MOVE WK-NUM-TOLD01 TO BC-NRO-TOLD(I)
              IF WI-TIPO-MEDIO EQUAL 'A'
                 COMPUTE BC-ACU-ABONOS = BC-ACU-ABONOS +
                    WI-MEDIO-SOUSD
                 IF BC-ACU-ABONOS EQUAL BC-MONTO-SOUS
                    MOVE 1 TO BC-IND-COMPLETADA
060209              IF WI-PRODUCTO NOT EQUAL 'FECD' AND
060209                 WI-PRODUCTO NOT EQUAL 'FEVD' AND
070527                 WI-PRODUCTO NOT EQUAL 'FECO' AND
070527                 WI-PRODUCTO NOT EQUAL 'FEVO' AND
070527                 WI-PRODUCTO NOT EQUAL 'SPCE' AND
070527                 WI-PRODUCTO NOT EQUAL 'SPOD' AND
900390                 WI-PRODUCTO NOT EQUAL 'FOEC' AND
900390                 WI-PRODUCTO NOT EQUAL 'FOEV' AND
900390                 WI-PRODUCTO NOT EQUAL 'SPFO' AND
900390                 WI-PRODUCTO NOT EQUAL 'FVED' AND
900390                 WI-PRODUCTO NOT EQUAL 'FCED' AND
900390                 WI-PRODUCTO NOT EQUAL 'SFOD' AND
900390                 WI-PRODUCTO NOT EQUAL 'SPIR' AND
900390                 WI-PRODUCTO NOT EQUAL 'SPOP'

                       PERFORM PROCESAR-CV-COMPRA-VENTA
060209              END-IF
                    IF WK-RC EQUAL 0
                       IF TLDCDD87-RC-GEN = 00
500417                    PERFORM PROCESO-ACTUALIZA-MDCFTOLD
                       END-IF
                    END-IF
                 ELSE
                    IF BC-ACU-ABONOS GREATER BC-MONTO-SOUS
                       MOVE 'EL' TO WK-CODRET-CHAR
                       MOVE '** MONTO DE ABONOS MAYOR QUE CARGOS '
                          TO WK-MENSAJE
                       MOVE 10 TO WK-RC
                    ELSE
500417                 PERFORM PROCESO-ACTUALIZA-MDCFTOLD
                    END-IF
                 END-IF
              ELSE
500417           PERFORM PROCESO-ACTUALIZA-MDCFTOLD
              END-IF
           END-IF
           .
       PROCESAR-CV-COMPRA-VENTA.
      **************************
           MOVE WI-DATA-01 TO WX-DATA-01
           MOVE '430' TO WK-TRAN-TOLD
           MOVE BC-CLIENTE TO WI-CLIENTE
           MOVE BC-TIPO-CLIENTE TO WI-TIPO-CLIENTE
           MOVE BC-IDEN-CLIENTE TO WI-IDEN-CLIENTE
           MOVE BC-MONEDA-ORI TO WI-MONEDA-ORI
           MOVE BC-MONEDA-DES TO WI-MONEDA-DES
           MOVE BC-CODIGO-SUC TO WI-CODIGO-SUC
           MOVE BC-MONTO-SOUS TO WI-MONTO-SOUSD
           MOVE BC-MONTO-ME TO WI-MONTO-MED
FO2515     MOVE BC-MONTO-SOUS TO WI-MEDIO-SOUSD
FO2515     MOVE BC-MONTO-ME TO WI-MEDIO-MED
           MOVE BC-RATIO TO WI-RATIOD
           PERFORM PROCESAR-CV-CARGOS-ABONOS-DET
           MOVE WX-DATA-01 TO WI-DATA-01
           COMPUTE BC-NUM-CARGOS-ABONOS = BC-NUM-CARGOS-ABONOS + 1
           MOVE BC-NUM-CARGOS-ABONOS TO I
           MOVE '430' TO BC-COD-TRANS(I)
           MOVE WK-NUM-TOLD01 TO BC-NRO-TOLD(I)
           .
       PROCESAR-CV-EXT-MASIVO.
      *************************
           PERFORM VARYING I FROM 1 BY 1
              UNTIL I GREATER BC-NUM-CARGOS-ABONOS
              OR (WK-RC NOT EQUAL 0
              AND WK-RETORNO-TOLD NOT EQUAL 0)
                   MOVE BC-NRO-TOLD(I) TO WI-NUM-TOLD
                   MOVE BC-COD-TRANS(I) TO WK-TRAN-TOLD
                   PERFORM PROCESAR-COMPRA-VENTA-DET-EXT
                   PERFORM RUT-ENLAZAR-CV-TLDOD87
           END-PERFORM
           IF WK-RC EQUAL 0
              AND WK-RETORNO-TOLD EQUAL 0
500417        PERFORM LEER-SOLICITUD-UPD
              MOVE 1 TO BC-IND-EXTO
              PERFORM REGRABAR-SOLICITUD
           END-IF
           .
       CREAR-SOLICITUD.
      ****************
           INITIALIZE REG-SOLICITUDES-MDC
           MOVE WK-CLAVE TO BC-CLAVE
           COMPUTE BC-NUM-CA-XPROC =
              WI-NUM-ABONOS +
              WI-NUM-CARGOS +
              1
           MOVE WI-CLIENTE TO BC-CLIENTE
           MOVE WI-TIPO-CLIENTE TO BC-TIPO-CLIENTE
           MOVE WI-IDEN-CLIENTE TO BC-IDEN-CLIENTE
           MOVE WI-MONEDA-ORI TO BC-MONEDA-ORI
           MOVE WI-MONEDA-DES TO BC-MONEDA-DES
           MOVE WI-CODIGO-SUC TO BC-CODIGO-SUC
           MOVE WI-MONTO-SOUSD TO BC-MONTO-SOUS
           MOVE WI-MONTO-MED TO BC-MONTO-ME
           MOVE WI-RATIOD TO BC-RATIO
           PERFORM GRABAR-SOLICITUD
           .
       PROCESAR-CV-CARGOS-ABONOS-DET.
      *-----------------------------*
           IF WK-TRAN-TOLD = '2320' OR '2420' OR '2520' OR '2620' OR
              '2900'
              PERFORM RUT-HEADER-CV-TLDCDD87
              PERFORM FORMATEAR-CV-COMPRA-VENTA
           ELSE
              PERFORM FORMATEAR-CV-TRX-FINAL
           END-IF
           PERFORM RUT-ENLAZAR-CV-TLDOD87.
           MOVE TLDCDD87-CLOG TO WK-NUM-TOLD01
           MOVE TLDCDD87-RC-GEN TO WK-RETORNO-TOLD
           .
      *
       PROCESAR-COMPRA-VENTA-DET-EXT.
      *-----------------------------*
           INITIALIZE TLDCDD87.
           MOVE 'TLTM' TO TLDCDD87-CTRANS-DESP
      *
           MOVE WK-TRAN-TOLD TO TLDCDD87-CTRANSID

      *        SIN LLAVE DE AUTORIZACION
           MOVE 'N' TO TLDCDD87-GAUTORIZ
           MOVE 'N' TO TLDCDD87-GSECONDT
           MOVE 0 TO TLDCDD87-QFIRMAS1
           MOVE 0 TO TLDCDD87-QFIRMAS2
           IF TLDCDD87-CTRANSID = '430'
              MOVE 2 TO TLDCDD87-QSEQ
           ELSE
              MOVE 1 TO TLDCDD87-QSEQ
           END-IF
           MOVE 0 TO TLDCDD87-QMEDIOS
      *
           MOVE 'N' TO TLDCDD87-GVUELTO
           MOVE 'N' TO TLDCDD87-GITF
      *
           MOVE ZEROS TO TLDCDD87-CLOG-TERM
           MOVE WE-DATE TO TLDCDD87-FTERM
           MOVE WE-TIME TO TLDCDD87-HTERM
           MOVE WE-DATE TO TLDCDD87-FPOSTEO
FO         MOVE WI-USUARIO TO TLDCDD87-CUSERID                          00001590
FO         MOVE 'BA' TO TLDCDD87-CRED
OJOOO      MOVE 981 TO TLDCDD87-CAGENCIA
FO         MOVE 0 TO TLDCDD87-CSAGENCIA
FO         MOVE WI-NUM-TOLD TO TLDCDD87-CLOG-EXT
FO         MOVE ZEROS TO TLDCDD87-CLOG-TERM-EXT
FO         MOVE 'N' TO TLDCDD87-GLAVADO
FO         MOVE 'N' TO TLDCDD87-GPINPAD
FO         MOVE 'S' TO TLDCDD87-GEXT
FO         MOVE 'N' TO TLDCDD87-GCAS
FO         MOVE ZERO TO TLDCDD87-CVERSION
           .
       VALIDAR-CONSULTA-CAS.
      *---------------------*
           MOVE 0 TO WE-IND-MSG-CAS
      *
           MOVE LENGTH OF TLDCDD87-DATOS TO WE-LONG-TRAMA
           COMPUTE WE-LONG-TRAMA = WE-LONG-TRAMA - 6
      *
           PERFORM VARYING WI-I FROM 1 BY 1 UNTIL
              WI-I > WE-LONG-TRAMA OR
              WE-IND-MSG-CAS = '1'
                   EVALUATE TLDCDD87-DATOS(WI-I:6)
                   WHEN 'IM0126'
                        MOVE '1' TO WE-IND-MSG-CAS
                        MOVE 08 TO TLDCDD87-RC-GEN
                   WHEN OTHER
                        MOVE '0' TO WE-IND-MSG-CAS
                   END-EVALUATE
           END-PERFORM.
           MOVE 0 TO WI-J
           IF WE-IND-MSG-CAS = '1'
              MOVE SPACES TO WK-TEXTO-TOLD
              COMPUTE WI-I = WI-I - 1
              PERFORM VARYING WI-I FROM WI-I BY 1 UNTIL
                 WI-I > WE-LONG-TRAMA OR
                 WI-J > LENGTH OF WK-TEXTO-TOLD OR
                 TLDCDD87-DATOS(WI-I:1) > X'F9'
                      ADD 1 TO WI-J
                      MOVE TLDCDD87-DATOS(WI-I:1) TO WK-TEXTO-TOLD
                         (WI-J:1)
              END-PERFORM
           END-IF
           .
       RUT-HEADER-CV-TLDCDD87.
      *----------------------*
      *                                                                 00001850
           INITIALIZE TLDCDD87.
           PERFORM VARYING WI-I FROM 1 BY 1
              UNTIL WI-I > 20
                   MOVE SPACES TO TLDCDD87-EFLD(WI-I)
                   MOVE ZEROS TO TLDCDD87-FLD(WI-I)
           END-PERFORM
      *
           MOVE 'TLTM' TO TLDCDD87-CTRANS-DESP
           MOVE 1 TO TLDCDD87-QSEQ
           MOVE 0 TO TLDCDD87-QMEDIOS
      *
           IF WK-TRAN-TOLD EQUAL '430'
      *    TRANSACCION DE COMPRA VENTA FINAL
              MOVE 2 TO TLDCDD87-QSEQ
              MOVE '430' TO TLDCDD87-CTRANSID
              MOVE 1 TO TLDCDD87-QMEDIOS
704657        IF WI-FLAG-SUPER = '1'
704657           MOVE 'S' TO TLDCDD87-GAUTORIZ
704657           MOVE 'S' TO TLDCDD87-GSECONDT
704657        ELSE
704657           MOVE 'N' TO TLDCDD87-GAUTORIZ
704657           MOVE 'N' TO TLDCDD87-GSECONDT
704657        END-IF
           ELSE
              EVALUATE WI-CONTROLES(9:4)
              WHEN '0001'
      *       SI CUENTA CORRIENTE
                   IF WI-TIPO-MEDIO = 'C'
                      IF WI-CUENTA EQUAL SPACES
                         MOVE '0222' TO TLDCDD87-CTRANSID
                      ELSE
                         MOVE '0892' TO TLDCDD87-CTRANSID
                      END-IF
                   ELSE
                      IF WI-CUENTA EQUAL SPACES
                         MOVE '0211' TO TLDCDD87-CTRANSID
                      ELSE
                         MOVE '0832' TO TLDCDD87-CTRANSID
                      END-IF
                   END-IF
              WHEN OTHER
      *       SI CUENTA DE AHORROS
                   IF WI-TIPO-MEDIO = 'C'
                      IF WI-CUENTA EQUAL SPACES
                         MOVE '0222' TO TLDCDD87-CTRANSID
                      ELSE
                         MOVE '1892' TO TLDCDD87-CTRANSID
                      END-IF
                   ELSE
                      IF WI-CUENTA EQUAL SPACES
                         MOVE '0211' TO TLDCDD87-CTRANSID
                      ELSE
                         MOVE '1832' TO TLDCDD87-CTRANSID
                      END-IF
                   END-IF
              END-EVALUATE
      *

      *    SI TRANSACCION ES AUTORIZADA
              IF WI-FLAG-SUPER = '1'
      *    ACEPTA SOBREGIROS
704657           MOVE 'S' TO TLDCDD87-GAUTORIZ
704657*           MOVE 'R'                  TO  TLDCDD87-GAUTORIZ
                 MOVE 'S' TO TLDCDD87-GSECONDT
                 MOVE '9999' TO TLDCDD87-CSUPERVID
              ELSE
      *        SIN LLAVE DE AUTORIZACION
                 MOVE SPACES TO TLDCDD87-CSUPERVID
                 MOVE 'N' TO TLDCDD87-GAUTORIZ
                 MOVE 'N' TO TLDCDD87-GSECONDT
              END-IF
      *
              IF WI-TIPO-MEDIO = 'C' AND
                 WI-COD-MEDIO = '02' AND
                 (WI-TRAN-TOLD = '2320' OR '2420' OR '2520' OR '2620')
                 IF WI-CODRET-CAS = 'A'
                    MOVE '9999' TO TLDCDD87-CSUPERVID
                    MOVE 'R' TO TLDCDD87-GAUTORIZ
                    MOVE 'S' TO TLDCDD87-GSECONDT
                    MOVE 'A' TO WK-CODRET-CAS
                    MOVE WI-FLAG-RETEN TO WK-FLAG-RETEN
                 ELSE
                    IF WI-CODRET-CAS = 'C'
                       IF WI-TIPO-MEDIO = 'C'
                          MOVE LCP-DAY1 TO TT-DD
                          MOVE LCP-MONTH TO TT-MM
                          MOVE LCP-CENT TO TT-SS
                          MOVE LCP-YEAR TO TT-AA
                          MOVE WI-NUMOPE-CAS TO TT-NUMOPE
                          PERFORM LEE-MDCFCAS
                          MOVE TT-RETENCION TO WK-FLAG-RETEN
                          MOVE TT-CODRET-CAS TO WK-CODRET-CAS
                          IF TT-CODRET-CAS = 'A'
                             MOVE 'R' TO TLDCDD87-GAUTORIZ
                             MOVE 'S' TO TLDCDD87-GSECONDT
                             MOVE '9999' TO TLDCDD87-CSUPERVID
                          END-IF
                       END-IF
                    ELSE
                       IF WI-CODRET-CAS = ' '
                          MOVE LCP-DAY1 TO TT-DD
                          MOVE LCP-MONTH TO TT-MM
                          MOVE LCP-CENT TO TT-SS
                          MOVE LCP-YEAR TO TT-AA
                          MOVE WI-NUMOPE-CAS TO TT-NUMOPE
                          PERFORM LEE-MDCFCAS
                          IF CR = ZEROS
                             MOVE TT-CODRET-CAS TO WK-CODRET-CAS
                             MOVE TT-RETENCION TO WK-FLAG-RETEN
                             MOVE 1 TO SW-EXISTE-CONSULTA-CAS
                          END-IF
                       ELSE
                          MOVE WI-FLAG-RETEN TO WK-FLAG-RETEN
                       END-IF
                    END-IF
                 END-IF
                 IF WK-CODRET-CAS = 'A' AND WI-CODRET-CAS NOT = ' '
                    STRING WI-CONTROLES(3:2)
                           WI-CONTROLES(6:3)
                           WI-CUENTA-OFI
                           WI-CONTROLES(9:4)
                           WI-CUENTA-NRO
                       DELIMITED BY SIZE INTO WK-CTACTE
                 END-IF
              END-IF
      *
      *
              IF WI-ITF = '0'
                 IF WI-TIPO-MEDIO = 'C' OR 'A'
FO2642              MOVE WI-TRAN-TOLD TO TLDCDD87-EFLD(6)               00001750
FO2642              MOVE 'BAC EXONERADO' TO TLDCDD87-EFLD(1)            00001750
                 END-IF
              ELSE
                 IF WI-TIPO-MEDIO = 'C' OR 'A'
                    MOVE 430 TO TLDCDD87-FLD(6)                         00001750
                 END-IF
              END-IF
           END-IF.
      *
           MOVE 0 TO TLDCDD87-QFIRMAS1
           MOVE 0 TO TLDCDD87-QFIRMAS2
      *
           MOVE 'N' TO TLDCDD87-GVUELTO
           MOVE 'N' TO TLDCDD87-GITF
      *
           MOVE WK-NRO-SOLICITUD TO TLDCDD87-CLOG-TERM
           MOVE WE-DATE TO TLDCDD87-FTERM
           MOVE WE-TIME TO TLDCDD87-HTERM
           MOVE WE-DATE TO TLDCDD87-FPOSTEO
FO         MOVE WI-USUARIO TO TLDCDD87-CUSERID                          00001590
FO         MOVE 'BA' TO TLDCDD87-CRED
           MOVE 981 TO TLDCDD87-CAGENCIA
FO         MOVE 0 TO TLDCDD87-CSAGENCIA
FO         MOVE ZEROS TO TLDCDD87-CLOG-EXT                              00001720
FO         MOVE ZEROS TO TLDCDD87-CLOG-EXT
FO         MOVE WK-NRO-SOLICITUD TO TLDCDD87-CLOG-TERM-EXT
FO         MOVE 'N' TO TLDCDD87-GLAVADO
FO         MOVE 'N' TO TLDCDD87-GPINPAD
FO         MOVE 'N' TO TLDCDD87-GEXT
FO         MOVE 'N' TO TLDCDD87-GCAS
FO         MOVE ZERO TO TLDCDD87-CVERSION.
      *
       RUT-INICIALIZA-MEDIOS-MPI.
      *-------------------------*
           INITIALIZE TLDCMPI-REG
      *
           INITIALIZE REG-FIRMA1-MPI
           INITIALIZE REG-FIRMA2-MPI
           INITIALIZE REG-VUELTO-MPI
           INITIALIZE REG-ITF-MPI
           .

      *
FO    *==> GENERA DATOS VARIABLES TOLD II PARA COMPRA VENTA
      *
       FORMATEAR-CV-COMPRA-VENTA.
      *-------------------------*

      * NUMERO DE CUENTA
           IF WI-CUENTA NUMERIC
              MOVE WI-CUENTA TO TLDCDD87-FLD(1)(3:13)
           ELSE
              MOVE ZEROS TO TLDCDD87-FLD(1)
           END-IF
      * CONTROLES
           IF WI-CONTROLES NUMERIC
FO            MOVE WI-CONTROLES TO TLDCDD87-FLD(2)(4:12)
           ELSE
FO            MOVE ZEROS TO TLDCDD87-FLD(2)(4:12)
           END-IF
      *
           MOVE ZEROS TO TLDCDD87-FLD(3)
      *
FO    * MONTO DE OPERACION ORIGEN
           IF WI-TRAN-TOLD = '2320' OR '2520'
              IF WI-TIPO-MEDIO = 'C'
FO2642           MOVE WI-MEDIO-MED TO TLDCDD87-FLDD(4)
500417                                WOR-IMP-CARGO-ABONO
500417           MOVE WI-MONEDA-ORI TO WOR-MONEDA-ORIGEN
              ELSE
                 MOVE WI-MEDIO-SOUSD TO TLDCDD87-FLDD(4)
                                        WOR-IMP-CARGO-ABONO
FO2642        END-IF
           ELSE
              IF WI-TIPO-MEDIO = 'C'
FO2642           MOVE WI-MEDIO-SOUSD TO TLDCDD87-FLDD(4)
500417                                  WOR-IMP-CARGO-ABONO
500417           MOVE WI-MONEDA-ORI TO WOR-MONEDA-ORIGEN
              ELSE
FO2642           MOVE WI-MEDIO-MED TO TLDCDD87-FLDD(4)
                                      WOR-IMP-CARGO-ABONO
              END-IF
           END-IF.

           MOVE ZEROS TO TLDCDD87-FLD(5).

FO ==>* AFECTACION CONTABLE(CODIGO ORIGEN) CONSULTA NFF
      *    IF WI-TIPO-MEDIO = 'A'
      *       MOVE   ZEROS               TO  TLDCDD87-FLD (6)
      *    ELSE
OJITOO*       MOVE  1717                 TO  TLDCDD87-FLD (6)
      *    END-IF.
      *
      *    IF WI-FLAG-SUPER = '1'
      *       MOVE  1799                 TO  TLDCDD87-FLD (6)
FO ==>*    END-IF.
*
           MOVE ZEROS TO TLDCDD87-FLD(7).
           MOVE ZEROS TO TLDCDD87-FLD(8).
           MOVE ZEROS TO TLDCDD87-FLD(9).
           MOVE ZEROS TO TLDCDD87-FLD(10).
      *
           MOVE 99 TO TLDCDD87-FLD(11).
      *
           MOVE ZEROS TO TLDCDD87-FLD(12)
           MOVE ZEROS TO TLDCDD87-FLD(13)
      *
      * FIN DE CAMPOS NUMERICOS
      *
           MOVE X'FF' TO TLDCDD87-DEL(13).
      *
      * FIN DE CAMPOS ALFANUMERICOS
      *
FO2642     MOVE X'FF' TO TLDCDD87-EDEL(13)
           .
       FORMATEAR-CV-TRX-FINAL.
      *-----------------------*
      *
           PERFORM RUT-HEADER-CV-TLDCDD87
      *
           PERFORM RUT-INICIALIZA-MEDIOS-MPI
      *
           PERFORM MEDIOS-NOTA-CTBLE-MPI
      *
           PERFORM GENERAR-DATOS-VARIABLES-TOLD
      *
           PERFORM MEDIOS-DE-CARGO-MPI
           .
       GENERAR-DATOS-VARIABLES-TOLD.
      *----------------------------*
           PERFORM VARYING WI-I FROM 1 BY 1
              UNTIL WI-I > 20
                   MOVE SPACES TO TLDCDD87-EFLD(WI-I)
                   MOVE ZEROS TO TLDCDD87-FLD(WI-I)
           END-PERFORM
      *
      * TIPO DE DOCUMENTO
           MOVE 01 TO TLDCDD87-FLD(1)

OJITOO* NUMERO DE DOCUMENTO (CONSULTA N.F.F.)
           MOVE '00000000' TO TLDCDD87-EFLD(1)

      * MONEDA SOLICITADA (ABONO)
           MOVE WI-MONEDA-DES TO W-NUMERIC
           MOVE W-NUMERIC(13:3) TO TLDCDD87-EFLD(2)

      * IMPORTE SOLICITADO
FO2515*    IF WI-TRAN-TOLD = '2320' OR '2350'
FO2515     IF WI-TRAN-TOLD = '2320' OR '2520'
              MOVE WI-MEDIO-SOUSD TO TLDCDD87-FLDD(2)
           ELSE
              MOVE WI-MEDIO-MED TO TLDCDD87-FLDD(2)
           END-IF

      * MONEDA RECIBIDA (CARGO)
           MOVE WI-MONEDA-ORI TO W-NUMERIC
           MOVE W-NUMERIC(13:3) TO TLDCDD87-EFLD(3)
500417     MOVE WI-MONEDA-ORI TO WOR-MONEDA-ORIGEN

      * IMPORTE CONTRAVALOR (CARGO)
           IF WI-TRAN-TOLD = '2320' OR '2520'
              MOVE WI-MEDIO-MED TO TLDCDD87-FLDD(3)
           ELSE
              MOVE WI-MEDIO-SOUSD TO TLDCDD87-FLDD(3)
           END-IF

      * NOMBRE DEL CLIENTE
           MOVE SPACES TO TLDCDD87-EFLD(4)
      * BUSCAR NOMBRE, DOCUMENTO RUC Y TIPO DOCUMENTO
           PERFORM CONSULTAR-DOCUMENTO
      * TIPO DE CLIENTE
           MOVE '01' TO TLDCDD87-EFLD(5)

      * ARBITRAJE
           MOVE ZEROS TO TLDCDD87-FLD(4)

FO2515     IF WI-TRAN-TOLD = '2320' OR '2420'
      * TIPO DE CAMBIO
              COMPUTE TLDCDD87-FLD(5) = WI-RATIO * 10
FO2515     ELSE
FO2515        COMPUTE TLDCDD87-FLD(4) = WI-RATIO * 10
FO2515     END-IF

      * GLOSA
           MOVE SPACES TO TLDCDD87-EFLD(6)

      * CODIGO DE MEDIO DE ABONO 1
           MOVE '05' TO TLDCDD87-EFLD(7)

      * IMPORTE DEL MEDIO DE ABONO 1
           IF WI-TRAN-TOLD = '2320' OR '2520'
              MOVE WI-MEDIO-SOUS TO TLDCDD87-FLD(6)
           ELSE
              MOVE WI-MEDIO-ME TO TLDCDD87-FLD(6)
           END-IF

      * DATOS DEL MEDIO DE ABONO 1
      *
FO         MOVE MPI-REDEFINICION(1)(1:40) TO TLDCDD87-EFLD(8)
FO         MOVE MPI-REDEFINICION(1)(41:30) TO TLDCDD87-EFLD(9)

OJO   *    MOVE 11111111              TO   TLDCDD87-EFLD (8) (1:8)
OJO   *    MOVE WI-CLIENTE            TO   TLDCDD87-EFLD (8) (9:32)
OJO   *    MOVE WI-CLIENTE            TO   TLDCDD87-EFLD (9) (1:28)

OJITOO* CLAVE JOURNAL TMF PARA BT Y SITEMA ORIGEN
FO    *    MOVE TIBEFLD1(1:30)        TO   TLDCDD87-EFLD (14)

OJITOO* SISTEMA ORIGEN TMF PARA BT
FO    *    MOVE TIBEFLD1(31:4)        TO   TLDCDD87-EFLD(14) (31:4)

      * FIN DE CAMPOS NUMERICOS
           MOVE X'FF' TO TLDCDD87-DEL(07)

      * FIN DE CAMPOS ALFANUMERICOS
           MOVE X'FF' TO TLDCDD87-EDEL(14)
           .
       CONSULTAR-DOCUMENTO.
      *--------------------*
      *
           INITIALIZE SRMR031-DATA.
      *
           MOVE 'R' TO RM-ACCION
           MOVE ALL ZEROES TO RM-CUST-CTLS-I
      *
           MOVE '0003' TO RM-CTL1-CUST-I
           MOVE '1' TO RM-FUNCTION
           MOVE ZEROS TO RM-TIN-CD
                         RM-TIN
           MOVE WI-CLIENTE TO RM-CUST-NBR-I
      *
           EXEC CICS LINK PROGRAM('SRMR031 ')
                COMMAREA(SRMR031-DATA)
                LENGTH(LENGTH OF SRMR031-DATA)
                END-EXEC.
      *
           IF RM-RETURN-CODE = '00'
              MOVE RMCMRTCS-NAME-LINE-1 TO TLDCDD87-EFLD(4)
              MOVE RMCMRTCS-TIN-CD TO WK-TINX
              MOVE WK-TINN TO TLDCDD87-FLD(1)
              MOVE RMCMRTCS-TIN TO TLDCDD87-EFLD(1)
           END-IF
           .
       MEDIOS-NOTA-CTBLE-MPI.
      *---------------------*
      *===>MEDIO CONTABLE
           MOVE '05' TO MPI-MEDIO-PAGO(1)

OJITOO*===>OFICINA EMISORA     (CONSULTA NFF)
           MOVE '981' TO MPI-OFI-EMISORA(1)

OJITOO*===>OFICINA RECEPTORA   (CONSULTA NFF)
           MOVE '981' TO MPI-OFI-RECEPT(1)

OJITOO*===>CUENTA CONTABLE DE PRUEBAS (CONSULTA NFF)
      *    MOVE 19080900000103         TO MPI-CTA-CTBLE (1)
           MOVE ZEROS TO MPI-CTA-CTBLE(1)

           MOVE ZEROS TO MPI-NRO-NOTA(1)
           MOVE SPACES TO MPI-IND-NOAFECTO-CTBLE(1)
           .
       MEDIOS-DE-CARGO-MPI.
      *--------------------*
      *    MEDIO DE CARGO
           MOVE 05 TO MPI-MEDIO-PAGO(1)
           MOVE 'C' TO MPI-TIPO-OPERACION(1)

      *    MONEDA DE CARGO
           MOVE WI-MONEDA-ORI TO MPI-MONEDA-PAGO(1)

      *    IMPORTE DE CARGO
           IF WI-TRAN-TOLD = '2320' OR '2520'
              MOVE WI-MEDIO-MED TO MPI-IMPORTE-PAGO(1)
           ELSE
              MOVE WI-MEDIO-SOUSD TO MPI-IMPORTE-PAGO(1)
           END-IF

           MOVE 'N' TO MPI-FLAG-TASA-NEG(1)
      *
           MOVE MPI-IMPORTE-PAGO(1) TO MPI-PAGO-C-V(1)
           MOVE WI-MONEDA-DES TO MPI-MON-EQUIVALENTE(1)

      *    IMPORTE EQUIVALENTE
           IF WI-TRAN-TOLD = '2320' OR '2520'
              MOVE WI-MEDIO-SOUSD TO MPI-IMP-EQUIVALENTE(1)
           ELSE
              MOVE WI-MEDIO-MED TO MPI-IMP-EQUIVALENTE(1)
           END-IF

           MOVE WI-RATIOD TO MPI-TIPO-CAMBIO(1)
      *
           .
       RUT-ENLAZAR-CV-TLDOD87.
      *----------------------*
           MOVE TLDCDD87 TO TLDCDD05-TRAMA
           MOVE TLDCMPI-REG TO TLDCDD05-MEDIOS
500417     MOVE 'NO' TO FLAG-LINK-OK
      *
           EXEC CICS LINK PROGRAM('TLDOD87')
                COMMAREA(TLDCDD05)
                LENGTH(LENGTH OF TLDCDD05)
                RESP(WK-RESP)
                END-EXEC.
      *
           MOVE TLDCDD05-TRAMA TO TLDCDD87
           MOVE TLDCDD05-MEDIOS TO TLDCMPI-REG
      *
500417     MOVE WK-RESP TO NUM-RC-TLD
           EVALUATE WK-RESP
           WHEN DFHRESP(NORMAL)
                MOVE TLDCDD87-RC-GEN TO WOR-RC-TLD
500417          MOVE ALL SPACES TO WOR-DE-RC-TLD MSGS-TEXT
500417          MOVE 'SI' TO FLAG-LINK-OK
      *
500417          EVALUATE TLDCDD87-RC-GEN
500417          WHEN 00
500417               IF TLDCDD87-CTRANSID NOT = '430'
500417                  PERFORM 800-REGISTRO-BITACORA
500417               END-IF
500417          WHEN 99
                     MOVE 'TR' TO WK-CODRET-CHAR
                     MOVE '* LONGITUD TRAMA TLDOD87 ERR'
                        TO WK-MENSAJE WOR-DE-RC-TLD
                     MOVE 10 TO WK-RC
500417          WHEN OTHER
500417               PERFORM MSG-RETORNO-TOLD
                     MOVE MSGS-TEXT TO WOR-DE-RC-TLD
                                       WK-MENSAJE
                     MOVE 'TR' TO WK-CODRET-CHAR
500417               PERFORM 800-REGISTRO-BITACORA
                END-EVALUATE
           WHEN DFHRESP(PGMIDERR)
                MOVE 'PG' TO WK-CODRET-CHAR
                MOVE 'PGMIDERR TLDOD87' TO WK-MENSAJE
                MOVE 10 TO WK-RC
           WHEN OTHER
                MOVE 'PG' TO WK-CODRET-CHAR
                MOVE 'ERROR TLDOD87 LINK' TO WK-MENSAJE
                MOVE 10 TO WK-RC
           END-EVALUATE.

       MSG-RETORNO-TOLD.
      *-----------------*
FO0628     MOVE TLDCDD87-CLOG TO CLOG
FO0628     MOVE TLDCDD87-FSUBIDA TO WS-XFSUBIDA
FO0628     MOVE WS-NFSUBIDA TO FSUBIDA
FO2692*
FO2692     EXEC SQL
FO2692          SELECT
FO2692          EYE_MSGS,
FO2692          MSGS
FO2692          INTO
FO2692          :EYE-MSGS,
FO2692          :MSGS
FO2692          FROM TLDFDDLG
FO2692          WHERE FSUBIDA = :FSUBIDA
FO2692          AND CLOG = :CLOG
FO2692          END-EXEC.
FO2692     IF SQLCODE EQUAL 0
FO2692        MOVE MSGS-TEXT TO TLDCDD09
              MOVE TLDCDD09-ACU(1) TO WK-TEXTO-TOLD
FO2692     ELSE
FO2692        MOVE WE-MSG-ERRLOG TO WK-TEXTO-TOLD
500417        MOVE ALL SPACES TO MSGS-TEXT
FO2692     END-IF
           PERFORM VALIDAR-CONSULTA-CAS
      *
FO2692     .
       PROCESAR-TRANSFERENCIA.                                          00006200
      *-------------------*                                             00006200
           MOVE WI2-NUM-OPER TO WK-NRO-SOLICITUD
           MOVE ALL SPACES TO WK-NUM-OPER-SYSTEMAT WK-CODRET-CHAR-SYS
           COMPUTE WK-MONTO-CAS = WI2-MONTO / 100
           IF ((WI2-COMISION IS NUMERIC AND
              WI2-COMISION GREATER ZERO) AND (WI2-TIP-OPER = 'C') AND
              (WI2-TRAN-TOLD NOT EQUAL '2900'))
              PERFORM RTNA-SALDOS-SYSTEMATIC
              IF ((WK-RETORNO-RSYS = 09)
                 AND (WI2-FLAG-SOBRE = '1')
                 AND (WI2-FLAG-SUPER = '1'))
                 MOVE 00 TO WK-RETORNO-RSYS
              ELSE
                 IF ((WI2-TIP-OPER = 'C') AND (WI2-TRAN-TOLD = '0892')
                    AND (WI2-PRODUCTO = '001')
                    AND (WK-RETORNO-RSYS = 09))
                    MOVE 00 TO WK-RETORNO-RSYS
                 END-IF
              END-IF
              IF WK-RETORNO-RSYS = 00
FO               PERFORM GENERAR-HEADER-TR-TOLD
FO               PERFORM GENERAR-VARIABLES-TOLD
FO               PERFORM ENLAZAR-PROGRAMA-TLDOD87
                 MOVE TLDCDD87-RC-GEN TO WK-RETORNO-TOLD
                 MOVE 'S' TO WK-FLAG-CODRET-COMIS
                 MOVE TLDCDD87-CLOG TO WK-NUM-TOLD01
                 IF WK-RETORNO-TOLD = 00 AND WI2-ITF = '0'
                    MOVE '1' TO WI2-ITF
                    MOVE WI2-COMISION TO WI2-MONTO
FO                  PERFORM GENERAR-HEADER-TR-TOLD
FO                  PERFORM GENERAR-VARIABLES-TOLD
FO                  PERFORM ENLAZAR-PROGRAMA-TLDOD87
                    MOVE TLDCDD87-RC-GEN TO WK-RETORNO-TOLD-COMIS
                    MOVE 'S' TO WK-FLAG-CODRET-COMIS
                    MOVE TLDCDD87-CLOG TO WK-NUM-TOLD02
                 ELSE
FO                  MOVE ZEROS TO WK-NUM-TOLD01
FO                  MOVE ZEROS TO WK-NUM-TOLD02
                 END-IF
              END-IF
           ELSE
              IF ((WI2-COMISION IS NUMERIC AND
                 WI2-COMISION GREATER ZERO) AND (WI2-TIP-OPER = 'C') AND
                 (WI2-TRAN-TOLD EQUAL '2900'))
FO               PERFORM GENERAR-HEADER-TR-TOLD
FO               PERFORM GENERAR-HEADER-TR-TOLD-EXT
                 MOVE WI2-NUM-TOLD01 TO TLDCDD87-CLOG-EXT
FO               PERFORM ENLAZAR-PROGRAMA-TLDOD87
                 MOVE TLDCDD87-CLOG TO WK-NUM-TOLD01
                 MOVE TLDCDD87-RC-GEN TO WK-RETORNO-TOLD
FO               IF WK-RETORNO-TOLD = 00 AND WI2-ITF = '0'
                    MOVE '1' TO WI2-ITF
FO                  PERFORM GENERAR-HEADER-TR-TOLD
FO                  PERFORM GENERAR-HEADER-TR-TOLD-EXT
                    MOVE WI2-NUM-TOLD02 TO TLDCDD87-CLOG-EXT
FO                  PERFORM ENLAZAR-PROGRAMA-TLDOD87
                    MOVE TLDCDD87-CLOG TO WK-NUM-TOLD01
                    MOVE TLDCDD87-RC-GEN TO WK-RETORNO-TOLD-COMIS
                    MOVE 'S' TO WK-FLAG-CODRET-COMIS
                 ELSE
FO                  MOVE ZEROS TO WK-NUM-TOLD01
FO                  MOVE ZEROS TO WK-NUM-TOLD02
                 END-IF
              ELSE
                 MOVE 00 TO WK-RETORNO-RSYS
FO    *       PERFORM PROCESA-REG-BELSAF02
FO               PERFORM GENERAR-HEADER-TR-TOLD

                 IF WI2-TRAN-TOLD = '2900'
FO                  PERFORM GENERAR-HEADER-TR-TOLD-EXT
                 ELSE
                    PERFORM GENERAR-VARIABLES-TOLD
                 END-IF
FO               PERFORM ENLAZAR-PROGRAMA-TLDOD87
                 MOVE TLDCDD87-CLOG TO WK-NUM-TOLD01
                 MOVE TLDCDD87-RC-GEN TO WK-RETORNO-TOLD
              END-IF
           END-IF
           .
      *
       GENERAR-HEADER-TR-TOLD.
      *----------------------*
           INITIALIZE TLDCDD87.
           PERFORM VARYING WI-I FROM 1 BY 1
              UNTIL WI-I > 20
                   MOVE SPACES TO TLDCDD87-EFLD(WI-I)
                   MOVE ZEROS TO TLDCDD87-FLD(WI-I)
           END-PERFORM
      *
           MOVE 'TLTM' TO TLDCDD87-CTRANS-DESP
      *
FO         EVALUATE WI2-PRODUCTO
           WHEN 001
FO              IF WI2-TIP-OPER = 'C'
                   IF WI2-CUENTA EQUAL SPACES
                      MOVE '0222' TO TLDCDD87-CTRANSID
                   ELSE
                      MOVE '0892' TO TLDCDD87-CTRANSID
                   END-IF
                ELSE
                   IF WI2-CUENTA EQUAL SPACES
                      MOVE '0211' TO TLDCDD87-CTRANSID
                   ELSE
                      MOVE '0832' TO TLDCDD87-CTRANSID
                   END-IF
                END-IF
           WHEN OTHER
FO    *       SI CUENTA DE AHORROS
FO              IF WI2-TIP-OPER = 'C'
                   IF WI2-CUENTA EQUAL SPACES
                      MOVE '0222' TO TLDCDD87-CTRANSID
                   ELSE
                      MOVE '1892' TO TLDCDD87-CTRANSID
                   END-IF
                ELSE
                   IF WI2-CUENTA EQUAL SPACES
                      MOVE '0211' TO TLDCDD87-CTRANSID
                   ELSE
                      MOVE '1832' TO TLDCDD87-CTRANSID
                   END-IF
                END-IF
           END-EVALUATE.

           MOVE ZEROS TO TLDCDD87-CLOG-TERM
           MOVE WE-DATE TO TLDCDD87-FTERM
           MOVE WE-TIME TO TLDCDD87-HTERM
           MOVE WE-DATE TO TLDCDD87-FPOSTEO
           MOVE 'BA' TO TLDCDD87-CRED
OJITOO     MOVE 981 TO TLDCDD87-CAGENCIA
           MOVE 0 TO TLDCDD87-CSAGENCIA
           MOVE WI2-USUARIO TO TLDCDD87-CUSERID
           MOVE SPACES TO TLDCDD87-CESTACION
           MOVE SPACES TO TLDCDD87-CSUPERVID
      *
FO0628     IF WI2-FLAG-SUPER = '1'
              MOVE 'S' TO TLDCDD87-GSECONDT
704657        MOVE 'S' TO TLDCDD87-GAUTORIZ
704657*        MOVE  'R'                 TO  TLDCDD87-GAUTORIZ
              MOVE '9999    ' TO TLDCDD87-CSUPERVID
           ELSE
              MOVE 'N' TO TLDCDD87-GSECONDT
              MOVE 'N' TO TLDCDD87-GAUTORIZ
           END-IF
      *
FO         IF WI2-TIP-OPER = 'C' AND WI2-TRAN-TOLD = '0892'
FO            AND WI2-PRODUCTO = '001'
FO            IF WI2-CODRET-CAS = 'A'
                 MOVE 'R' TO TLDCDD87-GAUTORIZ
                 MOVE 'S' TO TLDCDD87-GSECONDT
FO               MOVE '9999' TO TLDCDD87-CSUPERVID
FO               MOVE 'A' TO WK-CODRET-CAS
FO               MOVE WI2-FLAG-RETEN TO WK-FLAG-RETEN
FO            ELSE
FO               IF WI2-CODRET-CAS = 'C'
FO                  IF WI2-TIP-OPER = 'C'
FO                     MOVE LCP-DAY1 TO TT-DD
FO                     MOVE LCP-MONTH TO TT-MM
FO                     MOVE LCP-CENT TO TT-SS
FO                     MOVE LCP-YEAR TO TT-AA
FO                     MOVE WI2-NUMOPE-CAS TO TT-NUMOPE
FO                     PERFORM LEE-MDCFCAS
FO                     MOVE TT-RETENCION TO WK-FLAG-RETEN
FO                     MOVE TT-CODRET-CAS TO WK-CODRET-CAS
FO                     IF TT-CODRET-CAS = 'A'
                          MOVE 'R' TO TLDCDD87-GAUTORIZ
                          MOVE 'S' TO TLDCDD87-GSECONDT
FO                        MOVE '9999' TO TLDCDD87-CSUPERVID
FO                     END-IF
FO                  END-IF
FO               ELSE
FO                  IF WI2-CODRET-CAS = ' '
FO                     MOVE LCP-DAY1 TO TT-DD
FO                     MOVE LCP-MONTH TO TT-MM
FO                     MOVE LCP-CENT TO TT-SS
FO                     MOVE LCP-YEAR TO TT-AA
FO                     MOVE WI2-NUMOPE-CAS TO TT-NUMOPE
FO                     PERFORM LEE-MDCFCAS
FO                     IF CR = ZEROS
FO                        MOVE TT-CODRET-CAS TO WK-CODRET-CAS
FO                        MOVE TT-RETENCION TO WK-FLAG-RETEN
FO                        MOVE 1 TO SW-EXISTE-CONSULTA-CAS
FO                     END-IF
FO                  ELSE
FO                     MOVE WI2-FLAG-RETEN TO WK-FLAG-RETEN
FO                  END-IF
FO               END-IF
FO            END-IF
FO            IF WK-CODRET-CAS = 'A' AND WI2-CODRET-CAS NOT = ' '
FO               STRING WI2-BANCO
FO                      WI2-MONEDA
FO                      WI2-OFICINA
                        '0'
FO                      WI2-PRODUCTO
FO                      WI2-CUENTA
FO                  DELIMITED BY SIZE INTO WK-CTACTE
FO            END-IF
FO         END-IF.
      *
           MOVE ZEROS TO TLDCDD87-CLOG-EXT
           MOVE WK-NRO-SOLICITUD TO TLDCDD87-CLOG-TERM-EXT
           MOVE 'N' TO TLDCDD87-GLAVADO
           MOVE 'N' TO TLDCDD87-GPINPAD
FO0628     MOVE 'N' TO TLDCDD87-GEXT
           MOVE 'N' TO TLDCDD87-GCAS
           MOVE ZERO TO TLDCDD87-CVERSION
           MOVE 'N' TO TLDCDD87-GMULT
           MOVE 1 TO TLDCDD87-QSEQ
           MOVE ZERO TO TLDCDD87-QMEDIOS
           MOVE ZERO TO TLDCDD87-QFIRMAS1
           MOVE ZERO TO TLDCDD87-QFIRMAS2
           MOVE 'N' TO TLDCDD87-GVUELTO
           MOVE 'N' TO TLDCDD87-GITF
           MOVE SPACES TO TLDCDD87-CSESION
           MOVE SPACES TO TLDCDD87-GLINEA
           MOVE SPACES TO TLDCDD87-HESTADIS
           MOVE 'N' TO TLDCDD87-SINAFECT
           MOVE ZEROS TO TLDCDD87-RC-GEN
           MOVE ZERO TO TLDCDD87-CLOG
           .
      *
       GENERAR-VARIABLES-TOLD.
      *-----------------------*
      *
FO         MOVE ZEROS TO WE-NUMERO-CTA
FO         MOVE WI2-OFICINA TO WE-COD-TIENDA
           IF WI2-CUENTA NUMERIC
FO            MOVE WI2-CUENTA TO WE-NRO-CUENTA
FO            MOVE WE-NUMERO-CTA TO TLDCDD87-FLD(1)(3:13)
           ELSE
FO            MOVE ZEROS TO TLDCDD87-FLD(1)
           END-IF
FO         MOVE WI2-BANCO TO WE-CONTROL-1
FO         MOVE WI2-MONEDA TO WE-CONTROL-2
FO         MOVE WI2-PRODUCTO TO WE-CONTROL-4
FO         MOVE WE-CONTROLES TO TLDCDD87-FLD(2)(4:12).
      *
           MOVE ZEROS TO TLDCDD87-FLD(3)
      *
           MOVE ZEROS TO TLDCDD87-FLD(5).
FO0628*    MOVE ZEROS                    TO  TLDCDD87-FLD (6).
FO         MOVE ZEROS TO TLDCDD87-FLD(6)
      *
JFOF       IF WI2-ITF = '0'
JFOF          IF WI2-TIP-OPER = 'C' OR 'A'
FO2642           MOVE '1414' TO TLDCDD87-FLD(6)                         00001750
FO2642           MOVE 'BAC EXONERADO' TO TLDCDD87-EFLD(1)               00001750
JFOF          END-IF
JPAZ          IF (WI2-COMISION IS NUMERIC AND
JPAZ             WI2-COMISION GREATER ZERO AND WI2-TIP-OPER = 'C' AND
JPAZ             WK-RETORNO-RSYS = 00)
JPAZ             COMPUTE WI2-MONTO = WI2-MONTO - WI2-COMISION           00001750
FO2642           MOVE '1414' TO TLDCDD87-FLD(6)                         00001750
FO2642           MOVE 'BAC EXONERADO' TO TLDCDD87-EFLD(1)               00001750
JFOF          END-IF
JPAZ       ELSE
JPAZ          MOVE 0 TO TLDCDD87-FLD(6)                                 00001750
JPAZ          MOVE ALL SPACES TO TLDCDD87-EFLD(6)                       00001750
JFOF       END-IF.
FO         MOVE WI2-MONTO-D TO TLDCDD87-FLDD(4)
500417                         WOR-IMP-CARGO-ABONO.
      *    IF WI2-FLAG-SOBRE = 1
OJITOO*       MOVE  1799                 TO  TLDCDD87-FLD (6)
      *    END-IF.
      *
           MOVE ZEROS TO TLDCDD87-FLD(7).
           MOVE ZEROS TO TLDCDD87-FLD(8).
           MOVE ZEROS TO TLDCDD87-FLD(9).
           MOVE ZEROS TO TLDCDD87-FLD(10).
      *
           MOVE 99 TO TLDCDD87-FLD(11).
      *
      *
           MOVE ZEROS TO TLDCDD87-FLD(12)
           MOVE ZEROS TO TLDCDD87-FLD(13)
      *
      * FIN DE CAMPOS NUMERICOS
      *
           MOVE X'FF' TO TLDCDD87-DEL(13).
      *
      * FIN DE CAMPOS ALFANUMERICOS
FO2642     MOVE X'FF' TO TLDCDD87-EDEL(13)
           .
      *
       ENLAZAR-PROGRAMA-TLDOD87.
      *-------------------------*
      *
500417     MOVE 'NO' TO FLAG-LINK-OK
           EXEC CICS LINK PROGRAM('TLDOD87')
                COMMAREA(TLDCDD87)
                LENGTH(LENGTH OF TLDCDD87)
                RESP(WK-RESP)
                END-EXEC.
      *
           EVALUATE WK-RESP
           WHEN DFHRESP(NORMAL)
                MOVE TLDCDD87-RC-GEN TO WOR-RC-TLD
500417          MOVE ALL SPACES TO WOR-DE-RC-TLD MSGS-TEXT
500417          MOVE 'SI' TO FLAG-LINK-OK
      *
500417          EVALUATE TLDCDD87-RC-GEN
500417          WHEN 00
500417               IF TLDCDD87-CTRANSID NOT = '430'
500417                  PERFORM 800-REGISTRO-BITACORA
500417               END-IF
500417          WHEN 99
                     MOVE 'TR' TO WK-CODRET-CHAR
                     MOVE '* LONGITUD TRAMA TLDOD87 ERR'
                        TO WK-MENSAJE WOR-DE-RC-TLD
                     MOVE 10 TO WK-RC
500417          WHEN OTHER
500417               PERFORM MSG-RETORNO-TOLD
                     MOVE MSGS-TEXT TO WOR-DE-RC-TLD
                     MOVE 'TR' TO WK-CODRET-CHAR
                                  WK-MENSAJE
500417               PERFORM 800-REGISTRO-BITACORA
                END-EVALUATE
           WHEN DFHRESP(PGMIDERR)
                MOVE 'PG' TO WK-CODRET-CHAR
                MOVE '** PGMIDERR TLDOD87' TO WK-MENSAJE
                MOVE 10 TO WK-RC
           WHEN OTHER
                MOVE 'PG' TO WK-CODRET-CHAR
                MOVE '** ERROR TLDOD87 LINK' TO WK-MENSAJE
                MOVE 10 TO WK-RC
           END-EVALUATE.

      *
       GENERAR-HEADER-TR-TOLD-EXT.
      *--------------------------*
      *
           IF WI2-TRAN-TOLD = '2900'
              MOVE WI2-NUM-TOLD01 TO TLDCDD87-CLOG-EXT
              MOVE WI2-USUARIO TO TLDCDD87-CUSERID
FO            MOVE 'S' TO TLDCDD87-GEXT
           END-IF.
      *
       RTNA-SALDOS-SYSTEMATIC.
      *-----------------------*
      *
      *       SIMO201 : CONSULTA SALDOS EN SAVING'S - & - IMPAC'S
      *
           MOVE 00 TO WK-RETORNO-RSYS
           MOVE 03 TO T-ENTIDAD
           MOVE 001 TO T-MONEDA
           MOVE 04 TO T-TIPO-CONCEPTO
           MOVE 04 TO T-CONCEPTO
           MOVE SPACES TO T-DOCUMENTO
           MOVE ZEROS TO T-SECUENCIA T-CLIENTE T-OFICINA T-CUENTA.
           IF WI2-TIP-OPER = 'C'
              IF WI2-TRAN-TOLD = '0892'
                 MOVE 'IM' TO T-SISTEMA WK-APLICACION
                 MOVE 0001 TO T-CATEGORIA
                 MOVE ZEROS TO T-PRODUCTO
              ELSE
                 MOVE 'ST' TO T-SISTEMA WK-APLICACION
                 MOVE 0002 TO T-CATEGORIA
                 MOVE 2222 TO T-PRODUCTO
              END-IF
           END-IF
           COMPUTE WK-COMISION-SYS = WI2-COMISION / 100
           COMPUTE WK-MONTO-SYS = WI2-MONTO / 100
           MOVE ZEROS TO WK-TARIF
           PERFORM RTNA-LEE-TARIFARIO
           MOVE T-TASA1-NOM(1) TO WK-TARIF.

           IF WK-TARIF IS NUMERIC
              COMPUTE WK-COMISION-SYS ROUNDED =(WK-TARIF / 100) *
                 WK-COMISION-SYS
           ELSE
              COMPUTE WK-COMISION-SYS ROUNDED =(WK-COMISION-SYS * 0.001)
           END-IF

           COMPUTE WK-MONTO-SYS ROUNDED = WK-MONTO-SYS + WK-COMISION-SYS

           IF WK-APLICACION = 'IM'
              STRING '03'
                     WI2-MONEDA
                     WI2-OFICINA
                     '0'
                     WI2-PRODUCTO
                     WI2-CUENTA DELIMITED BY SIZE INTO
                 WS-WMS-CONTROL-KEY
              EXEC CICS LINK PROGRAM('SIMO201')
                   COMMAREA(WS-DATOS-IMPACS)
                   LENGTH(700)
                   END-EXEC
              IF WS-WMS-RETORNO NOT = 00
                 MOVE WS-WMS-RETORNO TO WK-RETORNO-RSYS
              ELSE
                 IF WS-MEMO-CURRENT-BAL IS GREATER WK-MONTO-SYS
                    CONTINUE
                 ELSE
                    MOVE 09 TO WK-RETORNO-RSYS
                 END-IF
              END-IF
           ELSE
              MOVE WK-APLICACION TO LK-APPL
              STRING '03'
                     WI2-MONEDA
                     WI2-OFICINA
                     WI2-PRODUCTO
                     '0000'
                     WI2-CUENTA DELIMITED BY SIZE INTO LK-KEY
              MOVE 'R' TO LK-CONTROL-OP
              EXEC CICS LINK PROGRAM('SSTO101')
                   COMMAREA(LK-AREA)
                   LENGTH(600)
                   END-EXEC
              IF LK-RETURN-CODE NOT = 00
                 MOVE LK-RETURN-CODE TO WK-RETORNO-RSYS
              ELSE
                 IF LK-SALDIS IS GREATER WK-MONTO-SYS
                    CONTINUE
                 ELSE
                    MOVE 09 TO WK-RETORNO-RSYS
                 END-IF
              END-IF
           END-IF.
      *
       RTNA-LEE-TARIFARIO.
      *-------------------*
           EXEC CICS READ DATASET('TFRFTARI')
                RIDFLD(T-CLAVE)
                INTO (REG-TARIFA)
                RESP(CR)
                END-EXEC.
      *
       LEER-SOLICITUD-UPD.
      *------------------*
           EXEC CICS
                READ
                UPDATE
                FILE(WK-MDCFTOLD)
                INTO (REG-SOLICITUDES-MDC)
                RIDFLD(WK-CLAVE)
                RESP(WK-RESP)
                END-EXEC
           IF WK-RESP EQUAL DFHRESP(NOTFND)
              IF WK-FLAG-INI NOT EQUAL 'S'
                 MOVE 'NF' TO WK-CODRET-CHAR
                 MOVE '** ER SOLICITUD NO HAY MDCFTOLD' TO WK-MENSAJE
                 MOVE 10 TO WK-RC
500417           PERFORM VERIFICO-MENSAJE-MDCFTOLD
              END-IF
           ELSE
              IF WK-RESP NOT EQUAL DFHRESP(NORMAL)
                 MOVE 'SB' TO WK-CODRET-CHAR
                 MOVE '** ERROR LEER SOLICIT MDCFTOLD' TO WK-MENSAJE
                 MOVE 10 TO WK-RC
500417           PERFORM VERIFICO-MENSAJE-MDCFTOLD
              END-IF
           END-IF
           .
       REGRABAR-SOLICITUD.
      *------------------*
500417     MOVE 'SB' TO WK-CODRET-CHAR
500417     MOVE 'ERROR ACTUALIZAR MDCFTOLD' TO WK-MENSAJE
500417     MOVE 10 TO WK-RC

           EXEC CICS
                REWRITE
                FILE(WK-MDCFTOLD)
                FROM (REG-SOLICITUDES-MDC)
                RESP(WK-RESP)
                END-EXEC
500417     PERFORM VERIFICO-MENSAJE-MDCFTOLD
           .

       GRABAR-SOLICITUD.
      *-----------------*
           MOVE 'DP' TO WK-CODRET-CHAR
           MOVE '** ERROR GRABAR MDCFTOLD' TO WK-MENSAJE
           MOVE 10 TO WK-RC
           EXEC CICS
                WRITE
                FILE(WK-MDCFTOLD)
                FROM (REG-SOLICITUDES-MDC)
                RIDFLD(WK-CLAVE)
                RESP(WK-RESP)
                END-EXEC
500417     PERFORM VERIFICO-MENSAJE-MDCFTOLD
           .

       ELIMINAR-SOLICITUD.
      *-----------------*
500417     MOVE 'DL' TO WK-CODRET-CHAR
500417     MOVE 'ERROR ELIMINAR MDCFTOLD' TO WK-MENSAJE
500417     MOVE 10 TO WK-RC

           EXEC CICS
                DELETE
                FILE(WK-MDCFTOLD)
                RESP(WK-RESP)
                END-EXEC
500417     PERFORM VERIFICO-MENSAJE-MDCFTOLD
           .

500417 VERIFICO-MENSAJE-MDCFTOLD.
      *--------------------------*
           EVALUATE WK-RESP
           WHEN DFHRESP(NORMAL)
                MOVE ALL SPACES TO WK-CODRET-CHAR
                                   WK-MENSAJE
                MOVE ALL ZEROS TO WK-RC

           WHEN DFHRESP(NOSTART)
                MOVE ' NO START ' TO WK-MENSAJE-TXT
                MOVE 10 TO WK-MENSAJE-RC
           WHEN DFHRESP(FILENOTFOUND)
                MOVE ' NO FILE  ' TO WK-MENSAJE-TXT
                MOVE 12 TO WK-MENSAJE-RC
           WHEN DFHRESP(NOTFND)
                MOVE ' NO FOUND ' TO WK-MENSAJE-TXT
                MOVE 13 TO WK-MENSAJE-RC
           WHEN DFHRESP(DUPREC)
                MOVE ' DUP RECOR' TO WK-MENSAJE-TXT
                MOVE 14 TO WK-MENSAJE-RC
           WHEN DFHRESP(DUPKEY)
                MOVE ' DUP KEY  ' TO WK-MENSAJE-TXT
                MOVE 15 TO WK-MENSAJE-RC
           WHEN DFHRESP(INVREQ)
                MOVE ' INV REQ  ' TO WK-MENSAJE-TXT
                MOVE 16 TO WK-MENSAJE-RC
           WHEN DFHRESP(NOTOPEN)
                MOVE ' NOT OPEN ' TO WK-MENSAJE-TXT
                MOVE 19 TO WK-MENSAJE-RC
           WHEN DFHRESP(ILLOGIC)
                MOVE ' ILLOGIC  ' TO WK-MENSAJE-TXT
                MOVE 21 TO WK-MENSAJE-RC
           WHEN DFHRESP(NOTAUTH)
                MOVE ' NO AUTORZ' TO WK-MENSAJE-TXT
                MOVE 70 TO WK-MENSAJE-RC
           WHEN DFHRESP(DISABLED)
                MOVE ' DISABLED ' TO WK-MENSAJE-TXT
                MOVE 84 TO WK-MENSAJE-RC
           WHEN DFHRESP(LOCKED)
                MOVE ' LOCKED   ' TO WK-MENSAJE-TXT
                MOVE 100 TO WK-MENSAJE-RC
           WHEN OTHER
                MOVE ' GENERICO ' TO WK-MENSAJE-TXT
                MOVE WK-RESP TO WK-MENSAJE-RC
           END-EVALUATE.


       PROCESAR-LBTR.
      *-------------------*
           IF WI3-MONTO IS NOT NUMERIC
              MOVE 'ED' TO WK-CODRET-CHAR                               00001430
              MOVE '** ERROR EN DATA RECIBIDA    ' TO WK-MENSAJE        00012200
              PERFORM ENVIAR-MENSAJE-ERR                                00012210
           END-IF
           INITIALIZE REG-COMMAREA-XBARCA5.
           MOVE WI3-ACCION TO XBA-ACCION.
           MOVE WI3-REFERENCIA TO XBA-REFERENCIA
           MOVE WI3-CODOPER TO XBA-COD-OPERACION.
           IF WI3-MODTRANS = '3'
              MOVE '1' TO WI3-MODTRANS
           END-IF.
           IF WI3-MODTRANS = '2'
              IF WI3-CODOPER = 'C170' AND WI3-MONTO < 1500000
                 MOVE '4' TO WI3-MODTRANS
              END-IF
              IF WI3-CODOPER = 'C101' AND WI3-MONTO < 500000
                 MOVE '4' TO WI3-MODTRANS
              END-IF
           END-IF.
           MOVE WI3-MODTRANS TO XBA-MODO-TRANSFER.
           MOVE WI3-CTAORI TO XBA-CTA-ORIGEN-CTAMYO.
           MOVE WI3-CTAORIBAN TO XBA-CTA-ORIGEN-CDGBCO.
           MOVE WI3-SUBCTAORI TO XBA-CTA-ORIGEN-SUBCTA.
           MOVE WI3-CTADESMON TO XBA-CTA-ORIGEN-MONEDA.
           MOVE WI3-CTADES TO XBA-CTA-DESTINO-CTAMYO.
           MOVE WI3-CTADESBAN TO XBA-CTA-DESTINO-CDGBCO.
           MOVE WI3-SUBCTADES TO XBA-CTA-DESTINO-SUBCTA.
           MOVE WI3-CTADESMON TO XBA-CTA-DESTINO-MONEDA.
           COMPUTE XBA-MONTO-OPERACION = WI3-MONTO / 100.
           MOVE WI3-INSTRUCCION TO XBA-INST-TRANSFERENCIA.
           MOVE WI3-CAJERO TO XBA-REG-CODIGO.
           MOVE WI3-HORA TO XBA-REG-HORA.
           MOVE WI3-TERMINAL TO XBA-REG-TERMINAL.
           EXEC CICS LINK PROGRAM('XBARCA5')
                COMMAREA(REG-COMMAREA-XBARCA5)
                LENGTH(200)
                END-EXEC.
      *
       PROCESAR-CONSULTA.
      *-------------------*
           MOVE ZEROS TO WO4-NUMREG.
           MOVE '99' TO WO4-CODRET.
           PERFORM UBICA-OPERCAS.
           PERFORM UBICA-OPERCAS-04
           PERFORM FIN-UBICA-OPERCAS.
      *
       UBICA-OPERCAS.
      *-------------*
           MOVE LCP-DAY1 TO TT-DD WK-DD
           MOVE LCP-MONTH TO TT-MM WK-MM
           MOVE LCP-CENT TO TT-SS WK-SS
           MOVE LCP-YEAR TO TT-AA WK-AA
           MOVE LOW-VALUES TO TT-NUMOPE
           EXEC CICS STARTBR DATASET('MDCFCAS')
                RIDFLD(TT-CLAVE)
                GTEQ
                RESP(CR)
                END-EXEC
           PERFORM EVALUAR-OPERCAS
           .
       EVALUAR-OPERCAS.
      *-- ------------*
           IF CR EQUAL DFHRESP(NOTOPEN)
              MOVE '02' TO WO4-CODRET
              MOVE 'MDCFCAS CERRADO...¡AVISAR A SISTEMAS!  *' TO
                 WO4-MENSAJE
           END-IF
           IF CR EQUAL DFHRESP(NOTFND)
              MOVE '01' TO WO4-CODRET
              MOVE 'NO EXISTE CONSULTAS EN CAS             *' TO
                 WO4-MENSAJE
           END-IF
      *
           .
       UBICA-OPERCAS-05.
      *-----------------*
           MOVE ZEROS TO WX-NUM-CAS
           PERFORM UNTIL CR NOT EQUAL DFHRESP(NORMAL) OR
              TT-NUMOPE EQUAL WI5-NUMOPE-CAS
                   EXEC CICS READNEXT DATASET('MDCFCAS')
                        INTO (REG-OPERCAS)
                        RIDFLD(TT-CLAVE)
                        RESP(CR)
                        END-EXEC
                   IF TT-NUMOPE = WI5-NUMOPE-CAS
                      MOVE TT-SEMILLA-CAS TO WX-NUM-CAS
                   END-IF
           END-PERFORM
           .
       UBICA-OPERCAS-04.
      *-----------------*
           IF CR = DFHRESP(NORMAL)
              EXEC CICS READNEXT DATASET('MDCFCAS')
                   INTO (REG-OPERCAS)
                   RIDFLD(TT-CLAVE)
                   RESP(CR)
                   END-EXEC
           END-IF
           PERFORM UNTIL CR NOT EQUAL DFHRESP(NORMAL) OR
              TT-FECHA NOT EQUAL WK-FECHA
      *          IF CR = DFHRESP(NORMAL) AND TT-CODRET-CAS NOT = 'X'
                   IF CR = DFHRESP(NORMAL)
                      ADD 1 TO WO4-NUMREG
                      MOVE TT-NUMOPE TO WO4-NUMOPE-CAS(WO4-NUMREG)
                      MOVE TT-CODRET-CAS TO WO4-CODRET-CAS(WO4-NUMREG)
                      MOVE TT-RETENCION TO WO4-FLAG-RETEN(WO4-NUMREG)
                   END-IF
                   EXEC CICS READNEXT DATASET('MDCFCAS')
                        INTO (REG-OPERCAS)
                        RIDFLD(TT-CLAVE)
                        RESP(CR)
                        END-EXEC
           END-PERFORM
           MOVE '00' TO WO4-CODRET
           .
      *
       FIN-UBICA-OPERCAS.
      *-------------------*
           EXEC CICS ENDBR DATASET('MDCFCAS')
                RESP(CR)
                END-EXEC
      *
           MOVE 'CONSULTA CAS CORRECTA                  *' TO
              WO4-MENSAJE.
      *
       PROCESAR-EXTORNO-CONS.
      *-------------------*
           INITIALIZE CASCCA1-AREA-COMUN-CAS.
           PERFORM UBICA-OPERCAS
           PERFORM UBICA-OPERCAS-05
           PERFORM FIN-UBICA-OPERCAS.
           IF TT-NUMOPE = WI5-NUMOPE-CAS
              IF TT-CON-RETENCION
                 MOVE 01 TO CASCCA1-RED
                 MOVE 981 TO CASCCA1-TIENDA-ORIGEN
                 MOVE 'MDCOCOMM' TO CASCCA1-REG-EMPLEADO
                 MOVE 82 TO CASCCA1-CAJERO-SAFE
                 MOVE 04 TO CASCCA1-TIP-OPERACION
                 MOVE 00 TO CASCCA1-CODTRAN
                 MOVE WI5-NUMOPE-CAS TO CASCCA1-NREFERENCIA
                 MOVE WX-NUM-CAS TO CASCCA1-COD-CONSULTA
                 MOVE TT-MONTO TO CASCCA1-IMP-CONSULTA
                 MOVE TT-CUENTA TO CASCCA1-CTACTE-NRO
                 MOVE '03' TO CASCCA1-CTACTE-CTL1
                 MOVE TT-MONEDA TO CASCCA1-CTACTE-CTL2
                 MOVE TT-OFICINA TO CASCCA1-CTACTE-CTL3
                 MOVE TT-PRODUCTO TO CASCCA1-CTACTE-CTL4
                 MOVE 99 TO WO-CODERR-CAS
                 MOVE ' ' TO WO-CODRET-CAS
                 MOVE 00 TO CASCCA1-TIP-CONSULTA
                 MOVE 00 TO CASCCA1-TIP-NIVEL
                 MOVE 00 TO CASCCA1-TIP-SITUACION
                 MOVE 'N' TO CASCCA1-FLAG-MSGTELLER
                 EXEC CICS LINK PROGRAM('CASO103')
                      COMMAREA(CASCCA1-AREA-COMUN-CAS)
                      LENGTH(LENGTH OF CASCCA1-AREA-COMUN-CAS)
                      END-EXEC
                 MOVE CASCCA1-COD-RETORNO TO WO-CODERR-CAS
                 MOVE CASCCA1-VAR-MSG-1 TO WO-MENSAJE-CAS
                 MOVE LCP-DAY1 TO TT-DD
                 MOVE LCP-MONTH TO TT-MM
                 MOVE LCP-CENT TO TT-SS
                 MOVE LCP-YEAR TO TT-AA
                 MOVE WI5-NUMOPE-CAS TO TT-NUMOPE
                 PERFORM LEE-UPDATE-MDCFCAS
                 IF CASCCA1-COD-RETORNO = 00
                    IF CR = DFHRESP(NORMAL)
                       MOVE 'X' TO TT-CODRET-CAS
                                   WO-CODRET-CAS
                       MOVE WTIEMPO TO TT-HORA-RESP
                       MOVE '0' TO TT-RETENCION
                                   WO-FLAG-RETEN
                       PERFORM REGRABA-MDCFCAS
                    END-IF
                 END-IF
              ELSE
                 MOVE ZEROS TO WO-CODRET
                               WO-CODERR-CAS
                 MOVE TT-CODRET-CAS TO WO-CODRET-CAS
                 MOVE TT-RETENCION TO WO-FLAG-RETEN
              END-IF
           ELSE
              MOVE 99 TO WO-CODERR-CAS
              MOVE 'CONSULTA CAS NO ENCONTRADA' TO WO-CODRET-CAS
           END-IF.

       GENERA-TRX-CAS.
      *--------------*
           INITIALIZE CASCCA1-AREA-COMUN-CAS.
           MOVE 99 TO WO-CODERR-CAS
           MOVE ' ' TO WO-CODRET-CAS
           PERFORM OBTIENE-SEMILLA
           MOVE WX-NUM-CAS TO CASCCA1-COD-CONSULTA.
           MOVE 01 TO CASCCA1-RED.
           MOVE 981 TO CASCCA1-TIENDA-ORIGEN.
           MOVE TLDCDD87-CUSERID TO CASCCA1-REG-EMPLEADO.
           MOVE 82 TO CASCCA1-CAJERO-SAFE.
           IF WI-CODOPE = '01'
              MOVE WI-NUMOPE-CAS TO CASCCA1-NREFERENCIA
              MOVE '03' TO CASCCA1-CTACTE-CTL1
              IF WI-CONTROLES NUMERIC
                 MOVE WI-CONTROLES(6:3) TO CASCCA1-CTACTE-CTL2
                 MOVE WI-CONTROLES(9:4) TO CASCCA1-CTACTE-CTL4
              ELSE
                 MOVE ZEROS TO CASCCA1-CTACTE-CTL2
                 MOVE ZEROS TO CASCCA1-CTACTE-CTL4
              END-IF
              MOVE WI-CUENTA-OFI TO CASCCA1-CTACTE-CTL3
              MOVE WI-CUENTA-NRO TO CASCCA1-CTACTE-NRO
              MOVE WK-MONTO TO CASCCA1-IMP-CONSULTA
              IF WI-MEDIO-MONEDA = '01'
                 COMPUTE CASCCA1-IMP-CONSULTA = WI-MEDIO-SOUS / 100
              ELSE
                 COMPUTE CASCCA1-IMP-CONSULTA = WI-MEDIO-ME / 100
              END-IF
601463*       CAMBIO SOLO PARA SPOM COMPRA VENTA
601463        IF WI-PRODUCTO = 'SPOM'
601463           COMPUTE CASCCA1-IMP-CONSULTA = WI-MEDIO-SOUS / 100
601463        END-IF
601463*       CAMBIO SOLO PARA SPOM
           ELSE
              MOVE WI2-NUMOPE-CAS TO CASCCA1-NREFERENCIA
              MOVE ZEROS TO CASCCA1-CTACTE
              MOVE '03' TO CASCCA1-CTACTE-CTL1
              MOVE WI2-MONEDA TO CASCCA1-CTACTE-CTL2
              MOVE WI2-OFICINA TO CASCCA1-CTACTE-CTL3
              MOVE WI2-PRODUCTO TO CASCCA1-CTACTE-CTL4(2:3)
              MOVE WI2-CUENTA TO CASCCA1-CTACTE-NRO
              MOVE WK-MONTO-CAS TO CASCCA1-IMP-CONSULTA
           END-IF
           MOVE CASCCA1-NREFERENCIA TO WX-NUMOPE.
           MOVE 00 TO CASCCA1-TIP-CONSULTA
           MOVE 00 TO CASCCA1-TIP-NIVEL
           MOVE 00 TO CASCCA1-TIP-SITUACION.
           MOVE 00 TO CASCCA1-TIP-OPERACION.
           MOVE 00 TO CASCCA1-CODTRAN.
           MOVE 'MDCC' TO CASCCA1-START-TRX.
           MOVE 'N' TO CASCCA1-FLAG-MSGTELLER.
      *
           EXEC CICS LINK PROGRAM('CASO100')
                COMMAREA(CASCCA1-AREA-COMUN-CAS)
                LENGTH(LENGTH OF CASCCA1-AREA-COMUN-CAS)
                END-EXEC.
      *
           IF CASCCA1-COD-RETORNO = 00
              EXEC CICS LINK PROGRAM('CASO101')
                   COMMAREA(CASCCA1-AREA-COMUN-CAS)
                   LENGTH(LENGTH OF CASCCA1-AREA-COMUN-CAS)
                   END-EXEC
              MOVE CASCCA1-COD-RETORNO TO WO-CODERR-CAS
              MOVE CASCCA1-VAR-MSG-1 TO WO-MENSAJE-CAS
              IF CASCCA1-COD-RETORNO = 00
                 PERFORM GRABA-TRANS-CAS
                 IF CR = DFHRESP(DUPREC)
                    PERFORM LEE-UPDATE-MDCFCAS
                    IF CR = DFHRESP(NORMAL)
                       MOVE TT-CODRET-CAS TO WO-CODRET-CAS
                       MOVE TT-RETENCION TO WO-FLAG-RETEN
                       MOVE WX-NUM-CAS TO TT-SEMILLA-CAS
                       MOVE WTIEMPO TO TT-HORA-GRAB
                       MOVE WK-DETALLES-CONSULTA TO TT-DETALLES-CONSULTA
                       PERFORM REGRABA-MDCFCAS
                    END-IF
                 END-IF
              ELSE
                 MOVE ' ' TO WO-CODRET-CAS
              END-IF
           ELSE
              MOVE CASCCA1-COD-RETORNO TO WO-CODERR-CAS
              MOVE CASCCA1-VAR-MSG-1 TO WO-MENSAJE-CAS
           END-IF.
      *
       GRABA-TRANS-CAS.
      *---------------*
           INITIALIZE REG-OPERCAS.
           MOVE LCP-DAY1 TO TT-DD
           MOVE LCP-MONTH TO TT-MM
           MOVE LCP-CENT TO TT-SS
           MOVE LCP-YEAR TO TT-AA
           MOVE WX-NUMOPE TO TT-NUMOPE.
           MOVE WX-NUM-CAS TO TT-SEMILLA-CAS.
           MOVE '1' TO TT-RETENCION
                       WO-FLAG-RETEN
           MOVE 'C' TO TT-CODRET-CAS
                       WO-CODRET-CAS.
           MOVE WTIEMPO TO TT-HORA-GRAB.
           MOVE SPACES TO TT-HORA-RESP.
           MOVE CASCCA1-CTACTE-CTL2 TO TT-MONEDA
           MOVE CASCCA1-CTACTE-CTL4 TO TT-PRODUCTO
           MOVE CASCCA1-CTACTE-CTL3 TO TT-OFICINA
           MOVE CASCCA1-CTACTE-NRO TO TT-CUENTA
           MOVE CASCCA1-IMP-CONSULTA TO TT-MONTO
           MOVE TT-DETALLES-CONSULTA TO WK-DETALLES-CONSULTA
           EXEC CICS WRITE FILE('MDCFCAS')
                FROM (REG-OPERCAS)
                RIDFLD(TT-CLAVE)
                RESP(CR)
                END-EXEC.
      *
       LEE-MDCFCAS.
      *-----------*
           EXEC CICS READ DATASET('MDCFCAS')
                INTO (REG-OPERCAS)
                RIDFLD(TT-CLAVE)
                EQUAL
                RESP(CR)
                END-EXEC.
           MOVE CR TO WK-CODERR-CAS
           IF CR NOT = 00
              IF CR = DFHRESP(NOTFND)
                 MOVE 'REGISTRO NO ENCONTRADO   ' TO WK-MENSAJE-CAS
              ELSE
                 IF CR = DFHRESP(NOTOPEN)
                    MOVE 'ARCHIVO MDCFCAS CERRADO  ' TO WK-MENSAJE-CAS
                 ELSE
                    MOVE 'ERROR ARCHIVO MDCFCAS    ' TO WK-MENSAJE-CAS
                 END-IF
              END-IF
           END-IF
           .
      *
       OBTIENE-SEMILLA.
      *---------------*
           PERFORM LEE-UPDATE-SEMILLA-PPA.
           MOVE PAR-SEMICAS TO WX-NUM-CAS.
           ADD 1 TO WX-ORDC-NOPE.
           MOVE WX-NUM-CAS TO PAR-SEMICAS.
           PERFORM REGRABA-SEMILLA-PPA.
      *
       LEE-UPDATE-MDCFCAS.
      *------------------*
           EXEC CICS READ DATASET('MDCFCAS')
                INTO (REG-OPERCAS)
                RIDFLD(TT-CLAVE)
                UPDATE
                EQUAL
                RESP(CR)
                END-EXEC.
      *
       REGRABA-MDCFCAS.
      *--------------------*
           EXEC CICS REWRITE DATASET('MDCFCAS')
                FROM (REG-OPERCAS)
                RESP(CR)
                END-EXEC.
      *
       LEE-UPDATE-SEMILLA-PPA.
      *----------------------*
           MOVE 'SEMI' TO PAR-KEY.
           EXEC CICS READ DATASET('MDCFPARA')
                INTO (REG-PARAMETROS)
                RIDFLD(PAR-KEY)
                UPDATE
                EQUAL
                RESP(CR)
                END-EXEC.
      *
       REGRABA-SEMILLA-PPA.
      *--------------------*
           EXEC CICS REWRITE DATASET('MDCFPARA')
                FROM (REG-PARAMETROS)
                RESP(CR)
                END-EXEC.
      *
       PROCESA-RETENCION-IM.
      *--------------------*
      *
      *       SIMO111 : RESTA RETENCION EN TRAMITE
      *
           MOVE WK-CTACTE TO CASCCA2-CTACTE
           MOVE WK-MONTO TO CASCCA2-IMP-CONSULTA
           EXEC CICS LINK PROGRAM('SIMO111')
                COMMAREA(CASCCA2-AREA-COMUN-CAS)
                LENGTH(800)
                END-EXEC.
           IF CASCCA2-COD-RETORNO NOT = 00
           NEXT SENTENCE
           END-IF.
      *
FO2515 BUSCA-CCER001.
FO2515*-------------
FO2515     INITIALIZE WF-COMM-CCE
FO2515     MOVE 'A' TO CCE-ACCESO
FO2515     MOVE RMCMRTCS-TIN-CD TO CCE-TIPO-DOC(1)
FO2515     MOVE RMCMRTCS-TIN TO CCE-NRO-DOC(1)
FO2515     IF RMCMRTCS-CUST-TYP-CD = 'P'
FO2515        MOVE RMCMRTCS-PATERNAL-NAME TO CCE-APE-PAT(1)
FO2515        MOVE RMCMRTCS-MATERNAL-NAME TO CCE-APE-MAT(1)
FO2515        MOVE RMCMRTCS-FIRST-NAME TO CCE-1ER-NOMB(1)
FO2515        MOVE RMCMRTCS-SECOND-NAME TO CCE-2DO-NOMB(1)
FO2515     END-IF
FO2515     MOVE RMCMRTCS-NAME-LINE-1 TO CCE-NOMB-PJ(1)
FO2515     PERFORM RUTINA-ALERTA.

FO2515 RUTINA-ALERTA.
FO2515*--------------
903312     IF WI-CODOPE = '07'
903312        MOVE SPACES TO WK-CODRET-CHAR
903312     END-IF
FO2515     MOVE WI-USUARIO TO CCE-USUARIO.
FO2515     MOVE EIBTRMID TO CCE-TERMINAL.
FO2515     MOVE 'MCD ' TO CCE-ORIGEN.
FO2515     MOVE 'MDCOCOMM' TO CCE-PROGRAMA.
FO2515     MOVE '981' TO CCE-TIENDA.
FO2515     EXEC CICS LINK PROGRAM('CCER001')
FO2515          COMMAREA(WF-COMM-CCE)
FO2515          LENGTH(LENGTH OF WF-COMM-CCE)
FO2515          RESP(WE-RESP)
FO2515          END-EXEC.
FO2515     IF WE-RESP NOT EQUAL DFHRESP(NORMAL)
FO2515        MOVE 'ERROR AL LLAMAR A RUTINA CCER001' TO WK-MENSAJE
FO2515        PERFORM ENVIAR-MENSAJE-ERR
FO2515     ELSE
FO2515        IF CCE-RETURN-CODE-1 = '00'
FO2515           IF CCE-CRITICIDAD = 'R1'
903312              IF WI-CODOPE = '07'
W18070                 MOVE 00 TO WK-RC
903312              ELSE
W18070                 MOVE 10 TO WK-RC
903312              END-IF
W18070              MOVE 'R1' TO WK-CODRET-CHAR
FO2515              MOVE 'ALERTA ORDE/BENEF - R1 - CLIENTE ESPECIAL'
FO2515                 TO WK-MENSAJE
903312              IF WI-CODOPE = '07'
903312                 THEN
903312                 MOVE WK-CODRET-CHAR TO WO-CODRET-R1(I)
903312                 MOVE WI-TIP-CLIE(I) TO WO-TIP-CLIE-E(I)
903312                 MOVE WI-TIP-DOC(I) TO WO-TIP-DOC-E(I)
903312                 MOVE WI-NUM-DOC(I) TO WO-NUM-DOC-E(I)
903312                 MOVE WI-NOM-CLIE(I) TO WO-NOM-CLIE-E(I)
903312                 MOVE 1 TO SW-ERR-07
903312              ELSE
FO2515                 PERFORM ENVIAR-MENSAJE-ERR
903312              END-IF
903312           ELSE
903312              IF WI-CODOPE = '07'
903312                 MOVE '00' TO WO-CODRET-R1(I)
903312                 MOVE WI-TIP-CLIE(I) TO WO-TIP-CLIE-E(I)
903312                 MOVE WI-TIP-DOC(I) TO WO-TIP-DOC-E(I)
903312                 MOVE WI-NUM-DOC(I) TO WO-NUM-DOC-E(I)
903312                 MOVE WI-NOM-CLIE(I) TO WO-NOM-CLIE-E(I)
903312              END-IF
FO2515           END-IF
903312        ELSE
903312           IF WI-CODOPE = '07'
903312              MOVE CCE-RETURN-CODE-1 TO WO-CODRET-R1(I)
903312              MOVE WI-TIP-CLIE(I) TO WO-TIP-CLIE-E(I)
903312              MOVE WI-TIP-DOC(I) TO WO-TIP-DOC-E(I)
903312              MOVE WI-NUM-DOC(I) TO WO-NUM-DOC-E(I)
903312              MOVE WI-NOM-CLIE(I) TO WO-NOM-CLIE-E(I)
903312              MOVE 1 TO SW-ERR-07
903312           END-IF
FO2515        END-IF
FO2515     END-IF.


      *         ...  NUEVAS RUTINAS  ...                                00002650
      *                                                                 00002650
500417 800-REGISTRO-BITACORA.                                           00002650
500417*----------------------+                                          00002650
500417*                                                                 00002650
500417     INITIALIZE DCLMDC-CABO                                       02650
500417     PERFORM 805-PREPARA-DATOS-COMUNES                            00002650
500417                                                                  00002650
500417*    ....  VERIFICAMOS  SI TRANSACCION ED DE CARGOS Y ABONOS .... 00002650
500417                                                                  00002650
500417     EVALUATE TLDCDD87-CTRANSID                                   00002650
500417     WHEN '0892'                                                  00002650
500417     WHEN '1892'                                                  00002650
500417     WHEN '0222'                                                  00002650
500417          MOVE 'CARGO' TO TI-OPER OF DCLMDC-CABO                  02650
500417          PERFORM 810-PREPARA-DATA-BITACORA                       00002650
500417          PERFORM 820-EJECUTA-INSERT-BITACORA                     00002650
500417                                                                  00002650
500417     WHEN '0832'                                                  00002650
500417     WHEN '1832'                                                  00002650
500417     WHEN '0211'                                                  00002650
500417          MOVE 'ABONO' TO TI-OPER OF DCLMDC-CABO                  02650
500417          PERFORM 810-PREPARA-DATA-BITACORA                       00002650
500417          PERFORM 820-EJECUTA-INSERT-BITACORA                     00002650
500417                                                                  00002650
500417     END-EVALUATE.                                                00002650
500417                                                                  00002650
500417                                                                  00002650
500417 805-PREPARA-DATOS-COMUNES.                                       00002650
500417*--------------------------*                                      00002660
500417*                                                                 00002650
500417     MOVE TLDCDD87-FTERM(1:4) TO WOR-FEC-DB2-SA                   00002650
500417     MOVE TLDCDD87-FTERM(5:2) TO WOR-FEC-DB2-MM                   00002650
500417     MOVE TLDCDD87-FTERM(7:2) TO WOR-FEC-DB2-DD                   00002650
500417     MOVE '.' TO WOR-FEC-DB2-P1 WOR-FEC-DB2-P2                    00002650
500417     MOVE WOR-FEC-DB2 TO FE-PROC OF DCLMDC-CABO.                  02650
500417                                                                  00002650
500417     MOVE WK-NRO-SOLICITUD TO NU-OPER OF DCLMDC-CABO.             02650
500417*    MOVE TLDCDD87-CLOG-TERM-EXT TO NU-OPER     OF DCLMDC-CABO.   02650
500417                                                                  00002650
500417     MOVE TLDCDD87-HTERM(1:2) TO WOR-HOR-DB2-HH                   00002650
500417     MOVE TLDCDD87-HTERM(3:2) TO WOR-HOR-DB2-MM                   00002650
500417     MOVE TLDCDD87-HTERM(5:2) TO WOR-HOR-DB2-SS.                  00002650
500417     MOVE '.' TO WOR-HOR-DB2-P1 WOR-HOR-DB2-P2.                   00002650
500417     MOVE WOR-HOR-DB2 TO HO-PROC OF DCLMDC-CABO.                  02650
500417                                                                  00002650
500417     MOVE TLDCDD87-CLOG TO NU-TRX-TLD OF DCLMDC-CABO.             02650
500417     MOVE TLDCDD87-CTRANSID TO TI-TRX-TLD OF DCLMDC-CABO.         02650
500417     MOVE TLDCDD87-CUSERID TO CO-USER OF DCLMDC-CABO.             02650
500417     MOVE TLDCDD87-CLOG-EXT TO NU-TRX-TLD-EXTOR                   00002650
500417        OF DCLMDC-CABO.                                           02650
500417     MOVE WOR-RC-TLD TO RC-TLD OF DCLMDC-CABO.                    00002650
500417     MOVE WOR-DE-RC-TLD TO DE-RC-TLD OF DCLMDC-CABO.              00002650
500417                                                                  00002650
500417                                                                  00002650
500417                                                                  00002650
500417 810-PREPARA-DATA-BITACORA.                                       00002650
500417*--------------------------*                                      00002660
500417     EVALUATE WI-CODOPE                                           00002650
500417     WHEN '01'                                                    00002650
500417*                   ... PROCESO COMPRA-VTA                        00002650
500417          PERFORM 830-PREPARA-DATA-CV                             00002650
500417     WHEN '02'                                                    00002650
500417*                   ... PROCESO TRANSFERENCIA                     00002650
500417          PERFORM 840-PREPARA-DATA-TRANSFERENCIA                  00002650
500417     END-EVALUATE.                                                00002650
500417                                                                  00002650
500417                                                                  00002650
500417 820-EJECUTA-INSERT-BITACORA.                                     00002650
500417*----------------------------*                                    00002660
           EXEC SQL INSERT INTO
                MDC_CABO(FE_PROC, NU_OPER, HO_PROC,
                TI_TRX_TLD, TI_OPER, IL_EXTOR, CO_MONE_OPER,
                IM_OPER, CO_CLIE, NU_CNTA, NU_TRX_TLD,
                NU_TRX_TLD_EXTOR, RC_TLD, DE_RC_TLD, CO_USER
                )
                VALUES
                (:DCLMDC-CABO.FE-PROC,
                :DCLMDC-CABO.NU-OPER,
                :DCLMDC-CABO.HO-PROC,
                :DCLMDC-CABO.TI-TRX-TLD,
                :DCLMDC-CABO.TI-OPER,
                :DCLMDC-CABO.IL-EXTOR,
                :DCLMDC-CABO.CO-MONE-OPER,
                :DCLMDC-CABO.IM-OPER,
                :DCLMDC-CABO.CO-CLIE,
                :DCLMDC-CABO.NU-CNTA,
                :DCLMDC-CABO.NU-TRX-TLD,
                :DCLMDC-CABO.NU-TRX-TLD-EXTOR,
                :DCLMDC-CABO.RC-TLD,
                :DCLMDC-CABO.DE-RC-TLD,
                :DCLMDC-CABO.CO-USER
                )
                END-EXEC.
           EVALUATE SQLCODE
           WHEN 0
                CONTINUE
           WHEN OTHER
                MOVE 'ERROR INSERT TAB TP-MDC-CABO' TO WK-MENSAJE
                MOVE 'SQLCODE: ' TO WK-MENSAJE-TXT
                MOVE SQLCODE TO WK-MENSAJE-RC
           END-EVALUATE.
500417*                                                                 00002660
      *                                                                 00002660
500417 830-PREPARA-DATA-CV.                                             00002650
500417*--------------------*                                            00002660
500417*                                                                 00002660
500417*       ... VERIFICAMOS SI TRX , ES UN EXTORNO  ...               00002660
500417     IF WI2-TRAN-TOLD = '2900'                                    00002650
500417        MOVE 'S' TO IL-EXTOR OF DCLMDC-CABO                       02650
500417        MOVE TLDCDD87-CLOG-EXT TO NU-TRX-TLD-EXTOR                00002650
500417           OF DCLMDC-CABO                                         02650
500417        MOVE TLDCDD87-CLOG TO NU-TRX-TLD OF DCLMDC-CABO           02650
500417     END-IF.                                                      00002650
500417                                                                  00002650
500417                                                                  00002650
500417*     IMPORTE DEL CARGO / ABONO                                   00002660
500417     MOVE WOR-IMP-CARGO-ABONO TO IM-OPER OF DCLMDC-CABO.          02650
500417                                                                  00002650
500417*     CODIGO MONEDA OPERACION                                     00002660
500417     MOVE WOR-MONEDA-ORIGEN TO CO-MONE-OPER OF DCLMDC-CABO.       650
500417                                                                  00002650
500417*     NRO DE CUENTA                                               00002660
500417     MOVE '03' TO WOR-CONTROL-BCO                                 00002660
500417     MOVE WI-CONTROLES(6:3) TO WOR-CONTROL-MON                    00002660
500417     MOVE WI-CUENTA-OFI TO WOR-CONTROL-OFIC                       00002660
500417     MOVE WI-CONTROLES(9:4) TO WOR-CONTROL-PROD                   00002660
500417     MOVE WI-CUENTA-NRO TO WOR-NRO-CUENTA                         00002660
500417     MOVE WOR-CONTROLES-CTA-IBK TO NU-CNTA OF DCLMDC-CABO.        02660
500417                                                                  00002660
500417                                                                  00002650
500417 840-PREPARA-DATA-TRANSFERENCIA.                                  00002650
500417*-------------------------------*                                 00002660
500417*                                                                 00002660
500417     MOVE TLDCDD87-GEXT TO IL-EXTOR OF DCLMDC-CABO.               02650
500417*                                                                 00002660
500417*     IMPORTE DEL CARGO / ABONO                                   00002660
500417     MOVE WOR-IMP-CARGO-ABONO TO IM-OPER OF DCLMDC-CABO.          02650
500417                                                                  00002650
500417*     NRO DE CUENTA                                               00002660
500417     MOVE WE-CONTROL-1 TO WOR-CONTROL-BCO                         00002660
500417     MOVE WE-CONTROL-2 TO WOR-CONTROL-MON                         00002660
500417     MOVE WE-COD-TIENDA TO WOR-CONTROL-OFIC                       00002660
500417     MOVE WE-CONTROL-4 TO WOR-CONTROL-PROD                        00002660
500417     MOVE WE-NUMERO-CTA(4:10) TO WOR-NRO-CUENTA                   00002660
500417     MOVE WOR-CONTROLES-CTA-IBK TO NU-CNTA OF DCLMDC-CABO.        02660
500417                                                                  00002660
500417*                                                                 00002660
500417     IF TLDCDD87-GEXT = 'S'                                       00002650
500417        MOVE TLDCDD87-CLOG-EXT TO NU-TRX-TLD-EXTOR                00002650
500417           OF DCLMDC-CABO                                         02650
500417        MOVE TLDCDD87-CLOG TO NU-TRX-TLD                          00002650
500417           OF DCLMDC-CABO                                         02650
500417     END-IF.                                                      00002650
      *                                                                 00002650
500417 PROCESO-ACTUALIZA-MDCFTOLD.
      *----------------------------
      *
500417     MOVE REG-SOLICITUDES-MDC TO WOR-REG-SOLICITUDES-MDC
500417     PERFORM LEER-SOLICITUD-UPD
500417     MOVE WOR-REG-SOLICITUDES-MDC TO REG-SOLICITUDES-MDC
500417     PERFORM REGRABAR-SOLICITUD
           .

500417
500417 LEER-SOLICITUD.
500417*---------------*
500417     EXEC CICS
500417          READ
500417          FILE(WK-MDCFTOLD)
500417          INTO (REG-SOLICITUDES-MDC)
500417          RIDFLD(WK-CLAVE)
500417          RESP(WK-RESP)
500417          END-EXEC
500417     IF WK-RESP EQUAL DFHRESP(NOTFND)
500417        IF WK-FLAG-INI NOT EQUAL 'S'
500417           MOVE 'NF' TO WK-CODRET-CHAR
500417           MOVE 'ER SOLICITUD NO HAY MDCFTOLD' TO WK-MENSAJE
500417           MOVE 10 TO WK-RC
500417           PERFORM VERIFICO-MENSAJE-MDCFTOLD
500417        END-IF
500417     ELSE
500417        IF WK-RESP NOT EQUAL DFHRESP(NORMAL)
500417           MOVE 'SB' TO WK-CODRET-CHAR
500417           MOVE 'ERROR LEER SOLICIT MDCFTOLD' TO WK-MENSAJE
500417           MOVE 10 TO WK-RC
500417           PERFORM VERIFICO-MENSAJE-MDCFTOLD
500417        END-IF
500417     END-IF
500417     .
888888 GRABACION-CAMFLOG.
888888*-------------------*
888888*
888888     INITIALIZE REG-CAMFLOG.
888888     STRING LCP-CENT
888888            LCP-YEAR
888888            LCP-MONTH
888888            LCP-DAY1
888888        DELIMITED BY SIZE INTO REG-FECHA-REAL

888888     MOVE WTIEMPO                 TO REG-HORAS-REAL
888888
888888     MOVE DFHCOMMAREA             TO  REG-LOG-MDCOCOMM
888888*
888888     EXEC CICS WRITE   DATASET ('CAMFLOG')
888888                       FROM    (REG-CAMFLOG)
888888                       LENGTH  (WC-LOG-LENGTH)
888888                       RBA
888888                       RIDFLD  (WE-KEY-RBA)
888888                       RESP  (WS-RESP)
888888                       RESP2 (WS-RESP2)
888888                       END-EXEC.
888888*
W18070 RETORNAR.                                                        00002650
W18070*--------*                                                        00002660
W18070     MOVE WO-HOST-PARAMETROS TO DFHCOMMAREA                       00001000
W18070     EXEC CICS RETURN                                             00002670
W18070          END-EXEC.                                               00002680
      *
      *
      *--------------------------------------------------------------*
      *--------------------- FIN   MDCOCOMM -------------------------*
      *--------------------------------------------------------------*

