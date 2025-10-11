//B45617IT JOB (B45617),'MIGUEL.ANGEL.CHAVEZ',CLASS=9,MSGCLASS=S
//* RUN COB PROGRAM
//STEP1     EXEC PGM=CDCINPLZ
//CEEOPTS   DD *
  TEST(,,,DBMDT%B45617:)
/*
//STEPLIB   DD DSN=B45617.COBOL.LOAD.COB360,DISP=SHR
//CUENTAS   DD DSN=B45617.OW.SOURCE(CUENTAS),DISP=SHR
//TASAS     DD DSN=B45617.OW.SOURCE(TASAS),DISP=SHR
//CTASINT   DD DSN=B45617.OW.SOURCE(CTASINT),DISP=SHR
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//* ICE TOOL REPORT
//STEP2   EXEC PGM=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//INFILE   DD DSN=B45617.OW.SOURCE(CTASINT),DISP=SHR
//OUTMONTH  DD DSN=B45617.IPZ.MONTH,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(10,5)),DCB=(RECFM=FB,LRECL=200)
//OUTYEAR DD DSN=B45617.IPZ.YEAR,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(10,5)),DCB=(RECFM=FB,LRECL=200)
//TOOLIN   DD *
  SORT FROM(INFILE) TO(OUTMONTH) USING(CTL1)
  SORT FROM(INFILE) TO(OUTYEAR) USING(CTL2)
/*
//CTL1CNTL DD *
  SORT FIELDS=(1,3,CH,A,4,4,CH,A,8,2,CH,A,10,2,CH,A)
  SUM FIELDS=NONE
  OUTFIL REMOVECC,
  HEADER1=(C'UID ',
           C'NOMBRE          ',
           C'APELLIDO        ',
           C'  FECHA     ',
           C'          MONTO',
           C'          INTERES',
           C'            TOTAL'),
  HEADER2=(C' '),
  SECTIONS=(1,9,
    TRAILER3=(C'----------------------------------------------------',
              C'---------------------------------------------',/,1,3,
              C' PERIODO: ',4,4,C'/',8,2,27X,
              TOT=(42,10,ZD,EDIT=($IIII,III,III.TT)),
              11X,
              TOT=(52,10,ZD,EDIT=($TT.TT)),1X,
              TOT=(62,10,ZD,EDIT=($IIII,III,III.TT)),/)),
  BUILD=(1,3,1X,12,15,1X,27,15,1X,
         4,4,C'/',8,2,C'/',10,2,1X,
         42,10,ZD,EDIT=($IIII,III,III.TT),
         1X,
         52,10,ZD,EDIT=($IIII,III,III.TT),
         1X,
         62,10,ZD,EDIT=($IIII,III,III.TT),
         103X)
/*
//CTL2CNTL DD *
  SORT FIELDS=(1,3,CH,A,4,4,CH,A)
  OUTFIL REMOVECC,
  HEADER1=(C'RESUMEN ANUAL POR CLIENTE',/,
       C'UID ',
       C'NOMBRE          ',
       C'APELLIDO        ',
       C'  FECHA     ',
       C'          MONTO',
       C'          INTERES',
       C'            TOTAL'),
   HEADER2=(C' '),
    SECTIONS=(1,7,
      TRAILER3=(C'----------------------------------------',
               C'----------------------------------------',
               C'-----------------',/,1,3,
               C' TOTAL AÑO ',4,4,30X,
                TOT=(42,10,ZD,EDIT=($IIII,III,III.TT)),1X,
                TOT=(52,10,ZD,EDIT=($IIII,III,III.TT)),
                TOT=(62,10,ZD,EDIT=($IIII,III,III.TT)),/)),
    BUILD=(1,3,1X,
           12,15,1X,
           27,15,1X,
           4,4,C'/',8,2,C'/',10,2,1X,
           42,10,ZD,EDIT=($IIII,III,III.TT),
           1X,
           52,10,ZD,EDIT=($IIII,III,III.TT),1X,
           62,10,ZD,EDIT=($IIII,III,III.TT),
           103X)
/*



