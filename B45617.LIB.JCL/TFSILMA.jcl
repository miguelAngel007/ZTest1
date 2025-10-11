//B45617TF JOB (B45617),'PROYECTO.FIN.TFSILMA',CLASS=9,MSGCLASS=A
//SORTORIG EXEC PGM=SORT
//SORTIN  DD DSN=D.CDC.G01SN.CDC.CDCPG01.CDCCON.ACUMDIA1,DISP=SHR
//SORTOUT DD DSN=D.CDC.G01.F.ACUMSORT,
//           DISP=(NEW,CATLG,DELETE),
//           SPACE=(CYL,(10,5)),UNIT=SYSDA,DCB=(RECFM=FB)
//SYSIN DD *
 SORT FIELDS=(45,19,CH,A,
              225,15,CH,A,
              305,3,CH,A,
              70,12,CH,A,
              124,4,CH,A,
              118,6,CH,A)
/*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//JOIN1    EXEC PGM=SORT
//SORTJNF1 DD DSN=D.CDC.G01.F.ACUMSORT,DISP=SHR
//SORTJNF2 DD DSN=D.CDC.G01SN.CDC.CDCP188.PLNTOT,DISP=SHR
//SORTOUT  DD DSN=D.CDC.G01.F.INTSORT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(10,5)),DCB=(RECFM=FB)
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(45,16,A)
  JOINKEYS FILE=F2,FIELDS=(1,16,A)

  JOIN UNPAIRED,F1

  REFORMAT FIELDS=(F1:1,26,
                   F1:27,4,
                   F1:33,3,
                   F1:38,3,
                   F1:41,4,
                   F1:45,19,
                   F1:64,6,
                   F1:70,12,
                   F1:140,4,
                   F1:144,3,
                   F1:147,2,
                   F1:225,15,
                   F1:240,40,
                   F1:305,3,
                   F1:1737,255,
                   F2:24,10)

  SORT FIELDS=COPY
/*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
