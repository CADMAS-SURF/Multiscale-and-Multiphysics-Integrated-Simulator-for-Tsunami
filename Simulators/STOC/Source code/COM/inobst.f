      SUBROUTINE INOBST
C======================================================================
C     形状データを読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'OBSTR.h'
C
      CHARACTER(3)::CTMP
      REAL(8)::RTMP(30)
      INTEGER::ITMP(20),inside
C
      INTEGER::I,IE,IERR,IS,LSEA,N,NDAT1,NFPRS,IDUM,JDUM
C
      LSEA = 0
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:inobst:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FILE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'YES' ) THEN
               LOBST = 1
            ELSE IF( CTMP.EQ.'NO ' ) THEN
               LOBST = 0
            ELSE
               CALL ERRMSG('INOBST',6640)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=FILE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'D-FILE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'YES' ) THEN
               LDPRS = 1
            ELSE IF( CTMP.EQ.'NO ' ) THEN
               LDPRS = 0
            ELSE
               CALL ERRMSG('INOBST',6641)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=D-FILE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SOLID' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 6 ) THEN
               CALL ERRMSG('INOBST',6642)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 6'
               WRITE(LP,*) 'VARIABLE=SOLID'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            CALL MODIJ(ITMP(1),ITMP(2),ITMP(3),ITMP(4),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOBSS = NOBSS + 1
            IF( NOBSS.GT.NOBSSZ ) THEN
               CALL ERRMSG('INOBST',6643)
               WRITE(LP,*) 'THE NUMBER OF SOLID OBSTACLE MAY ',
     $                     'NOT BE OVER NOBSSZ'
               WRITE(LP,*) 'VARIABLE=SOLID'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IOBSS(1,NOBSS) = ITMP(1)
            IOBSS(2,NOBSS) = ITMP(2)
            IOBSS(3,NOBSS) = ITMP(3)
            IOBSS(4,NOBSS) = ITMP(4)
            IOBSS(5,NOBSS) = ITMP(5)
            IOBSS(6,NOBSS) = ITMP(6)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PLATE-I' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INOBST',6644)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=PLATE-I'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IDUM=ITMP(1)
            CALL MODIJ(ITMP(1),IDUM,ITMP(2),ITMP(3),2,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOBSP = NOBSP + 1
            IF( NOBSP.GT.NOBPSZ ) THEN
               CALL ERRMSG('INOBST',6645)
               WRITE(LP,*) 'THE NUMBER OF PLATE OBSTACLE MAY ',
     $                     'NOT BE OVER NOBPSZ'
               WRITE(LP,*) 'VARIABLE=PLATE-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IOBSP(1,NOBSP) = ITMP(1)
            IOBSP(2,NOBSP) = ITMP(1)
            IOBSP(3,NOBSP) = ITMP(2)
            IOBSP(4,NOBSP) = ITMP(3)
            IOBSP(5,NOBSP) = ITMP(4)
            IOBSP(6,NOBSP) = ITMP(5)
            IOBSP(7,NOBSP) = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PLATE-J' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INOBST',6646)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=PLATE-J'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            JDUM=ITMP(1)
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(1),JDUM,3,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOBSP = NOBSP + 1
            IF( NOBSP.GT.NOBPSZ ) THEN
               CALL ERRMSG('INOBST',6647)
               WRITE(LP,*) 'THE NUMBER OF PLATE OBSTACLE MAY ',
     $                     'NOT BE OVER NOBPSZ'
               WRITE(LP,*) 'VARIABLE=PLATE-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IOBSP(1,NOBSP) = ITMP(2)
            IOBSP(2,NOBSP) = ITMP(3)
            IOBSP(3,NOBSP) = ITMP(1)
            IOBSP(4,NOBSP) = ITMP(1)
            IOBSP(5,NOBSP) = ITMP(4)
            IOBSP(6,NOBSP) = ITMP(5)
            IOBSP(7,NOBSP) = 2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PLATE-K' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INOBST',6648)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=PLATE-K'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(4),ITMP(5),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOBSP = NOBSP + 1
            IF( NOBSP.GT.NOBPSZ ) THEN
               CALL ERRMSG('INOBST',6649)
               WRITE(LP,*) 'THE NUMBER OF PLATE OBSTACLE MAY ',
     $                     'NOT BE OVER NOBPSZ'
               WRITE(LP,*) 'VARIABLE=PLATE-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IOBSP(1,NOBSP) = ITMP(2)
            IOBSP(2,NOBSP) = ITMP(3)
            IOBSP(3,NOBSP) = ITMP(4)
            IOBSP(4,NOBSP) = ITMP(5)
            IOBSP(5,NOBSP) = ITMP(1)
            IOBSP(6,NOBSP) = ITMP(1)
            IOBSP(7,NOBSP) = 3
C
         ELSE IF( CLINE(IS:IE) .EQ. 'POROUS' .OR.
     $            CLINE(IS:IE) .EQ. 'F-POROUS'  ) THEN
            NFPRS = 0
            IF(CLINE(IS:IE).EQ.'F-POROUS') NFPRS=1
            CALL MGETR(RTMP,NDAT1,30)
            IF( NDAT1 .NE. 16 ) THEN
               CALL ERRMSG('INOBST',6650)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 16'
               WRITE(LP,*) 'VARIABLE=POROUS'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            ITMP(1)=NINT(RTMP(1))
            ITMP(2)=NINT(RTMP(2))
            ITMP(3)=NINT(RTMP(3))
            ITMP(4)=NINT(RTMP(4))
            ITMP(5)=NINT(RTMP(5))
            ITMP(6)=NINT(RTMP(6))
C
            CALL MODIJ(ITMP(1),ITMP(2),ITMP(3),ITMP(4),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NPORS = NPORS + 1
            IF( NPORS.GT.NPRSSZ ) THEN
               CALL ERRMSG('INOBST',6651)
               WRITE(LP,*) 'THE NUMBER OF POROUS AREA MAY ',
     $                     'NOT BE OVER NPRSSZ'
               WRITE(LP,*) 'VARIABLE=POROUS'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IPORS(1,NPORS) = ITMP(1)
            IPORS(2,NPORS) = ITMP(2)
            IPORS(3,NPORS) = ITMP(3)
            IPORS(4,NPORS) = ITMP(4)
            IPORS(5,NPORS) = ITMP(5)
            IPORS(6,NPORS) = ITMP(6)
            IPORS(7,NPORS) = NFOBS
            IF(NFOBS.EQ.0) IPORS(7,NPORS)=1
            IF(NFPRS.EQ.0) IPORS(7,NPORS)=0
            RPORS(1,NPORS) = RTMP(7)
            RPORS(2,NPORS) = RTMP(8)
            RPORS(3,NPORS) = RTMP(9)
            RPORS(4,NPORS) = RTMP(10)
            RPORS(5,NPORS) = RTMP(11)
            RPORS(6,NPORS) = RTMP(12)
            RPORS(7,NPORS) = RTMP(13)
            RPORS(8,NPORS) = RTMP(14)
            RPORS(9,NPORS) = RTMP(15)
            RPORS(10,NPORS)= RTMP(16)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'D-POROUS'  ) THEN
            CALL MGETR(RTMP,NDAT1,30)
            IF( NDAT1 .NE. 21 ) THEN
               CALL ERRMSG('INOBST',6652)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 21'
               WRITE(LP,*) 'VARIABLE=D-POROUS'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            ITMP(1)=NINT(RTMP(1))
            ITMP(2)=NINT(RTMP(2))
            ITMP(3)=NINT(RTMP(3))
            ITMP(4)=NINT(RTMP(4))
            ITMP(5)=NINT(RTMP(5))
            ITMP(6)=NINT(RTMP(6))
C
            CALL MODIJ(ITMP(1),ITMP(2),ITMP(3),ITMP(4),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NPORS = NPORS + 1
            IF( NPORS.GT.NPRSSZ ) THEN
               CALL ERRMSG('INOBST',6653)
               WRITE(LP,*) 'THE NUMBER OF POROUS AREA MAY ',
     $                     'NOT BE OVER NPRSSZ'
               WRITE(LP,*) 'VARIABLE=D-POROUS'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IPORS(1,NPORS) = ITMP(1)
            IPORS(2,NPORS) = ITMP(2)
            IPORS(3,NPORS) = ITMP(3)
            IPORS(4,NPORS) = ITMP(4)
            IPORS(5,NPORS) = ITMP(5)
            IPORS(6,NPORS) = ITMP(6)
            IPORS(7,NPORS) = 2
            RPORS(1,NPORS) = RTMP(7)
            RPORS(2,NPORS) = RTMP(8)
            RPORS(3,NPORS) = RTMP(9)
            RPORS(4,NPORS) = RTMP(10)
            RPORS(5,NPORS) = RTMP(11)
            RPORS(6,NPORS) = RTMP(12)
            RPORS(7,NPORS) = RTMP(13)
            RPORS(8,NPORS) = RTMP(14)
            RPORS(9,NPORS) = RTMP(15)
            RPORS(10,NPORS)= RTMP(16)
            RPORS(11,NPORS)= RTMP(17)
            RPORS(12,NPORS)= RTMP(18)
            RPORS(13,NPORS)= RTMP(19)
            RPORS(14,NPORS)= RTMP(20)
            RPORS(15,NPORS)= RTMP(21)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FRIC' ) THEN
            CALL MGETR(RTMP,NDAT1,30)
            IF( NDAT1 .NE. 7 ) THEN
               CALL ERRMSG('INOBST',6654)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 7'
               WRITE(LP,*) 'VARIABLE=FRIC'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            ITMP(1)=NINT(RTMP(1))
            ITMP(2)=NINT(RTMP(2))
            ITMP(3)=NINT(RTMP(3))
            ITMP(4)=NINT(RTMP(4))
            ITMP(5)=NINT(RTMP(5))
            ITMP(6)=NINT(RTMP(6))
C
            CALL MODIJ(ITMP(1),ITMP(2),ITMP(3),ITMP(4),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NFRIC = NFRIC + 1
            IF( NFRIC.GT.NFRCSZ ) THEN
               CALL ERRMSG('INOBST',6655)
               WRITE(LP,*) 'THE NUMBER OF FRIC AREA MAY ',
     $                     'NOT BE OVER NFRCSZ'
               WRITE(LP,*) 'VARIABLE=POROUS'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IFRIC(1,NFRIC) = ITMP(1)
            IFRIC(2,NFRIC) = ITMP(2)
            IFRIC(3,NFRIC) = ITMP(3)
            IFRIC(4,NFRIC) = ITMP(4)
            IFRIC(5,NFRIC) = ITMP(5)
            IFRIC(6,NFRIC) = ITMP(6)
            RFRIC(NFRIC)   = RTMP(7)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SEA-FLAG' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 2 ) THEN
               CALL ERRMSG('INOBST',6656)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2'
               WRITE(LP,*) 'VARIABLE=SEA-FLAG'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IDUM=ITMP(1)
            JDUM=ITMP(2)
            CALL MODIJ(ITMP(1),IDUM,ITMP(2),JDUM,0,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NSEA = NSEA + 1
            IF( NSEA.GT.NSEASZ ) THEN
               CALL ERRMSG('INOBST',6657)
               WRITE(LP,*) 'THE NUMBER OF SEA-FLAG MAY ',
     $                     'NOT BE OVER NSEASZ'
               WRITE(LP,*) 'VARIABLE=SEA-FLAG'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            ISEA(1,NSEA) = ITMP(1)
            ISEA(2,NSEA) = ITMP(2)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARENT' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'YES' ) THEN
               LSEA = 1
            ELSE IF( CTMP.EQ.'NO ' ) THEN
               LSEA = 0
            ELSE
               CALL ERRMSG('INOBST',6658)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=PARENT'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FLOAT-POROUS' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'ON ' ) THEN
               LFOBS = 1
            ELSE IF( CTMP.EQ.'OFF' ) THEN
               LFOBS = 0
            ELSE
               CALL ERRMSG('INOBST',6659)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=FLOAT-POROUS'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FLOAT-OBST' ) THEN
            NFOBS = NFOBS + 1
            IF( NFOBS.GT.NPRSSZ ) THEN
               CALL ERRMSG('INOBST',6660)
               WRITE(LP,*) 'THE NUMBER OF FLOAT-OBST MAY ',
     $                     'NOT BE OVER NPRSSZ'
               WRITE(LP,*) 'VARIABLE=FLOAT-OBST'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            CALL MGETR(RTMP,NDAT1,30)
            IF( NDAT1 .NE. 7 ) THEN
               CALL ERRMSG('INOBST',6661)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 7'
               WRITE(LP,*) 'VARIABLE=FLOAT-OBST'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            FPORS(1,NFOBS) = RTMP(1)
            FPORS(2,NFOBS) = RTMP(2)
            FPORS(3,NFOBS) = RTMP(3)
            FPORS(4,NFOBS) = RTMP(4)
            FPORS(5,NFOBS) = RTMP(5)
            FPORS(6,NFOBS) = RTMP(6)
            FPORS(7,NFOBS) = RTMP(7)
C
         ELSE
            CALL ERRMSG('INOBST',6662)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
      IF(LSEA.EQ.1) NSEA=-1
      IF(LFOBS.NE.0.AND.NFOBS.EQ.0) THEN
         CALL ERRMSG('INOBST',6663)
         WRITE(LP,*) 'FLOAT-OBST DATA IS NOTHING'
         CALL ABORT1('')
      END IF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INOBST',6664)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
