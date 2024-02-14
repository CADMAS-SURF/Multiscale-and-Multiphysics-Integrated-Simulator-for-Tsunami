      SUBROUTINE ININIT
C======================================================================
C     初期条件を読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'INITL.h'
C
      CHARACTER(3)::CTMP
      REAL(8)::RTMP(10)
C
      INTEGER::I,IE,IERR,IS,N,NDAT1
c20150318add(s)
      integer::itmp(4),inside
c20150318add(e)
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
cc         write(*,*) 'debug:ininit:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U' ) THEN
            CALL GETR(UUINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V' ) THEN
            CALL GETR(VVINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'W' ) THEN
            CALL GETR(WWINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K' ) THEN
            CALL GETR(AKINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'EP' ) THEN
            CALL GETR(EPINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Q2' ) THEN
            CALL GETR(AKINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'L' ) THEN
            CALL GETR(EPINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'T' ) THEN
            CALL GETR(TTINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'C' ) THEN
            CALL GETR(CCINIT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TC-FILE' ) THEN
            CALL GETC(CTMP,3)
            IF(CTMP.EQ.'ON') THEN
              TTINIT = 1.0D15
              CCINIT = 1.0D15
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'H' ) THEN
            NHINIT = NHINIT + 1
            IF( NHINIT.GT.NINTSZ ) THEN
               CALL ERRMSG('ININIT',6580)
               WRITE(LP,*) 'THE NUMBER OF WATER-LEVEL AREA MAY ',
     $                     'NOT BE OVER INITSZ'
               WRITE(LP,*) 'VARIABLE=H'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            CALL MGETR(RTMP,NDAT1,10)
            IF( NDAT1.NE.5 .AND. NDAT1.NE.1 ) THEN
               CALL ERRMSG('ININIT',6581)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=H'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
cadd20150318(s)
            ITMP(1)=NINT(RTMP(2))
            ITMP(2)=NINT(RTMP(3))
            ITMP(3)=NINT(RTMP(4))
            ITMP(4)=NINT(RTMP(5))
            CALL MODIJ(ITMP(1),ITMP(2),ITMP(3),ITMP(4),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
cadd20150318(e)
            HHINIT(NHINIT) = RTMP(1)
            IF( NDAT1.EQ.5 ) THEN
cmod20150318(s)
               IHINIT(1,NHINIT) = ITMP(1)
               IHINIT(2,NHINIT) = ITMP(2)
               IHINIT(3,NHINIT) = ITMP(3)
               IHINIT(4,NHINIT) = ITMP(4)
cmod20150318(s)
            ELSE
               IHINIT(1,NHINIT) = 0
               IHINIT(2,NHINIT) = 0
               IHINIT(3,NHINIT) = 0
               IHINIT(4,NHINIT) = 0
            END IF
C
         ELSE
            CALL ERRMSG('ININIT',6582)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('ININIT',6583)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
