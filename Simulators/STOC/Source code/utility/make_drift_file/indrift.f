      SUBROUTINE INDRIFT
C======================================================================
C     DRIFTブロックを読み込む
C======================================================================
      IMPLICIT NONE
C
      INCLUDe 'CONTROL.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      INTEGER::IE,IERR,IS,N
      INTEGER::IR,IQ,IH
C
C
      IR=0
      IQ=0
      IH=0
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIV-X' ) THEN
            CALL GETI(DIVX)
            IF( DIVX.LT.1 ) THEN
               CALL ERRMSG('INDRIFT',31)
               WRITE(LP,*)' VALUE MUST BE >=1'
               WRITE(LP,*) 'VARIABLE=DIV-X'
               WRITE(LP,*) 'VALUE=',DIVX
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIV-Y' ) THEN
            CALL GETI(DIVY)
            IF( DIVY.LT.1 ) THEN
               CALL ERRMSG('INDRIFT',32)
               WRITE(LP,*)' VALUE MUST BE >=1'
               WRITE(LP,*) 'VARIABLE=DIV-Y'
               WRITE(LP,*) 'VALUE=',DIVY
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DENSITY' ) THEN
            CALL GETR(RHOD)
            IR=1
            IF( RHOD.LE.0.D0 ) THEN
               CALL ERRMSG('INDRIFT',33)
               WRITE(LP,*)' VALUE MUST BE > 0'
               WRITE(LP,*) 'VARIABLE=DENSITY'
               WRITE(LP,*) 'VALUE=',RHOD
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Q' ) THEN
            CALL GETR(QD)
            IQ=1
            IF( QD.LE.0.D0 ) THEN
               CALL ERRMSG('INDRIFT',34)
               WRITE(LP,*)' VALUE MUST BE > 0'
               WRITE(LP,*) 'VARIABLE=Q'
               WRITE(LP,*) 'VALUE=',QD
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HEIGHT' ) THEN
            CALL GETR(HD)
            IH=1
            IF( HD.LE.0.D0 ) THEN
               CALL ERRMSG('INDRIFT',35)
               WRITE(LP,*)' VALUE MUST BE > 0'
               WRITE(LP,*) 'VARIABLE=HEIGHT'
               WRITE(LP,*) 'VALUE=',HD
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DENSITY-WATER' ) THEN
            CALL GETR(RHOL)
            IF( RHOL.LE.0.D0 ) THEN
               CALL ERRMSG('INDRIFT',38)
               WRITE(LP,*)' VALUE MUST BE > 0'
               WRITE(LP,*) 'VARIABLE=DENSITY=WATER'
               WRITE(LP,*) 'VALUE=',RHOL
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE
            CALL ERRMSG('INDRIFT',36)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
      RETURN
C
      IF( IR+IQ+IH.LE.1 ) THEN
         CALL ERRMSG('INDRIFT',37)
         WRITE(LP,*)' MUST BE SPECIFY AT LEAST TWO OF',
     $              ' DENSITY AND Q AND HEIGHT'
         CALL ABORT1('')
      ELSEIF( IR+IQ+IH.EQ.2 ) THEN
         IF( IR.EQ.0 ) RHOD=QD/HD
         IF( IQ.EQ.0 ) QD=RHOD*HD
         IF( IH.EQ.0 ) HD=QD/RHOD
      ELSE
         IF( QD>RHOD*HD ) THEN
            WRITE(LP,*) 'WARNING: Q>DENSITY*HEIGHT'
         ENDIF
      ENDIF
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INDRIFT',38)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
