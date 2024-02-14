      SUBROUTINE INTIME
C======================================================================
C     時間積分の制御情報を読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
C
      CHARACTER(8)::CTMP
C
      REAL(8)::R1,TT
      INTEGER::IE,IERR,IS,N,II
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:intime:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'START' ) THEN
            CALL GETR(R1)
            IF( LSTART.EQ.0 ) THEN
               RSTART = R1
               TIME =  RSTART
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'END' ) THEN
            CALL GETR(REND)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAXSTEP' ) THEN
            CALL GETI(MAXSTP)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPE' ) THEN
            CALL GETC(CTMP,8)
            IF( CTMP.EQ.'CONSTANT' ) THEN
               IDT = 0
            ELSE IF( CTMP.EQ.'AUTO    ' ) THEN
               IDT = 1
            ELSE IF( CTMP.EQ.'SEMI    ' ) THEN
               IDT = -1
            ELSE
               CALL ERRMSG('INTIME',6720)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR AUTO'
               WRITE(LP,*) 'VARIABLE=FILE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DT' ) THEN
            CALL GETR(DTCNST)
            DT = DTCNST
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DTSAFE' ) THEN
            CALL GETR(DTSAFE)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DTMIN' ) THEN
            CALL GETR(DTMIN)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DTMAX' ) THEN
            CALL GETR(DTMAX)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAX-ITERATION' ) THEN
            CALL GETI(MXITER)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RESTART-TIME' ) THEN
            CALL GETR(RSTART)
            LSTART = 2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RESTART-STEP' ) THEN
            CALL GETI(ISTEP)
            LSTART = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RESTART-AUTO' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'ON ' ) THEN
               call flnam('.ars')
               write(lp,*) trim(CFLNM)
               OPEN(IFLAR,FILE=trim(CFLNM),STATUS='OLD',
     $              FORM='FORMATTED',ERR=110)
               READ(IFLAR,*,ERR=110) II,TT
CCC               IF( II.GT.0 ) THEN
                  ISTEP=II
                  LSTART=1
CCC               ENDIF
               CLOSE(IFLAR)
  110          CONTINUE
               IF( ISTEP.EQ.-999 ) THEN
                  CALL ERRMSG('INTIME',6721)
                  WRITE(LP,*) 'CALCULATION IS ALREADY FINISHED.',II,TT
                  CALL ABORT1('')
               ENDIF
            ELSE IF( CTMP.EQ.'OFF' ) THEN
C              NOTHING TO DO
            ELSE
               CALL ERRMSG('INTIME',6722)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=RESTART-AUTO'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE
            CALL ERRMSG('INTIME',6723)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
C ... DTOLD,DTVの初期値を設定
      IF(IDT.NE.0.AND.DTCNST.EQ.0.0D0) DT=DTMIN
      DTOLD = DT
      DTV   = DT
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INTIME',6724)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
