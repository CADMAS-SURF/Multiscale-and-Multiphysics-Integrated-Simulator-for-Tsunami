      SUBROUTINE INMTRX
C======================================================================
C     行列ソルバーのパラメータを読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'MATRIX.h'
C
      CHARACTER(3)::CTMP
C
      INTEGER::IE,IERR,IS,N
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:inmtrx:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'EPS' ) THEN
            CALL GETR(EPSMTX)
         ELSE IF( CLINE(IS:IE) .EQ. 'EPS-R' ) THEN
            CALL GETR(EPRMTX)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAX-ITERATION' ) THEN
            CALL GETI(MAXMTX)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PRINT' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'YES' ) THEN
               LPRMTX = 1
            ELSE IF( CTMP.EQ.'NO ' ) THEN
               LPRMTX = 0
            ELSE
               CALL ERRMSG('INMTRX',6630)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=FILE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE
            CALL ERRMSG('INMTRX',6631)
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
      CALL ERRMSG('INMTRX',6632)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
