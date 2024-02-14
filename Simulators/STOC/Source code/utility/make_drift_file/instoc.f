      SUBROUTINE INSTOC
C======================================================================
C     STOCの解析条件ファイル名を読み込む
C======================================================================
      IMPLICIT NONE
C
      INCLUDe 'CONTROL.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      INTEGER::IE,IERR,IS,N
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FILE' ) THEN
            CALL GETC(AREAFILE,64)
C
         ELSE
            CALL ERRMSG('INSTOC',6550)
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
      CALL ERRMSG('INSTOC',21)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
