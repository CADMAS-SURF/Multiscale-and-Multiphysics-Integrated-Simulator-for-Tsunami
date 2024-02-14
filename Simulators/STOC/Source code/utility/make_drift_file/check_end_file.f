      SUBROUTINE CHECK_END_FILE(N_INUND,D_INUND,MAXD)
!//////////////////////////////////////////////////
!     endファイルに出力されている浸水深到達時刻の数をカウントする
!//////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'CONTROL.h'
!
      INTEGER,INTENT(OUT):: N_INUND
      REAL(8),INTENT(OUT):: D_INUND(MAXD)
      INTEGER,INTENT(IN):: MAXD
!
!
      ENDFILE=TRIM(CFLNM)//'.end'
      N_INUND=1
      D_INUND(:)=0.D0
      D_INUND(1)=1.D-2 ! 第一波到達の判定値
!
      WRITE(LP,*) ''
      WRITE(LP,*) 'READING END FILE ...'
      WRITE(LP,'(3X,A8,I3,A2,F5.3)') 'D_INUND(',1,')=',D_INUND(N_INUND)
!
      OPEN(INP,FILE=TRIM(ENDFILE),STATUS='OLD',FORM='FORMATTED',
     $     ERR=99)
!
      DO
         READ(INP,'(A132)',END=100) CLINE
         IF( CLINE(1:21).EQ.' # INUNDATION TIME D=' ) THEN
            N_INUND=N_INUND+1
            READ(CLINE(22:),*) D_INUND(N_INUND)
            WRITE(LP,'(3X,A8,I3,A2,F5.3)')
     $         'D_INUND(',N_INUND,')=',D_INUND(N_INUND)
         ENDIF
      ENDDO
!
  100 CONTINUE
      CLOSE(INP)
!
      RETURN
!
   99 CONTINUE
      CALL ERRMSG('CHECK_END_FILE',41)
      WRITE(*,*) 'CANNOT OPEN ',TRIM(ENDFILE)
      CALL ABORT1('')
      END
