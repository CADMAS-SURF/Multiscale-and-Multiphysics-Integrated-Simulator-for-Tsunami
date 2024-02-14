      SUBROUTINE INIT_RANDOM
!//////////////////////////////////////////////////
!     乱数の初期化を行う
!//////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FILE.h'
      INCLUDE 'CONTROL.h'
!
      INTEGER:: SEEDSIZE
      INTEGER,PARAMETER:: MAXSEED=32
      INTEGER::I_RAND_SEED(MAXSEED)=0
      INTEGER:: N
      REAL(8):: XRAND
!
!
      CALL RANDOM_SEED(SIZE=SEEDSIZE)
      IF( SEEDSIZE>MAXSEED ) THEN
         CALL ERRMSG('INIT_RANDOM',51)
         WRITE(LP,*) 'SEEDSIZE > MAXSEED'
         WRITE(LP,*) 'PLEASE MODIFY MAXSEED >=',SEEDSIZE
         CALL ABORT1('')
      ENDIF
!
      I_RAND_SEED(1:SEEDSIZE)=SEED  ! 種のセット
      CALL RANDOM_SEED(PUT=I_RAND_SEED)
!
!     乱数の種を巨大な数値に置き換える(0〜整数値の上限までの間のランダムな値)
!     (入力で種を0,1,2,...と小さくしか変化させない場合にケース毎に相関が出うることへの対策)
      DO N=1,SEEDSIZE
         CALL RANDOM_NUMBER(XRAND) ! 乱数[0,1]
         I_RAND_SEED(N)=INT(XRAND*DBLE(HUGE(1)))
!                                      HUGE(1)は整数の最大値
      ENDDO
      CALL RANDOM_SEED(PUT=I_RAND_SEED)
!
      RETURN
      END
