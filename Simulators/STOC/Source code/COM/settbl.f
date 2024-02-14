      SUBROUTINE SETTBL
C======================================================================
C     時系列テーブル値を更新する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'TABLER.h'
      INCLUDE 'TABLEI.h'
      INCLUDE 'TIMER.h'
C
      INTEGER,SAVE::IRSFLG=0
C
      REAL(8)::TIMEBK,TIMED
      INTEGER::M,N,NDAT1
C
      if(IRSFLG.eq.0)then
        TIMEBK = TIME
        TIME = TIME+0.5D0*DT
      endif
C
      TIMED = TIME
C
      DO 100 N=1,NTABLE
         NDAT1 = ITABLE(N)
C ...... テーブルを周期的に使用
         TIMED = MOD(TIME,TTABLE(NDAT1,N))
C
C ...... 未定義域(下側) テーブルの最初の値で固定
         IF( TIMED.LE.TTABLE(1,N) ) THEN
            TABLE(N) = VTABLE(1,N)
C
C ...... 未定義域(上側) テーブルの最後の値で固定
         ELSE IF( TIMED.GE.TTABLE(NDAT1,N) ) THEN
            TABLE(N) = VTABLE(NDAT1,N)
C
C ...... テーブル値から現在時刻に線形補間
         ELSE
            DO 110 M=2,NDAT1
               IF( TIMED.LE.TTABLE(M,N) ) THEN
                  TABLE(N) = ( ( TTABLE(M,N) - TIMED  ) * VTABLE(M-1,N)
     $                     +   ( TIMED - TTABLE(M-1,N)) * VTABLE(M,N) )
     $                     / ( TTABLE(M,N) - TTABLE(M-1,N) )
                  GO TO 120
               END IF
  110       CONTINUE
  120       CONTINUE
         END IF
  100 CONTINUE
C
      if(IRSFLG.eq.0)then
        TIME=TIMEBK
        IRSFLG=1
      endif
      RETURN
      END
