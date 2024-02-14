C     BCML2NSC用
C     NXY = MAX( MX,MY )
C     NESML ... 親メッシュからみた子側のオーバーラップ幅
C     NESNS ... 子側でみた自分の外側のオーバーラップ幅
C     NESTFL ... =0:親がいない、=1:親がいる
C
      REAL(8) :: TIMVB,TIMVF,TIMHB,TIMHF
      INTEGER :: NESTFL,NXY,NPNTML,NESNS(4),NESML(4)
      COMMON /CP_NESTBC_R/ TIMVB,TIMVF,TIMHB,TIMHF
      COMMON /CP_NESTBC_I/ NESTFL,NXY,NPNTML,NESNS,NESML
