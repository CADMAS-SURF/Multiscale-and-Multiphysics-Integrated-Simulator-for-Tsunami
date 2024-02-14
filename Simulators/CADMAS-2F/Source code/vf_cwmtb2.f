      SUBROUTINE VF_CWMTB2(KM,WT1,WT2,ZC,UN,UT,DMTBZZ,DMTBUN,DMTBUT)

CD=== 概要 ===========================================================

CDT   VF_CWMTB2:マトリクスデータの流速の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    KM                  : I/O : I*4 : k方向検索開始位置
CD    WT1                 : IN  : R*8 : 位相方向補間の重み
CD    WT2                 : IN  : R*8 : 位相方向補間の重み
CD    ZC                  : IN  : R*8 : 鉛直座標(静面を基準)
CD    UN                  : OUT : R*8 : 法線方向流速
CD    UT                  : OUT : R*8 : 接線方向流速
CD    DMTBZZ(MTBZZ)       : IN : R*8 : マトリクスデータのz座標
CD    DMTBUN(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT(MTBZZ,MTBTT) : IN : R*8 : マトリクスデータの鉛直方向流速
      DIMENSION DMTBZZ(MTBZZ),DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT)

C==== 実行 ===========================================================

CD    -- 位置の検索(部分サーチ) --
      IF     (ZC.LE.DMTBZZ(1)    ) THEN
        KM=1
        W1=1.0D0
        W2=0.0D0
      ELSEIF (ZC.GE.DMTBZZ(MTBZZ)) THEN
        KM=MTBZZ-1
        W1=0.0D0
        W2=1.0D0
      ELSE
        KP=0
        DO 100 K=KM,MTBZZ-1
          IF (DMTBZZ(K).LE.ZC .AND. ZC.LE.DMTBZZ(K+1)) THEN
            KP=K
            W2=1.0D0/(DMTBZZ(K+1)-DMTBZZ(K))
            W1=(DMTBZZ(K+1)-ZC)*W2
            W2=(ZC-DMTBZZ(K  ))*W2
            GOTO 110
          ENDIF
 100    CONTINUE
 110    CONTINUE
        IF (KP.EQ.0) CALL VF_A2ERR('VF_CWMTB2','P.G ERROR.')
        KM=KP
      ENDIF

CD    -- 流速の計算 --
      KM1=KM+1
      IM =MTBNOW
      IM1=MTBNOW+1
      P1=W1*DMTBUN(KM,IM )+W2*DMTBUN(KM1,IM )
      P2=W1*DMTBUN(KM,IM1)+W2*DMTBUN(KM1,IM1)
      UN=WT1*P1+WT2*P2
      P1=W1*DMTBUT(KM,IM )+W2*DMTBUT(KM1,IM )
      P2=W1*DMTBUT(KM,IM1)+W2*DMTBUT(KM1,IM1)
      UT=WT1*P1+WT2*P2

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
