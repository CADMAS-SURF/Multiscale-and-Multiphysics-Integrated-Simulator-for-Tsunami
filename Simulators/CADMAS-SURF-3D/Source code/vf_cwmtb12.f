      SUBROUTINE VF_CWMTB12(WVT,W1,W2,WVZ,DMTBTT,DMTBHH)

CD=== 概要 ===========================================================

CDT   VF_CWMTB12:マトリクスデータ-2の水位の計算

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    WVT            : IN  : R*8 : 無次元位相
CD    W1             : OUT : R*8 : 補間の重み(f=W1*f(i)+W2*f(i+1))
CD    W2             : OUT : R*8 : 補間の重み(f=W1*f(i)+W2*f(i+1))
CD    WVZ            : OUT : R*8 : 水位変動
CD    DMTBTT(MTBTT2) : IN : R*8 : マトリクスデータの無次元位相
CD    DMTBHH(MTBTT2) : IN : R*8 : マトリクスデータの水位
      DIMENSION DMTBTT(MTBTT2),DMTBHH(MTBTT2)

C==== 実行 ===========================================================

CD    -- 位置の検索(全サーチ) --
      IF (MTBNOW2.LE.0) MTBNOW2=MTBTT2-1
      IF     (WVT.LE.DMTBTT(1)     ) THEN
        MTBNOW2=1
        W1=1.0D0
        W2=0.0D0
      ELSEIF (WVT.GE.DMTBTT(MTBTT2)) THEN
        MTBNOW2=MTBTT2-1
        W1=0.0D0
        W2=1.0D0
      ELSE
        IP=0
        DO 100 I=MTBNOW2,1,-1
          IF (DMTBTT(I).LE.WVT .AND. WVT.LE.DMTBTT(I+1)) THEN
            IP=I
            W2=1.0D0/(DMTBTT(I+1)-DMTBTT(I))
            W1=(DMTBTT(I+1)-WVT)*W2
            W2=(WVT-DMTBTT(I  ))*W2
            GOTO 110
          ENDIF
 100    CONTINUE
 110    CONTINUE
        IF (IP.EQ.0) THEN
          DO 200 I=MTBTT2-1,MTBNOW2+1,-1
            IF (DMTBTT(I).LE.WVT .AND. WVT.LE.DMTBTT(I+1)) THEN
              IP=I
              W2=1.0D0/(DMTBTT(I+1)-DMTBTT(I))
              W1=(DMTBTT(I+1)-WVT)*W2
              W2=(WVT-DMTBTT(I  ))*W2
              GOTO 210
            ENDIF
 200      CONTINUE
 210      CONTINUE
        ENDIF
        IF (IP.EQ.0) CALL VF_A2ERR('VF_CWMTB12','P.G ERROR.')
        MTBNOW2=IP
      ENDIF

CD    -- 水位の計算 --
      IF (MTBTYP2.NE.2) THEN
        WVZ=W1*DMTBHH(MTBNOW2)+W2*DMTBHH(MTBNOW2+1)
      ELSE
        WVZ=0.0D0
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
