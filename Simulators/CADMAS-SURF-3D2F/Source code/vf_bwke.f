      SUBROUTINE VF_BWKE(AK,AE,BCK,BCE,BCF,INDB,INDBK,INDBE)

CD=== 概要 ===========================================================

CDT   VF_BWKE:境界面の乱流量を設定する
CD      (1)対数則は境界面に接するセル中心で設定するため、
CD      (2)ここではFREEと同じ処理をする

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    AK(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB,3)      : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)      : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    INDBK(MAXBK1,NUMB) : IN  : I*4 : 乱流エネルギの境界条件
CD    INDBE(MAXBE1,NUMB) : IN  : I*4 : 乱流エネルギ散逸の境界条件
      DIMENSION AK(NUMI,NUMJ,NUMK),AE(NUMI,NUMJ,NUMK)
      DIMENSION BCK(NUMB,3),BCE(NUMB,3),BCF(NUMB)
      DIMENSION INDB(MAXB1,NUMB),INDBK(MAXBK1,NUMB),INDBE(MAXBE1,NUMB)

C==== 実行 ===========================================================

CD    -- 境界面の乱流量を設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        IF (IJK.LE.0) GOTO 100
        NS =INDB(2,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

        CF1=BCF(L)
        CF2=1.0D0-CF1

        DO 99 LGF=2,3
          IBK=ABS(INDBK(LGF-1,L))
          IBE=ABS(INDBE(LGF-1,L))

CD      -- 計算セル側の値をとる --
          IF     (NS.EQ.1) THEN
            AKC=AK(I  ,J,K)
            AEC=AE(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            AKC=AK(I-1,J,K)
            AEC=AE(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            AKC=AK(I,J  ,K)
            AEC=AE(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            AKC=AK(I,J-1,K)
            AEC=AE(I,J-1,K)
          ELSEIF (NS.EQ.5) THEN
            AKC=AK(I,J,K  )
            AEC=AE(I,J,K  )
          ELSEIF (NS.EQ.6) THEN
            AKC=AK(I,J,K-1)
            AEC=AE(I,J,K-1)
          ENDIF

CD      -- 値固定 --
          IF     (IBK.EQ.1) THEN
            BCK(L,LGF)=BCK(L,LGF)

CD      -- 勾配ゼロ --
          ELSEIF (IBK.EQ.2) THEN
            BCK(L,LGF)=AKC

CD      -- 対数則 --
          ELSEIF (IBK.EQ.6) THEN
            BCK(L,LGF)=AKC

CD      -- 完全粗面 --
          ELSEIF (IBK.EQ.8) THEN
            BCK(L,LGF)=AKC

CD      -- プログラムエラー --
          ELSE
            CALL VF_A2ERR('VF_BWKE','P.G ERROR.')
          ENDIF

CD      -- 値固定 --
          IF     (IBE.EQ.1) THEN
            BCE(L,LGF)=BCE(L,LGF)

CD      -- 勾配ゼロ --
          ELSEIF (IBE.EQ.2) THEN
            BCE(L,LGF)=AEC

CD      -- 対数則 --
          ELSEIF (IBE.EQ.6) THEN
            BCE(L,LGF)=AEC

CD      -- 完全粗面 --
          ELSEIF (IBE.EQ.8) THEN
            BCE(L,LGF)=AEC

CD      -- プログラムエラー --
          ELSE
            CALL VF_A2ERR('VF_BWKE','P.G ERROR.')
          ENDIF

 99     CONTINUE

CD      -- ２相混合値の設定 --
        BCK(L,1)=CF1*BCK(L,2)+CF2*BCK(L,3)
        BCE(L,1)=CF1*BCE(L,2)+CF2*BCE(L,3)

 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
