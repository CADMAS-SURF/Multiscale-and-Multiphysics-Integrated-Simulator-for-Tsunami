      SUBROUTINE VF_BWFF(FF,BCF,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWFF:境界面のVOF関数Fを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION FF(NUMI,NUMJ,NUMK),BCF(NUMB)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 境界面のVOF関数Fを設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        IF (IJK.LE.0) GOTO 100
        NS =INDB(2,L)
        IB =INDB(4,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

CD      -- 値固定 --
        IF     (IB.EQ.1) THEN
          BCF(L)=BCF(L)

CD      -- フリー --
        ELSEIF (IB.EQ.2) THEN
          IF     (NS.EQ.1) THEN
            BCF(L)=FF(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            BCF(L)=FF(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            BCF(L)=FF(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            BCF(L)=FF(I,J-1,K)
          ELSEIF (NS.EQ.5) THEN
            BCF(L)=FF(I,J,K  )
          ELSEIF (NS.EQ.6) THEN
            BCF(L)=FF(I,J,K-1)
          ENDIF

CD      -- 造波境界 --
        ELSEIF (IB.EQ.5) THEN
          IF     (NS.EQ.1) THEN
            BCF(L)=FF(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            BCF(L)=FF(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            BCF(L)=FF(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            BCF(L)=FF(I,J-1,K)
          ELSE
            CALL VF_A2ERR('VF_BWFF','P.G ERROR.')
          ENDIF

CD      -- 放射境界 --
        ELSEIF (IB.EQ.7) THEN
          IF     (NS.EQ.1) THEN
            BCF(L)=FF(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            BCF(L)=FF(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            BCF(L)=FF(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            BCF(L)=FF(I,J-1,K)
          ELSE
            CALL VF_A2ERR('VF_BWFF','P.G ERROR.')
          ENDIF

CD      -- プログラムエラー --
        ELSE
          CALL VF_A2ERR('VF_BWFF','P.G ERROR.')
        ENDIF
 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
