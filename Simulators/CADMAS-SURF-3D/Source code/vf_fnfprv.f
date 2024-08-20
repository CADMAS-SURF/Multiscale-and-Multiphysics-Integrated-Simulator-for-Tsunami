      SUBROUTINE VF_FNFPRV(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)

CD=== 概要 ===========================================================

CDT   VF_FNFPRV:表面セルの向きを暫定NF(Provisional-NF)により決定する
CD      (1)表面セルは暫定的に7と入力される

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
      DIMENSION FF  (NUMI,NUMJ,NUMK),BCF (NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDS(NUMI*NUMJ*NUMK),IBUF(NUMBUF*MAXBUF)

C==== 実行 ===========================================================

CD    -- 表面セルのみのループにより、向きを決定する --
      DO 100 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        FM=-1.0D0

CD      -- z負方向に流体 --
        KB=K-1
        IF (NF(I,J,KB).EQ.0 .AND. NF(I,J,K+1).EQ.8) THEN
          FS=2.0D0*FF(I,J,KB)
          IF (INDX(I  ,J,KB).GE.1) THEN
            FS=FS+BCF(INDX(I  ,J,KB))
          ELSE
            FS=FS+FF(I-1,J,KB)
          ENDIF
          IF (INDX(I+1,J,KB).GE.1) THEN
            FS=FS+BCF(INDX(I+1,J,KB))
          ELSE
            FS=FS+FF(I+1,J,KB)
          ENDIF
          IF (INDY(I,J  ,KB).GE.1) THEN
            FS=FS+BCF(INDY(I,J  ,KB))
          ELSE
            FS=FS+FF(I,J-1,KB)
          ENDIF
          IF (INDY(I,J+1,KB).GE.1) THEN
            FS=FS+BCF(INDY(I,J+1,KB))
          ELSE
            FS=FS+FF(I,J+1,KB)
          ENDIF
          IF (FS.GT.FM) THEN
            NF(I,J,K)=5
            FM=FS
          ENDIF
        ENDIF

CD      -- z正方向に流体 --
        KB=K+1
        IF (NF(I,J,KB).EQ.0 .AND. NF(I,J,K-1).EQ.8) THEN
          FS=2.0D0*FF(I,J,KB)
          IF (INDX(I  ,J,KB).GE.1) THEN
            FS=FS+BCF(INDX(I  ,J,KB))
          ELSE
            FS=FS+FF(I-1,J,KB)
          ENDIF
          IF (INDX(I+1,J,KB).GE.1) THEN
            FS=FS+BCF(INDX(I+1,J,KB))
          ELSE
            FS=FS+FF(I+1,J,KB)
          ENDIF
          IF (INDY(I,J  ,KB).GE.1) THEN
            FS=FS+BCF(INDY(I,J  ,KB))
          ELSE
            FS=FS+FF(I,J-1,KB)
          ENDIF
          IF (INDY(I,J+1,KB).GE.1) THEN
            FS=FS+BCF(INDY(I,J+1,KB))
          ELSE
            FS=FS+FF(I,J+1,KB)
          ENDIF
          IF (FS.GT.FM) THEN
            NF(I,J,K)=6
            FM=FS
          ENDIF
        ENDIF

CD      -- y負方向に流体 --
        JB=J-1
        IF (NF(I,JB,K).EQ.0 .AND. NF(I,J+1,K).EQ.8) THEN
          FS=2.0D0*FF(I,JB,K)
          IF (INDX(I  ,JB,K).GE.1) THEN
            FS=FS+BCF(INDX(I  ,JB,K))
          ELSE
            FS=FS+FF(I-1,JB,K)
          ENDIF
          IF (INDX(I+1,JB,K).GE.1) THEN
            FS=FS+BCF(INDX(I+1,JB,K))
          ELSE
            FS=FS+FF(I+1,JB,K)
          ENDIF
          IF (INDZ(I,JB,K  ).GE.1) THEN
            FS=FS+BCF(INDZ(I,JB,K  ))
          ELSE
            FS=FS+FF(I,JB,K-1)
          ENDIF
          IF (INDZ(I,JB,K+1).GE.1) THEN
            FS=FS+BCF(INDZ(I,JB,K+1))
          ELSE
            FS=FS+FF(I,JB,K+1)
          ENDIF
          IF (FS.GT.FM) THEN
            NF(I,J,K)=3
            FM=FS
          ENDIF
        ENDIF

CD      -- y正方向に流体 --
        JB=J+1
        IF (NF(I,JB,K).EQ.0 .AND. NF(I,J-1,K).EQ.8) THEN
          FS=2.0D0*FF(I,JB,K)
          IF (INDX(I  ,JB,K).GE.1) THEN
            FS=FS+BCF(INDX(I  ,JB,K))
          ELSE
            FS=FS+FF(I-1,JB,K)
          ENDIF
          IF (INDX(I+1,JB,K).GE.1) THEN
            FS=FS+BCF(INDX(I+1,JB,K))
          ELSE
            FS=FS+FF(I+1,JB,K)
          ENDIF
          IF (INDZ(I,JB,K  ).GE.1) THEN
            FS=FS+BCF(INDZ(I,JB,K  ))
          ELSE
            FS=FS+FF(I,JB,K-1)
          ENDIF
          IF (INDZ(I,JB,K+1).GE.1) THEN
            FS=FS+BCF(INDZ(I,JB,K+1))
          ELSE
            FS=FS+FF(I,JB,K+1)
          ENDIF
          IF (FS.GT.FM) THEN
            NF(I,J,K)=4
            FM=FS
          ENDIF
        ENDIF

CD      -- x負方向に流体 --
        IB=I-1
        IF (NF(IB,J,K).EQ.0 .AND. NF(I+1,J,K).EQ.8) THEN
          FS=2.0D0*FF(IB,J,K)
          IF (INDY(IB,J  ,K).GE.1) THEN
            FS=FS+BCF(INDY(IB,J  ,K))
          ELSE
            FS=FS+FF(IB,J-1,K)
          ENDIF
          IF (INDY(IB,J+1,K).GE.1) THEN
            FS=FS+BCF(INDY(IB,J+1,K))
          ELSE
            FS=FS+FF(IB,J+1,K)
          ENDIF
          IF (INDZ(IB,J,K  ).GE.1) THEN
            FS=FS+BCF(INDZ(IB,J,K  ))
          ELSE
            FS=FS+FF(IB,J,K-1)
          ENDIF
          IF (INDZ(IB,J,K+1).GE.1) THEN
            FS=FS+BCF(INDZ(IB,J,K+1))
          ELSE
            FS=FS+FF(IB,J,K+1)
          ENDIF
          IF (FS.GT.FM) THEN
            NF(I,J,K)=1
            FM=FS
          ENDIF
        ENDIF

CD      -- x正方向に流体 --
        IB=I+1
        IF (NF(IB,J,K).EQ.0 .AND. NF(I-1,J,K).EQ.8) THEN
          FS=2.0D0*FF(IB,J,K)
          IF (INDY(IB,J  ,K).GE.1) THEN
            FS=FS+BCF(INDY(IB,J  ,K))
          ELSE
            FS=FS+FF(IB,J-1,K)
          ENDIF
          IF (INDY(IB,J+1,K).GE.1) THEN
            FS=FS+BCF(INDY(IB,J+1,K))
          ELSE
            FS=FS+FF(IB,J+1,K)
          ENDIF
          IF (INDZ(IB,J,K  ).GE.1) THEN
            FS=FS+BCF(INDZ(IB,J,K  ))
          ELSE
            FS=FS+FF(IB,J,K-1)
          ENDIF
          IF (INDZ(IB,J,K+1).GE.1) THEN
            FS=FS+BCF(INDZ(IB,J,K+1))
          ELSE
            FS=FS+FF(IB,J,K+1)
          ENDIF
          IF (FS.GT.FM) THEN
            NF(I,J,K)=2
            FM=FS
          ENDIF
        ENDIF

CD      -- 方向が決定しなければエラー --
        IF (NF(I,J,K).EQ.7) CALL VF_A2ERR('VF_FNFPRV','P.G ERROR.')

 100  CONTINUE

      CALL VF_P3SRI2(NF,IBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
