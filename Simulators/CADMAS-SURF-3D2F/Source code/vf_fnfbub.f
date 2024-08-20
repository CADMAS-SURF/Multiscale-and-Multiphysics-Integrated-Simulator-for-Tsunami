      SUBROUTINE VF_FNFBUB(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF,INDBUB)

CD=== 概要 ===========================================================

CDT   VF_FNFBUB:気胞セルの検索とNFの設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    FF(@FOR-3D@)        : IN  : R*8 : VOF関数F
CD    BCF(NUMB)           : IN  : R*8 : VOF関数Fの境界値
CD    NF(@FOR-3D@)        : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)      : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)      : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)      : IN  : I*4 : z面の状態を示すインデックス
CD    INDS(@FOR-1D@)      : OUT : I*4 : 表面セルのI,J,K座標
CD    IBUF(NUMBUF*MAXBUF) : --- : I*4 : 並列用のバッファ
CD    INDBUB(@FOR-1D@)    : --- : I*4 : 気胞セルのI,J,K座標（ワーク）
      DIMENSION FF  (NUMI,NUMJ,NUMK),BCF (NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDS(NUMI*NUMJ*NUMK),IBUF(NUMBUF*MAXBUF)
      DIMENSION INDBUB(NUMI*NUMJ*NUMK)

C==== 実行 ===========================================================

      RETURN

CD    -- 気胞セルの検索 --
      NBUB=0
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.0 .AND. FF(I,J,K).LE.FUPPER) THEN
              IF (FF(I,J,K).LT.FLOWER)THEN
C               ** 完全気胞セルとする **
                NF(I,J,K)=8
              ELSE
C               ** 仮りにNF=50とする **
                NBUB=NBUB+1
                INDBUB(NBUB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
                NF(I,J,K)=50
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 液滴セルの検索 --
      NBUB=NBUB
      DO 150 K=2,NUMK-1
        DO 140 J=MYJS,MYJE
          DO 130 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.8 .AND. FF(I,J,K).GE.FLOWER) THEN
              IF (FF(I,J,K).GT.FUPPER)THEN
C               ** 完全液滴セルとする **
                NF(I,J,K)=0
              ELSE
C               ** 仮りにNF=50とする **
                NBUB=NBUB+1
                INDBUB(NBUB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
                NF(I,J,K)=50
              ENDIF
            ENDIF
 130      CONTINUE
 140    CONTINUE
 150  CONTINUE

      CALL VF_P3SRI1(NF,IBUF,0)

CCC   GOTO 1000

CD    -- 表面セルのみのループにより、向きを決定する --
      DO 200 L=1,NBUB
        IJK=INDBUB(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        FM=-1.0D0

CD      -- z負方向に流体 --
        KB=K-1
        IF (NF(I,J,KB).EQ.0 .AND. NF(I,J,K+1).GT.8) THEN
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
            NF(I,J,K)=50
            FM=FS
          ENDIF
        ENDIF

CD      -- z正方向に流体 --
        KB=K+1
        IF (NF(I,J,KB).EQ.0 .AND. NF(I,J,K-1).GE.8) THEN
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
            NF(I,J,K)=60
            FM=FS
          ENDIF
        ENDIF

CD      -- y負方向に流体 --
        JB=J-1
        IF (NF(I,JB,K).EQ.0 .AND. NF(I,J+1,K).GE.8) THEN
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
            NF(I,J,K)=30
            FM=FS
          ENDIF
        ENDIF

CD      -- y正方向に流体 --
        JB=J+1
        IF (NF(I,JB,K).EQ.0 .AND. NF(I,J-1,K).GE.8) THEN
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
            NF(I,J,K)=40
            FM=FS
          ENDIF
        ENDIF

CD      -- x負方向に流体 --
        IB=I-1
        IF (NF(IB,J,K).EQ.0 .AND. NF(I+1,J,K).GE.8) THEN
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
            NF(I,J,K)=10
            FM=FS
          ENDIF
        ENDIF

CD      -- x正方向に流体 --
        IB=I+1
        IF (NF(IB,J,K).EQ.0 .AND. NF(I-1,J,K).GE.8) THEN
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
            NF(I,J,K)=20
            FM=FS
          ENDIF
        ENDIF
 200  CONTINUE

 1000 CONTINUE
      DO 220 L=1,NBUB
        IJK=INDBUB(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        NF(I,J,K)=NF(I,J,K)/10
 220  CONTINUE

      CALL VF_P3SRI2(NF,IBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
