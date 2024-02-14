      SUBROUTINE VF_BSUWT2(UU,VV,WW,DBUF,NF,INDX,INDY,INDZ,INDS)

CD=== 概要 ===========================================================

CDT   VF_BSUWT2:自由表面での接線方向流速を再設定(特殊なパターン)
CD      (1)接線方向に、隣のセルが気体かつその下が構造物の場合、
CD      (2)流体が気体セル側に進行できないことを防ぐ

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : I/O : R*8 : z方向流速
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDS(NUMI*NUMJ*NUMK)

C==== 実行 ===========================================================

CD    -- 接線方向流速を再設定する(表面セルのみのループ) --
      DO 100 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        NF0=NF(I,J,K)
        IF     (NF0.EQ.5) THEN
          LB=K-1
          L1=I
          L2=I
          IF (NF(I-1,J,K).EQ.8 .AND. INDX(I  ,J,LB).NE.0) L2=L2+1
          IF (NF(I+1,J,K).EQ.8 .AND. INDX(I+1,J,LB).NE.0) L1=L1+1
          UU(L1,J,K)=UU(L2,J,K)
          L1=J
          L2=J
          IF (NF(I,J-1,K).EQ.8 .AND. INDY(I,J  ,LB).NE.0) L2=L2+1
          IF (NF(I,J+1,K).EQ.8 .AND. INDY(I,J+1,LB).NE.0) L1=L1+1
          VV(I,L1,K)=VV(I,L2,K)
        ELSEIF (NF0.EQ.6) THEN
          LB=K+1
          L1=I
          L2=I
          IF (NF(I-1,J,K).EQ.8 .AND. INDX(I  ,J,LB).NE.0) L2=L2+1
          IF (NF(I+1,J,K).EQ.8 .AND. INDX(I+1,J,LB).NE.0) L1=L1+1
          UU(L1,J,K)=UU(L2,J,K)
          L1=J
          L2=J
          IF (NF(I,J-1,K).EQ.8 .AND. INDY(I,J  ,LB).NE.0) L2=L2+1
          IF (NF(I,J+1,K).EQ.8 .AND. INDY(I,J+1,LB).NE.0) L1=L1+1
          VV(I,L1,K)=VV(I,L2,K)
        ELSEIF (NF0.EQ.3) THEN
          LB=J-1
          L1=I
          L2=I
          IF (NF(I-1,J,K).EQ.8 .AND. INDX(I  ,LB,K).NE.0) L2=L2+1
          IF (NF(I+1,J,K).EQ.8 .AND. INDX(I+1,LB,K).NE.0) L1=L1+1
          UU(L1,J,K)=UU(L2,J,K)
          L1=K
          L2=K
          IF (NF(I,J,K-1).EQ.8 .AND. INDZ(I,LB,K  ).NE.0) L2=L2+1
          IF (NF(I,J,K+1).EQ.8 .AND. INDZ(I,LB,K+1).NE.0) L1=L1+1
          WW(I,J,L1)=WW(I,J,L2)
        ELSEIF (NF0.EQ.4) THEN
          LB=J+1
          L1=I
          L2=I
          IF (NF(I-1,J,K).EQ.8 .AND. INDX(I  ,LB,K).NE.0) L2=L2+1
          IF (NF(I+1,J,K).EQ.8 .AND. INDX(I+1,LB,K).NE.0) L1=L1+1
          UU(L1,J,K)=UU(L2,J,K)
          L1=K
          L2=K
          IF (NF(I,J,K-1).EQ.8 .AND. INDZ(I,LB,K  ).NE.0) L2=L2+1
          IF (NF(I,J,K+1).EQ.8 .AND. INDZ(I,LB,K+1).NE.0) L1=L1+1
          WW(I,J,L1)=WW(I,J,L2)
        ELSEIF (NF0.EQ.1) THEN
          LB=I-1
          L1=J
          L2=J
          IF (NF(I,J-1,K).EQ.8 .AND. INDY(LB,J  ,K).NE.0) L2=L2+1
          IF (NF(I,J+1,K).EQ.8 .AND. INDY(LB,J+1,K).NE.0) L1=L1+1
          VV(I,L1,K)=VV(I,L2,K)
          L1=K
          L2=K
          IF (NF(I,J,K-1).EQ.8 .AND. INDZ(LB,J,K  ).NE.0) L2=L2+1
          IF (NF(I,J,K+1).EQ.8 .AND. INDZ(LB,J,K+1).NE.0) L1=L1+1
          WW(I,J,L1)=WW(I,J,L2)
        ELSEIF (NF0.EQ.2) THEN
          LB=I+1
          L1=J
          L2=J
          IF (NF(I,J-1,K).EQ.8 .AND. INDY(LB,J  ,K).NE.0) L2=L2+1
          IF (NF(I,J+1,K).EQ.8 .AND. INDY(LB,J+1,K).NE.0) L1=L1+1
          VV(I,L1,K)=VV(I,L2,K)
          L1=K
          L2=K
          IF (NF(I,J,K-1).EQ.8 .AND. INDZ(LB,J,K  ).NE.0) L2=L2+1
          IF (NF(I,J,K+1).EQ.8 .AND. INDZ(LB,J,K+1).NE.0) L1=L1+1
          WW(I,J,L1)=WW(I,J,L2)
        ENDIF
 100  CONTINUE

      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
