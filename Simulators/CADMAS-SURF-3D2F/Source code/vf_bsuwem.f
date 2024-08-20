      SUBROUTINE VF_BSUWEM(UU,VV,WW,DBUF,NF,INDS,IBUF,NLIM)

CD=== 概要 ===========================================================

CDT   VF_BSUWEM:特殊な気体セルに流速を設定する
C       砕波時等で角部が気体セルから流体セルに変化する場合に流速ゼロ
C       が閉じ込められないようにする(NFの決定前に行う)

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
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
CD    NLIM(@FOR-3D@)   : OUT : I*4 : 補正を行うためのワーク
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDS(NUMI*NUMJ*NUMK)
      DIMENSION IBUF(NUMBUF*MAXBUF) ,NLIM(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- ゼロクリア --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            NLIM(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 流体・気体セルから見て自分を向いていない表面セルの数を数える --
      DO 200 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        N  =NF(I,J,K)
        IF     (N.EQ.5 .OR. N.EQ.6) THEN
          NLIM(I+1,J,K)=NLIM(I+1,J,K)+1
          NLIM(I-1,J,K)=NLIM(I-1,J,K)+1
          NLIM(I,J+1,K)=NLIM(I,J+1,K)+1
          NLIM(I,J-1,K)=NLIM(I,J-1,K)+1
        ELSEIF (N.EQ.3 .OR. N.EQ.4) THEN
          NLIM(I+1,J,K)=NLIM(I+1,J,K)+1
          NLIM(I-1,J,K)=NLIM(I-1,J,K)+1
          NLIM(I,J,K+1)=NLIM(I,J,K+1)+1
          NLIM(I,J,K-1)=NLIM(I,J,K-1)+1
        ELSE
          NLIM(I,J+1,K)=NLIM(I,J+1,K)+1
          NLIM(I,J-1,K)=NLIM(I,J-1,K)+1
          NLIM(I,J,K+1)=NLIM(I,J,K+1)+1
          NLIM(I,J,K-1)=NLIM(I,J,K-1)+1
        ENDIF
 200  CONTINUE

      CALL VF_P3SRI1(NLIM,IBUF,0)

CD    -- 特殊な気体セルに流速を設定 --

CAKIY   → 3次元版として再考の余地有り

      DO 320 K=2,NUMK-1
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (NLIM(I,J,K).GE.2 .AND. NF(I,J,K).EQ.8) THEN
              IF (NF(I-1,J,K).EQ.8 .AND. NLIM(I-1,J,K).LT.2) THEN
                IF (NF(I+1,J,K).NE.8) UU(I,J,K)=UU(I+1,J,K)
              ENDIF
              IF (NF(I+1,J,K).EQ.8 .AND. NLIM(I+1,J,K).LT.2) THEN
                IF (NF(I-1,J,K).NE.8) UU(I+1,J,K)=UU(I,J,K)
              ENDIF
              IF (NF(I,J-1,K).EQ.8 .AND. NLIM(I,J-1,K).LT.2) THEN
                IF (NF(I,J+1,K).NE.8) VV(I,J,K)=VV(I,J+1,K)
              ENDIF
              IF (NF(I,J+1,K).EQ.8 .AND. NLIM(I,J+1,K).LT.2) THEN
                IF (NF(I,J-1,K).NE.8) VV(I,J+1,K)=VV(I,J,K)
              ENDIF
              IF (NF(I,J,K-1).EQ.8 .AND. NLIM(I,J,K-1).LT.2) THEN
                IF (NF(I,J,K+1).NE.8) WW(I,J,K)=WW(I,J,K+1)
              ENDIF
              IF (NF(I,J,K+1).EQ.8 .AND. NLIM(I,J,K+1).LT.2) THEN
                IF (NF(I,J,K-1).NE.8) WW(I,J,K+1)=WW(I,J,K)
              ENDIF
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
