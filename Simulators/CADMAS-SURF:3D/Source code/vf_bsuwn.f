      SUBROUTINE VF_BSUWN(XX,YY,ZZ,UU,VV,WW,GGX,GGY,GGZ,DBUF,SRCUV,
     &                    NF,INDS)

CD=== 概要 ===========================================================

CDT   VF_BSUWN:表面セルと気体セルの間の法線方向流速を設定(連続の式)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)      : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)      : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)      : I/O : R*8 : z方向流速
CD    GGX(@FOR-3D@)     : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : IN  : R*8 : z方向面積透過率
CD    SRCUV(NUMIJ,NUMK) : IN  : R*8 : 造波ソースのための流速
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDS(@FOR-1D@)    : IN  : I*4 : 表面セルのI,J,K座標
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION SRCUV(NUMIJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDS(NUMI*NUMJ*NUMK)

C==== 実行 ===========================================================

CD    -- 法線方向流速を設定する(表面セルのみのループ) --
      DO 100 L=1,NUMS
        IJK=INDS(L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        NF0=NF(I,J,K)
        I1 =I+1
        J1 =J+1
        K1 =K+1
        FXP=GGX(I1,J,K)*UU(I1,J,K)
        FXM=GGX(I ,J,K)*UU(I ,J,K)
        FYP=GGY(I,J1,K)*VV(I,J1,K)
        FYM=GGY(I,J ,K)*VV(I,J ,K)
        FZP=GGZ(I,J,K1)*WW(I,J,K1)
        FZM=GGZ(I,J,K )*WW(I,J,K )
        IF     (NF0.EQ.5) THEN
          WW(I,J,K1)=(FZM-ZZ(2,K)*( XX(4,I)*(FXP-FXM)
     &                             +YY(4,J)*(FYP-FYM)))/GGZ(I,J,K1)
        ELSEIF (NF0.EQ.6) THEN
          WW(I,J,K )=(FZP+ZZ(2,K)*( XX(4,I)*(FXP-FXM)
     &                             +YY(4,J)*(FYP-FYM)))/GGZ(I,J,K )
        ELSEIF (NF0.EQ.3) THEN
          VV(I,J1,K)=(FYM-YY(2,J)*( XX(4,I)*(FXP-FXM)
     &                             +ZZ(4,K)*(FZP-FZM)))/GGY(I,J1,K)
        ELSEIF (NF0.EQ.4) THEN
          VV(I,J ,K)=(FYP+YY(2,J)*( XX(4,I)*(FXP-FXM)
     &                             +ZZ(4,K)*(FZP-FZM)))/GGY(I,J ,K)
        ELSEIF (NF0.EQ.1) THEN
          UU(I1,J,K)=(FXM-XX(2,I)*( YY(4,J)*(FYP-FYM)
     &                             +ZZ(4,K)*(FZP-FZM)))/GGX(I1,J,K)
        ELSEIF (NF0.EQ.2) THEN
          UU(I ,J,K)=(FXP+XX(2,I)*( YY(4,J)*(FYP-FYM)
     &                             +ZZ(4,K)*(FZP-FZM)))/GGX(I ,J,K)
        ENDIF
 100  CONTINUE

CD    -- 造波ソースのための定数 --
      IWS=0
      JWS=0
      IF     (ISCTYP(1).GT.0) THEN
        IWS= ISCTYP(1)-(MYGIS-1)
      ELSEIF (ISCTYP(1).LT.0) THEN
        JWS=-ISCTYP(1)-(MYGJS-1)
      ENDIF

CD    -- 造波ソース --
      IF (IWS.NE.0 .OR. JWS.NE.0) THEN
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (IWS.EQ.I .OR. JWS.EQ.J) THEN
              DO 200 K=2,NUMK-1
                IF     (NF(I,J,K).EQ.-1) THEN
                ELSEIF (NF(I,J,K).EQ. 0) THEN
                ELSEIF (NF(I,J,K).NE. 8) THEN
                  IF (IWS.EQ.I) Q=2.0D0*XX(4,I)*SRCUV(J,K)
                  IF (JWS.EQ.J) Q=2.0D0*YY(4,J)*SRCUV(I,K)
                  IF     (NF(I,J,K).EQ.1) THEN
                    UU(I+1,J,K)=UU(I+1,J,K)+XX(2,I)*Q/GGX(I+1,J,K)
                  ELSEIF (NF(I,J,K).EQ.2) THEN
                    UU(I  ,J,K)=UU(I  ,J,K)-XX(2,I)*Q/GGX(I  ,J,K)
                  ELSEIF (NF(I,J,K).EQ.3) THEN
                    VV(I,J+1,K)=VV(I,J+1,K)+YY(2,J)*Q/GGY(I,J+1,K)
                  ELSEIF (NF(I,J,K).EQ.4) THEN
                    VV(I,J  ,K)=VV(I,J  ,K)-YY(2,J)*Q/GGY(I,J  ,K)
                  ELSEIF (NF(I,J,K).EQ.5) THEN
                    WW(I,J,K+1)=WW(I,J,K+1)+ZZ(2,K)*Q/GGZ(I,J,K+1)
                  ELSEIF (NF(I,J,K).EQ.6) THEN
                    WW(I,J,K  )=WW(I,J,K  )-ZZ(2,K)*Q/GGZ(I,J,K  )
                  ENDIF
                  GOTO 210
                ENDIF
 200          CONTINUE
 210          CONTINUE
            ENDIF
 300      CONTINUE
 310    CONTINUE
      ENDIF

      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
