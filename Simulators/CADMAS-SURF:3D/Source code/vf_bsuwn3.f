      SUBROUTINE VF_BSUWN3(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDS)

CD=== 概要 ===========================================================

CDT   VF_BSUWN3:表面セルと気体セルの間の法線方向流速を設定(勾配ゼロ等)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : I/O : R*8 : z方向流速
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDS(@FOR-1D@)   : IN  : I*4 : 表面セルのI,J,K座標
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
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
        IS1=0
        D1 =0.0D0
        V1 =0.0D0
        IS2=0
        D2 =0.0D0
        V2 =0.0D0
        IS3=0
        D3 =0.0D0
        V3 =0.0D0
        IS4=0
        D4 =0.0D0
        V4 =0.0D0
        IF     (NF0.EQ.5) THEN
          D0=ZZ(2,K)
          V0=WW(I,J,K)
          L1=K+1
          LS=I-1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,J,L1).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,J,L1).NE.-1) THEN
              IS1=1
              D1 =XX(3,I)
              V1 =WW(LS,J,L1)
            ENDIF
          ENDIF
          LS=I+1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,J,L1).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,J,L1).NE.-1) THEN
              IS2=1
              D2 =XX(3,LS)
              V2 =WW(LS,J,L1)
            ENDIF
          ENDIF
          LS=J-1
          IF (NF(I,LS,K).EQ.0 .OR. NF(I,LS,L1).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(I,LS,L1).NE.-1) THEN
              IS3=1
              D3 =YY(3,J)
              V3 =WW(I,LS,L1)
            ENDIF
          ENDIF
          LS=J+1
          IF (NF(I,LS,K).EQ.0 .OR. NF(I,LS,L1).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(I,LS,L1).NE.-1) THEN
              IS4=1
              D4 =YY(3,LS)
              V4 =WW(I,LS,L1)
            ENDIF
          ENDIF
          ISS=IS1+IS2+IS3+IS4
          IF (ISS.LE.0) THEN
            IF (IBSUW0.EQ.0 .AND. NF(I,J,K-2).EQ.0) THEN
              V0=V0+(V0-WW(I,J,K-1))*ZZ(2,K)/ZZ(2,K-1)
            ENDIF
            WW(I,J,K+1)=V0
          ELSE
            DS=D0+D1+D2+D3+D4
            WW(I,J,K+1)=( (DS-D0)*V0+(DS-D1)*V1+(DS-D2)*V2
     &                   +(DS-D3)*V3+(DS-D4)*V4           )/DS/DBLE(ISS)
          ENDIF
        ELSEIF (NF0.EQ.6) THEN
          D0=ZZ(2,K)
          V0=WW(I,J,K+1)
          L1=K-1
          LS=I-1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,J,L1).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,J,L1).NE.-1) THEN
              IS1=1
              D1 =XX(3,I)
              V1 =WW(LS,J,K)
            ENDIF
          ENDIF
          LS=I+1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,J,L1).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,J,L1).NE.-1) THEN
              IS2=1
              D2 =XX(3,LS)
              V2 =WW(LS,J,K)
            ENDIF
          ENDIF
          LS=J-1
          IF (NF(I,LS,K).EQ.0 .OR. NF(I,LS,L1).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(I,LS,L1).NE.-1) THEN
              IS3=1
              D3 =YY(3,J)
              V3 =WW(I,LS,K)
            ENDIF
          ENDIF
          LS=J+1
          IF (NF(I,LS,K).EQ.0 .OR. NF(I,LS,L1).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(I,LS,L1).NE.-1) THEN
              IS4=1
              D4 =YY(3,LS)
              V4 =WW(I,LS,K)
            ENDIF
          ENDIF
          ISS=IS1+IS2+IS3+IS4
          IF (ISS.LE.0) THEN
            IF (IBSUW0.EQ.0 .AND. NF(I,J,K+2).EQ.0) THEN
              V0=V0+(V0-WW(I,J,K+2))*ZZ(2,K)/ZZ(2,K+1)
            ENDIF
            WW(I,J,K)=V0
          ELSE
            DS=D0+D1+D2+D3+D4
            WW(I,J,K)=( (DS-D0)*V0+(DS-D1)*V1+(DS-D2)*V2
     &                 +(DS-D3)*V3+(DS-D4)*V4           )/DS/DBLE(ISS)
          ENDIF
        ELSEIF (NF0.EQ.3) THEN
          D0=YY(2,J)
          V0=VV(I,J,K)
          L1=J+1
          LS=I-1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,L1,K).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,L1,K).NE.-1) THEN
              IS1=1
              D1 =XX(3,I)
              V1 =VV(LS,L1,K)
            ENDIF
          ENDIF
          LS=I+1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,L1,K).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,L1,K).NE.-1) THEN
              IS2=1
              D2 =XX(3,LS)
              V2 =VV(LS,L1,K)
            ENDIF
          ENDIF
          LS=K-1
          IF (NF(I,J,LS).EQ.0 .OR. NF(I,L1,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(I,L1,LS).NE.-1) THEN
              IS3=1
              D3 =ZZ(3,K)
              V3 =VV(I,L1,LS)
            ENDIF
          ENDIF
          LS=K+1
          IF (NF(I,J,LS).EQ.0 .OR. NF(I,L1,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(I,L1,LS).NE.-1) THEN
              IS4=1
              D4 =ZZ(3,LS)
              V4 =VV(I,L1,LS)
            ENDIF
          ENDIF
          ISS=IS1+IS2+IS3+IS4
          IF (ISS.LE.0) THEN
            IF (IBSUW0.EQ.0 .AND. NF(I,J-2,K).EQ.0) THEN
              V0=V0+(V0-VV(I,J-1,K))*YY(2,J)/YY(2,J-1)
            ENDIF
            VV(I,J+1,K)=V0
          ELSE
            DS=D0+D1+D2+D3+D4
            VV(I,J+1,K)=( (DS-D0)*V0+(DS-D1)*V1+(DS-D2)*V2
     &                   +(DS-D3)*V3+(DS-D4)*V4           )/DS/DBLE(ISS)
          ENDIF
        ELSEIF (NF0.EQ.4) THEN
          D0=YY(2,J)
          V0=VV(I,J+1,K)
          L1=J-1
          LS=I-1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,L1,K).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,L1,K).NE.-1) THEN
              IS1=1
              D1 =XX(3,I)
              V1 =VV(LS,J,K)
            ENDIF
          ENDIF
          LS=I+1
          IF (NF(LS,J,K).EQ.0 .OR. NF(LS,L1,K).EQ.0) THEN
            IF (NF(LS,J,K).NE.-1 .AND. NF(LS,L1,K).NE.-1) THEN
              IS2=1
              D2 =XX(3,LS)
              V2 =VV(LS,J,K)
            ENDIF
          ENDIF
          LS=K-1
          IF (NF(I,J,LS).EQ.0 .OR. NF(I,L1,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(I,L1,LS).NE.-1) THEN
              IS3=1
              D3 =ZZ(3,K)
              V3 =VV(I,J,LS)
            ENDIF
          ENDIF
          LS=K+1
          IF (NF(I,J,LS).EQ.0 .OR. NF(I,L1,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(I,L1,LS).NE.-1) THEN
              IS4=1
              D4 =ZZ(3,LS)
              V4 =VV(I,J,LS)
            ENDIF
          ENDIF
          ISS=IS1+IS2+IS3+IS4
          IF (ISS.LE.0) THEN
            IF (IBSUW0.EQ.0 .AND. NF(I,J+2,K).EQ.0) THEN
              V0=V0+(V0-VV(I,J+2,K))*YY(2,J)/YY(2,J+1)
            ENDIF
            VV(I,J,K)=V0
          ELSE
            DS=D0+D1+D2+D3+D4
            VV(I,J,K)=( (DS-D0)*V0+(DS-D1)*V1+(DS-D2)*V2
     &                 +(DS-D3)*V3+(DS-D4)*V4           )/DS/DBLE(ISS)
          ENDIF
        ELSEIF (NF0.EQ.1) THEN
          D0=XX(2,I)
          V0=UU(I,J,K)
          L1=I+1
          LS=J-1
          IF (NF(I,LS,K).EQ.0 .OR. NF(L1,LS,K).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(L1,LS,K).NE.-1) THEN
              IS1=1
              D1 =YY(3,J)
              V1 =UU(L1,LS,K)
            ENDIF
          ENDIF
          LS=J+1
          IF (NF(I,LS,K).EQ.0 .OR. NF(L1,LS,K).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(L1,LS,K).NE.-1) THEN
              IS2=1
              D2 =YY(3,LS)
              V2 =UU(L1,LS,K)
            ENDIF
          ENDIF
          LS=K-1
          IF (NF(I,J,LS).EQ.0 .OR. NF(L1,J,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(L1,J,LS).NE.-1) THEN
              IS3=1
              D3 =ZZ(3,K)
              V3 =UU(L1,J,LS)
            ENDIF
          ENDIF
          LS=K+1
          IF (NF(I,J,LS).EQ.0 .OR. NF(L1,J,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(L1,J,LS).NE.-1) THEN
              IS4=1
              D4 =ZZ(3,LS)
              V4 =UU(L1,J,LS)
            ENDIF
          ENDIF
          ISS=IS1+IS2+IS3+IS4
          IF (ISS.LE.0) THEN
            IF (IBSUW0.EQ.0 .AND. NF(I-2,J,K).EQ.0) THEN
              V0=V0+(V0-UU(I-1,J,K))*XX(2,I)/XX(2,I-1)
            ENDIF
            UU(I+1,J,K)=V0
          ELSE
            DS=D0+D1+D2+D3+D4
            UU(I+1,J,K)=( (DS-D0)*V0+(DS-D1)*V1+(DS-D2)*V2
     &                   +(DS-D3)*V3+(DS-D4)*V4           )/DS/DBLE(ISS)
          ENDIF
        ELSEIF (NF0.EQ.2) THEN
          D0=XX(2,I)
          V0=UU(I+1,J,K)
          L1=I-1
          LS=J-1
          IF (NF(I,LS,K).EQ.0 .OR. NF(L1,LS,K).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(L1,LS,K).NE.-1) THEN
              IS1=1
              D1 =YY(3,J)
              V1 =UU(I,LS,K)
            ENDIF
          ENDIF
          LS=J+1
          IF (NF(I,LS,K).EQ.0 .OR. NF(L1,LS,K).EQ.0) THEN
            IF (NF(I,LS,K).NE.-1 .AND. NF(L1,LS,K).NE.-1) THEN
              IS2=1
              D2 =YY(3,LS)
              V2 =UU(I,LS,K)
            ENDIF
          ENDIF
          LS=K-1
          IF (NF(I,J,LS).EQ.0 .OR. NF(L1,J,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(L1,J,LS).NE.-1) THEN
              IS3=1
              D3 =ZZ(3,K)
              V3 =UU(I,J,LS)
            ENDIF
          ENDIF
          LS=K+1
          IF (NF(I,J,LS).EQ.0 .OR. NF(L1,J,LS).EQ.0) THEN
            IF (NF(I,J,LS).NE.-1 .AND. NF(L1,J,LS).NE.-1) THEN
              IS4=1
              D4 =ZZ(3,LS)
              V4 =UU(I,J,LS)
            ENDIF
          ENDIF
          ISS=IS1+IS2+IS3+IS4
          IF (ISS.LE.0) THEN
            IF (IBSUW0.EQ.0 .AND. NF(I+2,J,K).EQ.0) THEN
              V0=V0+(V0-UU(I+2,J,K))*XX(2,I)/XX(2,I+1)
            ENDIF
            UU(I,J,K)=V0
          ELSE
            DS=D0+D1+D2+D3+D4
            UU(I,J,K)=( (DS-D0)*V0+(DS-D1)*V1+(DS-D2)*V2
     &                 +(DS-D3)*V3+(DS-D4)*V4           )/DS/DBLE(ISS)
          ENDIF
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
