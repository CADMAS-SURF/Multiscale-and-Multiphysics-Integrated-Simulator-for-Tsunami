      SUBROUTINE VF_BSUWT(XX,YY,ZZ,UU,VV,WW,DBUF,NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_BSUWT:非計算面の流速をゼロとし、自由表面の接線方向流速を設定
CD      (1)通常面であり、かつ、流体セルに接していない面に設定
CD      (2)境界面の流速はゼロとしない

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
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- x方向流速を設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIS.EQ.1) IA=IA+1
      DO 120 K=2,NUMK-1
        DO 110 J=JA,JB
          DO 100 I=IA,IB
C           * 通常の面
            IF (INDX(I,J,K).EQ.0) THEN
              I1=I-1
              NL=NF(I1,J,K)
              NR=NF(I ,J,K)
C             * 両側を気体に挟まれている面
              IF     (NL.EQ.8 .AND. NR.EQ.8) THEN
                UU(I,J,K)=0.0D0
C             * 流体に接していない面
              ELSEIF (NL.NE.0 .AND. NR.NE.0) THEN
                DL=0.0D0
                VL=0.0D0
                DR=0.0D0
                VR=0.0D0
                IF (NL.EQ.3 .OR. NR.EQ.3) THEN
                  V0=UU(I,J-1,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=J-1
                    L2=J-2
                    IF (NF(I1,L1,K).NE.-1 .AND.
     &                  NF(I ,L1,K).NE.-1      ) THEN
                      IF (NF(I1,L2,K).NE.-1 .AND.
     &                    NF(I ,L2,K).NE.-1      ) THEN
                        IF (NF(I1,L2,K).EQ.0 .OR.
     &                      NF(I ,L2,K).EQ.0     ) THEN
                          V0=V0+(V0-UU(I,L2,K))*YY(3,J)/YY(3,L1)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.3) THEN
                    DL=YY(3,J)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.3) THEN
                    DR=YY(3,J)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.4 .OR. NR.EQ.4) THEN
                  V0=UU(I,J+1,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=J+1
                    L2=J+2
                    IF (NF(I1,L1,K).NE.-1 .AND.
     &                  NF(I ,L1,K).NE.-1      ) THEN
                      IF (NF(I1,L2,K).NE.-1 .AND.
     &                    NF(I ,L2,K).NE.-1      ) THEN
                        IF (NF(I1,L2,K).EQ.0 .OR.
     &                      NF(I ,L2,K).EQ.0     ) THEN
                          V0=V0+(V0-UU(I,L2,K))*YY(3,L1)/YY(3,L2)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.4) THEN
                    DL=YY(3,J+1)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.4) THEN
                    DR=YY(3,J+1)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.5 .OR. NR.EQ.5) THEN
                  V0=UU(I,J,K-1)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=K-1
                    L2=K-2
                    IF (NF(I1,J,L1).NE.-1 .AND.
     &                  NF(I ,J,L1).NE.-1      ) THEN
                      IF (NF(I1,J,L2).NE.-1 .AND.
     &                    NF(I ,J,L2).NE.-1      ) THEN
                        IF (NF(I1,J,L2).EQ.0 .OR.
     &                      NF(I ,J,L2).EQ.0     ) THEN
                          V0=V0+(V0-UU(I,J,L2))*ZZ(3,K)/ZZ(3,L1)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.5) THEN
                    DL=ZZ(3,K)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.5) THEN
                    DR=ZZ(3,K)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.6 .OR. NR.EQ.6) THEN
                  V0=UU(I,J,K+1)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=K+1
                    L2=K+2
                    IF (NF(I1,J,L1).NE.-1 .AND.
     &                  NF(I ,J,L1).NE.-1      ) THEN
                      IF (NF(I1,J,L2).NE.-1 .AND.
     &                    NF(I ,J,L2).NE.-1      ) THEN
                        IF (NF(I1,J,L2).EQ.0 .OR.
     &                      NF(I ,J,L2).EQ.0     ) THEN
                          V0=V0+(V0-UU(I,J,L2))*ZZ(3,L1)/ZZ(3,L2)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.6) THEN
                    DL=ZZ(3,K+1)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.6) THEN
                    DR=ZZ(3,K+1)
                    VR=V0
                  ENDIF
                ENDIF
                IF     (NL.EQ.8) THEN
                  UU(I,J,K)=VR
                ELSEIF (NR.EQ.8) THEN
                  UU(I,J,K)=VL
                ELSE
                  UU(I,J,K)=(DR*VL+DL*VR)/(DR+DL)
                ENDIF
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- y方向流速を設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMJS.EQ.1) JA=JA+1
      DO 220 K=2,NUMK-1
        DO 210 J=JA,JB
          DO 200 I=IA,IB
C           * 通常の面
            IF (INDY(I,J,K).EQ.0) THEN
              J1=J-1
              NL=NF(I,J1,K)
              NR=NF(I,J ,K)
C             * 両側を気体に挟まれている面
              IF     (NL.EQ.8 .AND. NR.EQ.8) THEN
                VV(I,J,K)=0.0D0
C             * 流体に接していない面
              ELSEIF (NL.NE.0 .AND. NR.NE.0) THEN
                DL=0.0D0
                VL=0.0D0
                DR=0.0D0
                VR=0.0D0
                IF (NL.EQ.1 .OR. NR.EQ.1) THEN
                  V0=VV(I-1,J,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=I-1
                    L2=I-2
                    IF (NF(L1,J1,K).NE.-1 .AND.
     &                  NF(L1,J ,K).NE.-1      ) THEN
                      IF (NF(L2,J1,K).NE.-1 .AND.
     &                    NF(L2,J ,K).NE.-1      ) THEN
                        IF (NF(L2,J1,K).EQ.0 .OR.
     &                      NF(L2,J ,K).EQ.0     ) THEN
                          V0=V0+(V0-VV(L2,J,K))*XX(3,I)/XX(3,L1)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.1) THEN
                    DL=XX(3,I)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.1) THEN
                    DR=XX(3,I)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.2 .OR. NR.EQ.2) THEN
                  V0=VV(I+1,J,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=I+1
                    L2=I+2
                    IF (NF(L1,J1,K).NE.-1 .AND.
     &                  NF(L1,J ,K).NE.-1      ) THEN
                      IF (NF(L2,J1,K).NE.-1 .AND.
     &                    NF(L2,J ,K).NE.-1      ) THEN
                        IF (NF(L2,J1,K).EQ.0 .OR.
     &                      NF(L2,J ,K).EQ.0     ) THEN
                          V0=V0+(V0-VV(L2,J,K))*XX(3,L1)/XX(3,L2)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.2) THEN
                    DL=XX(3,I+1)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.2) THEN
                    DR=XX(3,I+1)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.5 .OR. NR.EQ.5) THEN
                  V0=VV(I,J,K-1)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=K-1
                    L2=K-2
                    IF (NF(I,J1,L1).NE.-1 .AND.
     &                  NF(I,J ,L1).NE.-1      ) THEN
                      IF (NF(I,J1,L2).NE.-1 .AND.
     &                    NF(I,J ,L2).NE.-1      ) THEN
                        IF (NF(I,J1,L2).EQ.0 .OR.
     &                      NF(I,J ,L2).EQ.0     ) THEN
                          V0=V0+(V0-VV(I,J,L2))*ZZ(3,K)/ZZ(3,L1)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.5) THEN
                    DL=ZZ(3,K)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.5) THEN
                    DR=ZZ(3,K)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.6 .OR. NR.EQ.6) THEN
                  V0=VV(I,J,K+1)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=K+1
                    L2=K+2
                    IF (NF(I,J1,L1).NE.-1 .AND.
     &                  NF(I,J ,L1).NE.-1      ) THEN
                      IF (NF(I,J1,L2).NE.-1 .AND.
     &                    NF(I,J ,L2).NE.-1      ) THEN
                        IF (NF(I,J1,L2).EQ.0 .OR.
     &                      NF(I,J ,L2).EQ.0     ) THEN
                          V0=V0+(V0-VV(I,J,L2))*ZZ(3,L1)/ZZ(3,L2)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.6) THEN
                    DL=ZZ(3,K+1)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.6) THEN
                    DR=ZZ(3,K+1)
                    VR=V0
                  ENDIF
                ENDIF
                IF     (NL.EQ.8) THEN
                  VV(I,J,K)=VR
                ELSEIF (NR.EQ.8) THEN
                  VV(I,J,K)=VL
                ELSE
                  VV(I,J,K)=(DR*VL+DL*VR)/(DR+DL)
                ENDIF
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- z方向流速を設定 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      DO 320 K=3,NUMK-1
        DO 310 J=JA,JB
          DO 300 I=IA,IB
C           * 通常の面
            IF (INDZ(I,J,K).EQ.0) THEN
              K1=K-1
              NL=NF(I,J,K1)
              NR=NF(I,J,K )
C             * 両側を気体に挟まれている面
              IF     (NL.EQ.8 .AND. NR.EQ.8) THEN
                WW(I,J,K)=0.0D0
C             * 流体に接していない面
              ELSEIF (NL.NE.0 .AND. NR.NE.0) THEN
                DL=0.0D0
                VL=0.0D0
                DR=0.0D0
                VR=0.0D0
                IF (NL.EQ.1 .OR. NR.EQ.1) THEN
                  V0=WW(I-1,J,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=I-1
                    L2=I-2
                    IF (NF(L1,J,K1).NE.-1 .AND.
     &                  NF(L1,J,K ).NE.-1      ) THEN
                      IF (NF(L2,J,K1).NE.-1 .AND.
     &                    NF(L2,J,K ).NE.-1      ) THEN
                        IF (NF(L2,J,K1).EQ.0 .OR.
     &                      NF(L2,J,K ).EQ.0     ) THEN
                          V0=V0+(V0-WW(L2,J,K))*XX(3,I)/XX(3,L1)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.1) THEN
                    DL=XX(3,I)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.1) THEN
                    DR=XX(3,I)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.2 .OR. NR.EQ.2) THEN
                  V0=WW(I+1,J,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=I+1
                    L2=I+2
                    IF (NF(L1,J,K1).NE.-1 .AND.
     &                  NF(L1,J,K ).NE.-1      ) THEN
                      IF (NF(L2,J,K1).NE.-1 .AND.
     &                    NF(L2,J,K ).NE.-1      ) THEN
                        IF (NF(L2,J,K1).EQ.0 .OR.
     &                      NF(L2,J,K ).EQ.0     ) THEN
                          V0=V0+(V0-WW(L2,J,K))*XX(3,L1)/XX(3,L2)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.2) THEN
                    DL=XX(3,I+1)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.2) THEN
                    DR=XX(3,I+1)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.3 .OR. NR.EQ.3) THEN
                  V0=WW(I,J-1,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=J-1
                    L2=J-2
                    IF (NF(I,L1,K1).NE.-1 .AND.
     &                  NF(I,L1,K ).NE.-1      ) THEN
                      IF (NF(I,L2,K1).NE.-1 .AND.
     &                    NF(I,L2,K ).NE.-1      ) THEN
                        IF (NF(I,L2,K1).EQ.0 .OR.
     &                      NF(I,L2,K ).EQ.0     ) THEN
                          V0=V0+(V0-WW(I,L2,K))*YY(3,J)/YY(3,L1)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.3) THEN
                    DL=YY(3,J)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.3) THEN
                    DR=YY(3,J)
                    VR=V0
                  ENDIF
                ENDIF
                IF (NL.EQ.4 .OR. NR.EQ.4) THEN
                  V0=WW(I,J+1,K)
C                 * 線形補外の場合
                  IF (IBSUW0.EQ.0) THEN
                    L1=J+1
                    L2=J+2
                    IF (NF(I,L1,K1).NE.-1 .AND.
     &                  NF(I,L1,K ).NE.-1      ) THEN
                      IF (NF(I,L2,K1).NE.-1 .AND.
     &                    NF(I,L2,K ).NE.-1      ) THEN
                        IF (NF(I,L2,K1).EQ.0 .OR.
     &                      NF(I,L2,K ).EQ.0     ) THEN
                          V0=V0+(V0-WW(I,L2,K))*YY(3,L1)/YY(3,L2)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (NL.EQ.4) THEN
                    DL=YY(3,J+1)
                    VL=V0
                  ENDIF
                  IF (NR.EQ.4) THEN
                    DR=YY(3,J+1)
                    VR=V0
                  ENDIF
                ENDIF
                IF     (NL.EQ.8) THEN
                  WW(I,J,K)=VR
                ELSEIF (NR.EQ.8) THEN
                  WW(I,J,K)=VL
                ELSE
                  WW(I,J,K)=(DR*VL+DL*VR)/(DR+DL)
                ENDIF
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
