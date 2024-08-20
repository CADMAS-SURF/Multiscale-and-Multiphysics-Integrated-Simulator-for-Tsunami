      SUBROUTINE VF_FDROPF(XX,YY,ZZ,UU,VV,WW,FF,GGV,
     &                     DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                     DBUF,WKTX,WKTY,WKTZ,WKUU,WKVV,WKWW,WKFF,NF)

CD=== 概要 ===========================================================

CDT   VF_FDROPF:TimerDoor法による水滴の自由落下と流れ落ち

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    DROPTX(@FOR-3D@) : I/O : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@) : I/O : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@) : I/O : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@) : I/O : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@) : I/O : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@) : I/O : R*8 : 自由落下のz方向速度
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    WKTX(@FOR-3D@)   : OUT : R*8 : DROPTXの重み付きの値
CD    WKTY(@FOR-3D@)   : OUT : R*8 : DROPTYの重み付きの値
CD    WKTZ(@FOR-3D@)   : OUT : R*8 : DROPTZの重み付きの値
CD    WKUU(@FOR-3D@)   : OUT : R*8 : DROPUUの重み付きの値
CD    WKVV(@FOR-3D@)   : OUT : R*8 : DROPVVの重み付きの値
CD    WKWW(@FOR-3D@)   : OUT : R*8 : DROPWWの重み付きの値
CD    WKFF(@FOR-3D@)   : OUT : R*8 : 重みの合計
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK)
      DIMENSION DROPTX(NUMI,NUMJ,NUMK),DROPTY(NUMI,NUMJ,NUMK)
      DIMENSION DROPTZ(NUMI,NUMJ,NUMK),DROPUU(NUMI,NUMJ,NUMK)
      DIMENSION DROPVV(NUMI,NUMJ,NUMK),DROPWW(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION WKTX(NUMI,NUMJ,NUMK),WKTY(NUMI,NUMJ,NUMK)
      DIMENSION WKTZ(NUMI,NUMJ,NUMK),WKUU(NUMI,NUMJ,NUMK)
      DIMENSION WKVV(NUMI,NUMJ,NUMK),WKWW(NUMI,NUMJ,NUMK)
      DIMENSION WKFF(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9NF)

CD    -- 新しい水滴セルの抽出と、古い水滴処理の残骸の処理 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
CD          -- 新しい水滴セルに初速を与え、古い水滴セルは横移動 --
            IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
CD            -- 新しい水滴セルならば初速を与える --
              IF (DROPTX(I,J,K).LT.0.0D0)THEN
                DROPTX(I,J,K)=TNOW
                DROPTY(I,J,K)=TNOW
                DROPTZ(I,J,K)=TNOW
                IP=0
                UP=0.0D0
                VP=0.0D0
                WP=0.0D0
C               * 近傍に流体セルがあればそれを集める
                I1=I-1
                K1=K-1
                IF (NF(I1,J,K1).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J,K1)+UU(I1,J,K1))
                  VP=VP+0.5D0*(VV(I1,J+1,K1)+VV(I1,J,K1))
                  WP=WP+0.5D0*(WW(I1,J,K1+1)+WW(I1,J,K1))
                ENDIF
                I1=I+1
                K1=K-1
                IF (NF(I1,J,K1).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J,K1)+UU(I1,J,K1))
                  VP=VP+0.5D0*(VV(I1,J+1,K1)+VV(I1,J,K1))
                  WP=WP+0.5D0*(WW(I1,J,K1+1)+WW(I1,J,K1))
                ENDIF
                I1=I-1
                K1=K+1
                IF (NF(I1,J,K1).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J,K1)+UU(I1,J,K1))
                  VP=VP+0.5D0*(VV(I1,J+1,K1)+VV(I1,J,K1))
                  WP=WP+0.5D0*(WW(I1,J,K1+1)+WW(I1,J,K1))
                ENDIF
                I1=I+1
                K1=K+1
                IF (NF(I1,J,K1).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J,K1)+UU(I1,J,K1))
                  VP=VP+0.5D0*(VV(I1,J+1,K1)+VV(I1,J,K1))
                  WP=WP+0.5D0*(WW(I1,J,K1+1)+WW(I1,J,K1))
                ENDIF
                I1=I-1
                J1=J-1
                IF (NF(I1,J1,K).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J1,K).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J1,K)+UU(I1,J1,K))
                  VP=VP+0.5D0*(VV(I1,J1+1,K)+VV(I1,J1,K))
                  WP=WP+0.5D0*(WW(I1,J1,K+1)+WW(I1,J1,K))
                ENDIF
                I1=I+1
                J1=J-1
                IF (NF(I1,J1,K).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J1,K).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J1,K)+UU(I1,J1,K))
                  VP=VP+0.5D0*(VV(I1,J1+1,K)+VV(I1,J1,K))
                  WP=WP+0.5D0*(WW(I1,J1,K+1)+WW(I1,J1,K))
                ENDIF
                I1=I-1
                J1=J+1
                IF (NF(I1,J1,K).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J1,K).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J1,K)+UU(I1,J1,K))
                  VP=VP+0.5D0*(VV(I1,J1+1,K)+VV(I1,J1,K))
                  WP=WP+0.5D0*(WW(I1,J1,K+1)+WW(I1,J1,K))
                ENDIF
                I1=I+1
                J1=J+1
                IF (NF(I1,J1,K).EQ.0 .AND.
     &              (NF(I1,J,K).NE.-1 .OR. NF(I,J1,K).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I1+1,J1,K)+UU(I1,J1,K))
                  VP=VP+0.5D0*(VV(I1,J1+1,K)+VV(I1,J1,K))
                  WP=WP+0.5D0*(WW(I1,J1,K+1)+WW(I1,J1,K))
                ENDIF
                J1=J-1
                K1=K-1
                IF (NF(I,J1,K1).EQ.0 .AND.
     &              (NF(I,J1,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I+1,J1,K1)+UU(I,J1,K1))
                  VP=VP+0.5D0*(VV(I,J1+1,K1)+VV(I,J1,K1))
                  WP=WP+0.5D0*(WW(I,J1,K1+1)+WW(I,J1,K1))
                ENDIF
                J1=J+1
                K1=K-1
                IF (NF(I,J1,K1).EQ.0 .AND.
     &              (NF(I,J1,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I+1,J1,K1)+UU(I,J1,K1))
                  VP=VP+0.5D0*(VV(I,J1+1,K1)+VV(I,J1,K1))
                  WP=WP+0.5D0*(WW(I,J1,K1+1)+WW(I,J1,K1))
                ENDIF
                J1=J-1
                K1=K+1
                IF (NF(I,J1,K1).EQ.0 .AND.
     &              (NF(I,J1,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I+1,J1,K1)+UU(I,J1,K1))
                  VP=VP+0.5D0*(VV(I,J1+1,K1)+VV(I,J1,K1))
                  WP=WP+0.5D0*(WW(I,J1,K1+1)+WW(I,J1,K1))
                ENDIF
                J1=J+1
                K1=K+1
                IF (NF(I,J1,K1).EQ.0 .AND.
     &              (NF(I,J1,K).NE.-1 .OR. NF(I,J,K1).NE.-1)) THEN
                  IP=IP+1
                  UP=UP+0.5D0*(UU(I+1,J1,K1)+UU(I,J1,K1))
                  VP=VP+0.5D0*(VV(I,J1+1,K1)+VV(I,J1,K1))
                  WP=WP+0.5D0*(WW(I,J1,K1+1)+WW(I,J1,K1))
                ENDIF
C               * 流体セルがなければ、表面セルを集める
                IF (IP.EQ.0) THEN
                  I1=I-1
                  IF (NF(I1,J,K).NE.-1 .AND. NF(I1,J,K).NE.8) THEN
                    IP=IP+1
                    UP=UP+0.5D0*(UU(I1+1,J,K)+UU(I1,J,K))
                    VP=VP+0.5D0*(VV(I1,J+1,K)+VV(I1,J,K))
                    WP=WP+0.5D0*(WW(I1,J,K+1)+WW(I1,J,K))
                  ENDIF
                  I1=I+1
                  IF (NF(I1,J,K).NE.-1 .AND. NF(I1,J,K).NE.8) THEN
                    IP=IP+1
                    UP=UP+0.5D0*(UU(I1+1,J,K)+UU(I1,J,K))
                    VP=VP+0.5D0*(VV(I1,J+1,K)+VV(I1,J,K))
                    WP=WP+0.5D0*(WW(I1,J,K+1)+WW(I1,J,K))
                  ENDIF
                  K1=K-1
                  IF (NF(I,J,K1).NE.-1 .AND. NF(I,J,K1).NE.8) THEN
                    IP=IP+1
                    UP=UP+0.5D0*(UU(I+1,J,K1)+UU(I,J,K1))
                    VP=VP+0.5D0*(VV(I,J+1,K1)+VV(I,J,K1))
                    WP=WP+0.5D0*(WW(I,J,K1+1)+WW(I,J,K1))
                  ENDIF
                  K1=K+1
                  IF (NF(I,J,K1).NE.-1 .AND. NF(I,J,K1).NE.8) THEN
                    IP=IP+1
                    UP=UP+0.5D0*(UU(I+1,J,K1)+UU(I,J,K1))
                    VP=VP+0.5D0*(VV(I,J+1,K1)+VV(I,J,K1))
                    WP=WP+0.5D0*(WW(I,J,K1+1)+WW(I,J,K1))
                  ENDIF
                  J1=J-1
                  IF (NF(I,J1,K).NE.-1 .AND. NF(I,J1,K).NE.8) THEN
                    IP=IP+1
                    UP=UP+0.5D0*(UU(I+1,J1,K)+UU(I,J1,K))
                    VP=VP+0.5D0*(VV(I,J1+1,K)+VV(I,J1,K))
                    WP=WP+0.5D0*(WW(I,J1,K+1)+WW(I,J1,K))
                  ENDIF
                  J1=J+1
                  IF (NF(I,J1,K).NE.-1 .AND. NF(I,J1,K).NE.8) THEN
                    IP=IP+1
                    UP=UP+0.5D0*(UU(I+1,J1,K)+UU(I,J1,K))
                    VP=VP+0.5D0*(VV(I,J1+1,K)+VV(I,J1,K))
                    WP=WP+0.5D0*(WW(I,J1,K+1)+WW(I,J1,K))
                  ENDIF
                ENDIF
C               * 近傍に流速があったなら平均する
                IF (IP.GE.1) THEN
                  CP=1.0D0/DBLE(IP)
                  DROPUU(I,J,K)=UP*CP
                  DROPVV(I,J,K)=VP*CP
                  DROPWW(I,J,K)=WP*CP
C               * 無い場合は自分自身を見る
                ELSE
                  DROPUU(I,J,K)=0.5D0*(UU(I,J,K)+UU(I+1,J,K))
                  DROPVV(I,J,K)=0.5D0*(VV(I,J,K)+VV(I,J+1,K))
                  DROPWW(I,J,K)=0.5D0*(WW(I,J,K)+WW(I,J,K+1))
                ENDIF
C               * 障害物セルの上にのった水滴セルは止める
                IF (NF(I,J,K-1).EQ.-1) THEN
                  DROPUU(I,J,K)=0.0D0
                  DROPVV(I,J,K)=0.0D0
                  DROPWW(I,J,K)=0.0D0
                ENDIF
CD            -- 障害物セルの上にのった古い水滴セルは重力で横移動 --
              ELSEIF (NF(I,J,K-1).EQ.-1) THEN
                IF (NF(I-1,J,K).EQ.-1) THEN
                  H1=1.0D0
                ELSE
                  H1=1.0D0-GGV(I-1,J,K)
                ENDIF
                IF (NF(I+1,J,K).EQ.-1) THEN
                  H2=1.0D0
                ELSE
                  H2=1.0D0-GGV(I+1,J,K)
                ENDIF
                HX=0.5D0*(H2-H1)*ZZ(2,K)
                IF (NF(I,J-1,K).EQ.-1) THEN
                  H1=1.0D0
                ELSE
                  H1=1.0D0-GGV(I,J-1,K)
                ENDIF
                IF (NF(I,J+1,K).EQ.-1) THEN
                  H2=1.0D0
                ELSE
                  H2=1.0D0-GGV(I,J+1,K)
                ENDIF
                HY=0.5D0*(H2-H1)*ZZ(2,K)
                HX=HX/XX(2,I)
                HY=HY/YY(2,J)
                DN=1.0D0/(HX*HX+HY*HY+1.0D0)
                HX=-GRZ0*HX*DN
                HY=-GRZ0*HY*DN
                DROPUU(I,J,K)=DROPUU(I,J,K)+DTNOW*HX
                DROPVV(I,J,K)=DROPVV(I,J,K)+DTNOW*HY
              ENDIF
CD          -- 水滴でないのに水滴処理の残骸がある場合はきれいにする --
            ELSE
              DROPTX(I,J,K)=-1.0D10
              DROPTY(I,J,K)= 0.0D0
              DROPTZ(I,J,K)= 0.0D0
              DROPUU(I,J,K)= 0.0D0
              DROPVV(I,J,K)= 0.0D0
              DROPWW(I,J,K)= 0.0D0
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD1(DROPTX,DBUF,0)
      CALL VF_P3SRD1(DROPTY,DBUF,0)
      CALL VF_P3SRD1(DROPTZ,DBUF,0)
      CALL VF_P3SRD1(DROPUU,DBUF,0)
      CALL VF_P3SRD1(DROPVV,DBUF,0)
      CALL VF_P3SRD1(DROPWW,DBUF,0)

CD    -- X方向への水滴の移動 --
      DO 290 L=1,2
C       * X(+)方向へ
        IF (L.EQ.1) THEN
          IST=MYIE
          IEN=MYIS-1
          IF (MYMIS.EQ.1) IEN=MYIS
          IDL=-1
          SG =1.0D0
C       * X(-)方向へ
        ELSE
          IST=MYIS
          IEN=MYIE+1
          IF (MYMIE.EQ.1) IEN=MYIE
          IDL=1
          SG =-1.0D0
        ENDIF
C       * 重み付き値のクリア
        DO 220 K=2,NUMK-1
          DO 210 J=MYJS,MYJE
            DO 200 I=MYIS,MYIE
              WKTX(I,J,K)=0.0D0
              WKTY(I,J,K)=0.0D0
              WKTZ(I,J,K)=0.0D0
              WKUU(I,J,K)=0.0D0
              WKVV(I,J,K)=0.0D0
              WKWW(I,J,K)=0.0D0
              WKFF(I,J,K)=0.0D0
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
C       * 水滴の移動
        DO 250 I=IST,IEN,IDL
          I1=I-IDL
          DO 240 K=2,NUMK-1
            DO 230 J=MYJS,MYJE
C             * 水と重み付き値を移動
              DT=0.0D0
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8 .AND.
     &            NF(I1,J,K).NE.-1 .AND. SG*DROPUU(I,J,K).GT.0.0D0) THEN
                DT=XX(2,I)/ABS(DROPUU(I,J,K))
                IF((TNOW-DROPTX(I,J,K)).GT.DT)THEN
C                 * F値の移動
                  V0=XX(2,I )*YY(2,J)*ZZ(2,K)*GGV(I ,J,K)
                  V1=XX(2,I1)*YY(2,J)*ZZ(2,K)*GGV(I1,J,K)
                  FMOVE=MIN((1.0D0-FF(I1,J,K))*V1,FF(I,J,K)*V0)
                  FF(I ,J,K)=FF(I ,J,K)-FMOVE/V0
                  FF(I1,J,K)=FF(I1,J,K)+FMOVE/V1
C                 * 重み付き値の移動
                  IF (NF(I1,J,K).EQ.8) THEN
                    WKTX(I1,J,K)=WKTX(I1,J,K)+FMOVE*(DROPTX(I,J,K)+DT)
                    WKTY(I1,J,K)=WKTY(I1,J,K)+FMOVE* DROPTY(I,J,K)
                    WKTZ(I1,J,K)=WKTZ(I1,J,K)+FMOVE* DROPTZ(I,J,K)
                    WKUU(I1,J,K)=WKUU(I1,J,K)+FMOVE* DROPUU(I,J,K)
                    WKVV(I1,J,K)=WKVV(I1,J,K)+FMOVE* DROPVV(I,J,K)
                    WKWW(I1,J,K)=WKWW(I1,J,K)+FMOVE* DROPWW(I,J,K)
                    WKFF(I1,J,K)=WKFF(I1,J,K)+FMOVE
                  ENDIF
                ELSE
                  DT=0.0D0
                ENDIF
              ENDIF
C             * 動かない、または、残った水の重み付き値
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
                FOLD=FF(I,J,K)*XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
                WKTX(I,J,K)=WKTX(I,J,K)+FOLD*(DROPTX(I,J,K)+DT)
                WKTY(I,J,K)=WKTY(I,J,K)+FOLD* DROPTY(I,J,K)
                WKTZ(I,J,K)=WKTZ(I,J,K)+FOLD* DROPTZ(I,J,K)
                WKUU(I,J,K)=WKUU(I,J,K)+FOLD* DROPUU(I,J,K)
                WKVV(I,J,K)=WKVV(I,J,K)+FOLD* DROPVV(I,J,K)
                WKWW(I,J,K)=WKWW(I,J,K)+FOLD* DROPWW(I,J,K)
                WKFF(I,J,K)=WKFF(I,J,K)+FOLD
C             * (I,J,K)セルに水がなくなったら、セルは水滴でなくなる
              ELSE
                DROPTX(I,J,K)=-1.0D10
                DROPTY(I,J,K)= 0.0D0
                DROPTZ(I,J,K)= 0.0D0
                DROPUU(I,J,K)= 0.0D0
                DROPVV(I,J,K)= 0.0D0
                DROPWW(I,J,K)= 0.0D0
              ENDIF
 230        CONTINUE
 240      CONTINUE
 250    CONTINUE
C       * 重み付き値の平均を計算
        DO 280 K=2,NUMK-1
          DO 270 J=MYJS,MYJE
            DO 260 I=MYIS,MYIE
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
                W=1/WKFF(I,J,K)
                DROPTX(I,J,K)=WKTX(I,J,K)*W
                DROPTY(I,J,K)=WKTY(I,J,K)*W
                DROPTZ(I,J,K)=WKTZ(I,J,K)*W
                DROPUU(I,J,K)=WKUU(I,J,K)*W
                DROPVV(I,J,K)=WKVV(I,J,K)*W
                DROPWW(I,J,K)=WKWW(I,J,K)*W
              ENDIF
 260        CONTINUE
 270      CONTINUE
 280    CONTINUE
 290  CONTINUE

      CALL VF_P3SRD1(FF    ,DBUF,0)
      CALL VF_P3SRD1(DROPTX,DBUF,0)
      CALL VF_P3SRD1(DROPTY,DBUF,0)
      CALL VF_P3SRD1(DROPTZ,DBUF,0)
      CALL VF_P3SRD1(DROPUU,DBUF,0)
      CALL VF_P3SRD1(DROPVV,DBUF,0)
      CALL VF_P3SRD1(DROPWW,DBUF,0)

CD    -- Y方向への水滴の移動 --
      DO 390 L=1,2
C       * Y(+)方向へ
        IF (L.EQ.1) THEN
          JST=MYJE
          JEN=MYJS-1
          IF (MYMJS.EQ.1) JEN=MYJS
          JDL=-1
          SG =1.0D0
C       * Y(-)方向へ
        ELSE
          JST=MYJS
          JEN=MYJE+1
          IF (MYMJE.EQ.1) JEN=MYJE
          JDL=1
          SG =-1.0D0
        ENDIF
C       * 重み付き値のクリア
        DO 320 K=2,NUMK-1
          DO 310 J=MYJS,MYJE
            DO 300 I=MYIS,MYIE
              WKTX(I,J,K)=0.0D0
              WKTY(I,J,K)=0.0D0
              WKTZ(I,J,K)=0.0D0
              WKUU(I,J,K)=0.0D0
              WKVV(I,J,K)=0.0D0
              WKWW(I,J,K)=0.0D0
              WKFF(I,J,K)=0.0D0
 300        CONTINUE
 310      CONTINUE
 320    CONTINUE
C       * 水滴の移動
        DO 350 J=JST,JEN,JDL
          J1=J-JDL
          DO 340 K=2,NUMK-1
            DO 330 I=MYIS,MYIE
C             * 水と重み付き値を移動
              DT=0.0D0
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8 .AND.
     &            NF(I,J1,K).NE.-1 .AND. SG*DROPVV(I,J,K).GT.0.0D0) THEN
                DT=YY(2,J)/ABS(DROPVV(I,J,K))
                IF((TNOW-DROPTY(I,J,K)).GT.DT)THEN
C                 * F値の移動
                  V0=XX(2,I)*YY(2,J )*ZZ(2,K)*GGV(I,J ,K)
                  V1=XX(2,I)*YY(2,J1)*ZZ(2,K)*GGV(I,J1,K)
                  FMOVE=MIN((1.0D0-FF(I,J1,K))*V1,FF(I,J,K)*V0)
                  FF(I,J ,K)=FF(I,J ,K)-FMOVE/V0
                  FF(I,J1,K)=FF(I,J1,K)+FMOVE/V1
C                 * 重み付き値の移動
                  IF (NF(I,J1,K).EQ.8) THEN
                    WKTX(I,J1,K)=WKTX(I,J1,K)+FMOVE* DROPTX(I,J,K)
                    WKTY(I,J1,K)=WKTY(I,J1,K)+FMOVE*(DROPTY(I,J,K)+DT)
                    WKTZ(I,J1,K)=WKTZ(I,J1,K)+FMOVE* DROPTZ(I,J,K)
                    WKUU(I,J1,K)=WKUU(I,J1,K)+FMOVE* DROPUU(I,J,K)
                    WKVV(I,J1,K)=WKVV(I,J1,K)+FMOVE* DROPVV(I,J,K)
                    WKWW(I,J1,K)=WKWW(I,J1,K)+FMOVE* DROPWW(I,J,K)
                    WKFF(I,J1,K)=WKFF(I,J1,K)+FMOVE
                  ENDIF
                ELSE
                  DT=0.0D0
                ENDIF
              ENDIF
C             * 動かない、または、残った水の重み付き値
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
                FOLD=FF(I,J,K)*XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
                WKTX(I,J,K)=WKTX(I,J,K)+FOLD* DROPTX(I,J,K)
                WKTY(I,J,K)=WKTY(I,J,K)+FOLD*(DROPTY(I,J,K)+DT)
                WKTZ(I,J,K)=WKTZ(I,J,K)+FOLD* DROPTZ(I,J,K)
                WKUU(I,J,K)=WKUU(I,J,K)+FOLD* DROPUU(I,J,K)
                WKVV(I,J,K)=WKVV(I,J,K)+FOLD* DROPVV(I,J,K)
                WKWW(I,J,K)=WKWW(I,J,K)+FOLD* DROPWW(I,J,K)
                WKFF(I,J,K)=WKFF(I,J,K)+FOLD
C             * (I,J,K)セルに水がなくなったら、セルは水滴でなくなる
              ELSE
                DROPTX(I,J,K)=-1.0D10
                DROPTY(I,J,K)= 0.0D0
                DROPTZ(I,J,K)= 0.0D0
                DROPUU(I,J,K)= 0.0D0
                DROPVV(I,J,K)= 0.0D0
                DROPWW(I,J,K)= 0.0D0
              ENDIF
 330        CONTINUE
 340      CONTINUE
 350    CONTINUE
C       * 重み付き値の平均を計算
        DO 380 K=2,NUMK-1
          DO 370 J=MYJS,MYJE
            DO 360 I=MYIS,MYIE
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
                W=1/WKFF(I,J,K)
                DROPTX(I,J,K)=WKTX(I,J,K)*W
                DROPTY(I,J,K)=WKTY(I,J,K)*W
                DROPTZ(I,J,K)=WKTZ(I,J,K)*W
                DROPUU(I,J,K)=WKUU(I,J,K)*W
                DROPVV(I,J,K)=WKVV(I,J,K)*W
                DROPWW(I,J,K)=WKWW(I,J,K)*W
              ENDIF
 360        CONTINUE
 370      CONTINUE
 380    CONTINUE
 390  CONTINUE

      CALL VF_P3SRD1(FF    ,DBUF,0)
      CALL VF_P3SRD1(DROPTX,DBUF,0)
      CALL VF_P3SRD1(DROPTY,DBUF,0)
      CALL VF_P3SRD1(DROPTZ,DBUF,0)
      CALL VF_P3SRD1(DROPUU,DBUF,0)
      CALL VF_P3SRD1(DROPVV,DBUF,0)
      CALL VF_P3SRD1(DROPWW,DBUF,0)

CD    -- Z方向への水滴の移動 --
      DO 490 L=1,2
C       * Z(+)方向へ
        IF (L.EQ.1) THEN
          KST=NUMK-1
          KEN=2
          KDL=-1
C       * Z(-)方向へ
        ELSE
          KST=2
          KEN=NUMK-1
          KDL=1
        ENDIF
C       * 重み付き値のクリア
        DO 420 K=2,NUMK-1
          DO 410 J=MYJS,MYJE
            DO 400 I=MYIS,MYIE
              WKTX(I,J,K)=0.0D0
              WKTY(I,J,K)=0.0D0
              WKTZ(I,J,K)=0.0D0
              WKUU(I,J,K)=0.0D0
              WKVV(I,J,K)=0.0D0
              WKWW(I,J,K)=0.0D0
              WKFF(I,J,K)=0.0D0
 400        CONTINUE
 410      CONTINUE
 420    CONTINUE
C       * 水滴の移動
        DO 450 K=KST,KEN,KDL
          K1=K-KDL
          X=SQRT(2.0D0*GRZ0*ZZ(2,K))
          DO 440 J=MYJS,MYJE
            DO 430 I=MYIS,MYIE
C             * 水と重み付き値を移動
              DT=0.0D0
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8 .AND.
     &            NF(I,J,K1).NE.-1                              ) THEN
                IF (L.EQ.1) THEN
                  IF (DROPWW(I,J,K).GT.X) THEN
                    DT=(DROPWW(I,J,K)-SQRT(DROPWW(I,J,K)**2-X**2))/GRZ0
                  ENDIF
                ELSE
                  IF (DROPWW(I,J,K).LE.X) THEN
                    IF (DROPWW(I,J,K).LE.0.0D0) THEN
                      DT=(DROPWW(I,J,K)
     &                    +SQRT(DROPWW(I,J,K)**2+X**2))/GRZ0
                    ELSE
                      DT=2.0D0*DROPWW(I,J,K)/GRZ0
                    ENDIF
                  ENDIF
                ENDIF
                IF (DT.GT.0.0D0 .AND. (TNOW-DROPTZ(I,J,K)).GT.DT) THEN
C                 * F値の移動
                  V0=XX(2,I)*YY(2,J)*ZZ(2,K )*GGV(I,J,K )
                  V1=XX(2,I)*YY(2,J)*ZZ(2,K1)*GGV(I,J,K1)
                  FMOVE=MIN((1.0D0-FF(I,J,K1))*V1,FF(I,J,K)*V0)
                  FF(I,J,K )=FF(I,J,K )-FMOVE/V0
                  FF(I,J,K1)=FF(I,J,K1)+FMOVE/V1
C                 * 重み付き値の移動
                  IF (NF(I,J,K1).EQ.8) THEN
                    WKTX(I,J,K1)=WKTX(I,J,K1)+FMOVE* DROPTX(I,J,K)
                    WKTY(I,J,K1)=WKTY(I,J,K1)+FMOVE* DROPTY(I,J,K)
                    WKTZ(I,J,K1)=WKTZ(I,J,K1)+FMOVE*(DROPTZ(I,J,K)+DT)
                    WKUU(I,J,K1)=WKUU(I,J,K1)+FMOVE* DROPUU(I,J,K)
                    WKVV(I,J,K1)=WKVV(I,J,K1)+FMOVE* DROPVV(I,J,K)
                    WKWW(I,J,K1)=WKWW(I,J,K1)
     &                           +FMOVE*(DROPWW(I,J,K)-DT*GRZ0)
                    WKFF(I,J,K1)=WKFF(I,J,K1)+FMOVE
                  ENDIF
                ELSE
                  DT=0.0D0
                ENDIF
              ENDIF
C             * 動かない、または、残った水の重み付き値
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
                FOLD=FF(I,J,K)*XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
                WKTX(I,J,K)=WKTX(I,J,K)+FOLD* DROPTX(I,J,K)
                WKTY(I,J,K)=WKTY(I,J,K)+FOLD* DROPTY(I,J,K)
                WKTZ(I,J,K)=WKTZ(I,J,K)+FOLD*(DROPTZ(I,J,K)+DT)
                WKUU(I,J,K)=WKUU(I,J,K)+FOLD* DROPUU(I,J,K)
                WKVV(I,J,K)=WKVV(I,J,K)+FOLD* DROPVV(I,J,K)
                WKWW(I,J,K)=WKWW(I,J,K)+FOLD*(DROPWW(I,J,K)-DT*GRZ0)
                WKFF(I,J,K)=WKFF(I,J,K)+FOLD
C             * (I,J,K)セルに水がなくなったら、セルは水滴でなくなる
              ELSE
                DROPTX(I,J,K)=-1.0D10
                DROPTY(I,J,K)= 0.0D0
                DROPTZ(I,J,K)= 0.0D0
                DROPUU(I,J,K)= 0.0D0
                DROPVV(I,J,K)= 0.0D0
                DROPWW(I,J,K)= 0.0D0
              ENDIF
 430        CONTINUE
 440      CONTINUE
 450    CONTINUE
C       * 重み付き値の平均を計算
        DO 480 K=2,NUMK-1
          DO 470 J=MYJS,MYJE
            DO 460 I=MYIS,MYIE
              IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
                W=1/WKFF(I,J,K)
                DROPTX(I,J,K)=WKTX(I,J,K)*W
                DROPTY(I,J,K)=WKTY(I,J,K)*W
                DROPTZ(I,J,K)=WKTZ(I,J,K)*W
                DROPUU(I,J,K)=WKUU(I,J,K)*W
                DROPVV(I,J,K)=WKVV(I,J,K)*W
                DROPWW(I,J,K)=WKWW(I,J,K)*W
C               -- 水路天井にあたるようなら止める --
                IF (L.EQ.1) THEN
                  X=SQRT(2.0D0*GRZ0*ZZ(2,K))
                  IF (NF(I,J,K+1).EQ.-1 .AND. DROPWW(I,J,K).GT.X) THEN
                    DROPUU(I,J,K)=0.0D0
                    DROPVV(I,J,K)=0.0D0
                    DROPWW(I,J,K)=0.0D0
                  ENDIF
                ENDIF
              ENDIF
 460        CONTINUE
 470      CONTINUE
 480    CONTINUE
 490  CONTINUE

      CALL VF_P3SRD2(FF    ,DBUF,0)
      CALL VF_P3SRD2(DROPTX,DBUF,0)
      CALL VF_P3SRD2(DROPTY,DBUF,0)
      CALL VF_P3SRD2(DROPTZ,DBUF,0)
      CALL VF_P3SRD2(DROPUU,DBUF,0)
      CALL VF_P3SRD2(DROPVV,DBUF,0)
      CALL VF_P3SRD2(DROPWW,DBUF,0)

C     * デバッグ出力
C      DO 520 K=2,NUMK-1
C        DO 510 J=MYJS,MYJE
C          DO 500 I=MYIS,MYIE
C            IF (FF(I,J,K).GT.FLOWER .AND. NF(I,J,K).EQ.8) THEN
C              WRITE(*,9510) 'FDROPF',MYRANK,TNOW,
C     &                      0.5D0*(XX(1,I)+XX(1,I+1)),
C     &                      0.5D0*(YY(1,J)+YY(1,J+1)),
C     &                      0.5D0*(ZZ(1,K)+ZZ(1,K+1)),
C     &                      GGV(I,J,K)*FF(I,J,K)
C 9510         FORMAT( ' ',A,I5,100(' ',1PE12.5:))
C            ENDIF
C 500      CONTINUE
C 510    CONTINUE
C 520  CONTINUE

C     -- 実行文の終了 --
      CALL VF_A2CPUT(0,ICPUEN,KCP9NF)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
