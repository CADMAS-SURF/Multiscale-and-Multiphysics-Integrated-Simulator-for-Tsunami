      SUBROUTINE VF_FPVCPP(XX,YY,ZZ,UU,VV,WW,PP,FF,GGV,GGX,GGY,GGZ,
     &                     DBUF,PPPVC,NF,INDX,INDY,INDZ,INDB,IPVC)

CD=== 概要 ===========================================================

CDT   VF_FPVCPP:空気圧計算用圧力を計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)     : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN  : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN  : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN  : R*8 : z方向面積透過率
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    PPPVC(@FOR-3D@)  : OUT : R*8 : 空気圧の計算用の圧力
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    IPVC(@FOR-3D@)   : IN  : I*4 : 空気圧の計算用インデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF),PPPVC(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)    ,IPVC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- ゼロクリア --
      DO 100 L=1,NPVCB
        IPVCBC(L)=0
        PVCDIV(L)=0.0D0
        PVCPES(L)=0.0D0
        PVCPFS(L)=0.0D0
        PVCVES(L)=0.0D0
        PVCVFS(L)=0.0D0
 100  CONTINUE

CD    -- 造波、放射、フリーに接する気泡を探す --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            L =IPVC(I,J,K)
            IF (L.GE.1) THEN
              IP=I+1
              JP=J+1
              KP=K+1
              M=INDX(I ,J,K)
              IF (M.GE.1) THEN
                M=INDB(3,M)
                IF (M.EQ.4 .OR. M.EQ.5 .OR. M.EQ.7) IPVCBC(L)=1
              ENDIF
              M=INDX(IP,J,K)
              IF (M.GE.1) THEN
                M=INDB(3,M)
                IF (M.EQ.4 .OR. M.EQ.5 .OR. M.EQ.7) IPVCBC(L)=1
              ENDIF
              M=INDY(I,J ,K)
              IF (M.GE.1) THEN
                M=INDB(3,M)
                IF (M.EQ.4 .OR. M.EQ.5 .OR. M.EQ.7) IPVCBC(L)=1
              ENDIF
              M=INDY(I,JP,K)
              IF (M.GE.1) THEN
                M=INDB(3,M)
                IF (M.EQ.4 .OR. M.EQ.5 .OR. M.EQ.7) IPVCBC(L)=1
              ENDIF
              M=INDZ(I,J,K )
              IF (M.GE.1) THEN
                M=INDB(3,M)
                IF (M.EQ.4 .OR. M.EQ.5 .OR. M.EQ.7) IPVCBC(L)=1
              ENDIF
              M=INDZ(I,J,KP)
              IF (M.GE.1) THEN
                M=INDB(3,M)
                IF (M.EQ.4 .OR. M.EQ.5 .OR. M.EQ.7) IPVCBC(L)=1
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
      IF (NPROCS.NE.1) THEN
        DO 230 L=1,NPVCB
          M=IPVCBC(L)
          CALL VF_P1SUMI(M,IPVCBC(L))
 230    CONTINUE
      ENDIF

CD    -- Div(=PVCDIV)とVsum(=PVCVES)を計算し、DivにDiv/Vsumを格納 --
      DO 320 K=2,NUMK-1
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF     (NF(I,J,K).EQ.8) THEN
              L =IPVC(I,J,K)
              V =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
              DV=( XX(4,I)*( GGX(I+1,J,K)*UU(I+1,J,K)
     &                      -GGX(I  ,J,K)*UU(I  ,J,K))
     &            +YY(4,J)*( GGY(I,J+1,K)*VV(I,J+1,K)
     &                      -GGY(I,J  ,K)*VV(I,J  ,K))
     &            +ZZ(4,K)*( GGZ(I,J,K+1)*WW(I,J,K+1)
     &                      -GGZ(I,J,K  )*WW(I,J,K  )))*V
              PVCVES(L)=PVCVES(L)+V
              PVCDIV(L)=PVCDIV(L)+DV
            ELSEIF (NF(I,J,K).GE.1) THEN
              L =IPVC(I,J,K)
              V =XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
              PVCVES(L)=PVCVES(L)+V*(1.0D0-FF(I,J,K))
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE
      IF (NPROCS.NE.1) THEN
        DO 330 L=1,NPVCB
          DV=PVCDIV(L)
          CALL VF_P1SUMD(DV,PVCDIV(L))
          V =PVCVES(L)
          CALL VF_P1SUMD(V ,PVCVES(L))
 330    CONTINUE
      ENDIF
      DO 340 L=1,NPVCB
        PVCDIV(L)=PVCDIV(L)/PVCVES(L)
        PVCVES(L)=0.0D0
 340  CONTINUE

CD    -- 圧力の平均値を計算する(大気圧分を引いた値) --
      DO 420 K=2,NUMK-1
        DO 410 J=MYJS,MYJE
          DO 400 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.8) THEN
              L=IPVC(I,J,K)
              IF (PPPVC(I,J,K).NE.1.0D30) THEN
                V=XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
                PVCPES(L)=PVCPES(L)+V*PPPVC(I,J,K)
                PVCVES(L)=PVCVES(L)+V
              ELSE
                V=XX(2,I)*YY(2,J)*ZZ(2,K)*GGV(I,J,K)
                PVCPFS(L)=PVCPFS(L)+V*PP(I,J,K)
                PVCVFS(L)=PVCVFS(L)+V
              ENDIF
            ENDIF
 400      CONTINUE
 410    CONTINUE
 420  CONTINUE
      IF (NPROCS.NE.1) THEN
        DO 430 L=1,NPVCB
          V=PVCPES(L)
          CALL VF_P1SUMD(V,PVCPES(L))
          V=PVCVES(L)
          CALL VF_P1SUMD(V,PVCVES(L))
          V=PVCPFS(L)
          CALL VF_P1SUMD(V,PVCPFS(L))
          V=PVCVFS(L)
          CALL VF_P1SUMD(V,PVCVFS(L))
 430    CONTINUE
      ENDIF
      DO 440 L=1,NPVCB
        IF (IPVCBC(L).NE.0) THEN
          PVCPES(L)=0.0D0
        ELSE
          IF (PVCVES(L).GT.0.0D0) THEN
            PAV=PVCPES(L)/PVCVES(L)
          ELSE
            PAV=PVCPFS(L)/PVCVFS(L)
          ENDIF
          PVCPES(L)=(PAV+PVCP0)/(1.0D0+PVCGM*DTNOW*PVCDIV(L))-PVCP0
        ENDIF
 440  CONTINUE

CD    -- 圧力を設定する --
      DO 520 K=2,NUMK-1
        DO 510 J=1,NUMJ
          DO 500 I=1,NUMI
            L=IPVC(I,J,K)
            IF (L.GT.0) THEN
              PPPVC(I,J,K)=PVCPES(L)
              IF (NF(I,J,K).EQ.8) PP(I,J,K)=PPPVC(I,J,K)
            ELSE 
              PPPVC(I,J,K)=1.0D30
            ENDIF
 500      CONTINUE
 510    CONTINUE
 520  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
