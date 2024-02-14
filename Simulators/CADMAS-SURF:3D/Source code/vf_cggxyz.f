      SUBROUTINE VF_CGGXYZ(ISW,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                     NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_CGGXYZ:時間依存型の空隙率をスワップし、透過率を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISW            : IN  : I*4 : スイッチ
CD                                 = 0:スワップしない
CD                                 !=0:スワップする
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    GGV(@FOR-3D@)  : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)  : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)  : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)  : I/O : R*8 : z方向面積透過率
CD    GGV0(@FOR-3D@) : I/O : R*8 : 空隙率
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@) : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@) : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@) : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK),DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 時間依存型の空隙率をスワップする --
      IF (ISW.NE.0) THEN
        DO 120 K=1,NUMK
          DO 110 J=1,NUMJ
            DO 100 I=1,NUMI
              W          =GGV (I,J,K)
              GGV (I,J,K)=GGV0(I,J,K)
              GGV0(I,J,K)=W
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE
      ENDIF

CD    -- GGXを設定する --
      LA=MYGIS+MYMIS
      LB=MYGIE-MYMIE
      IF (MYMIE.EQ.1) LB=LB+1
      DO 230 IB=1,IPRNB
        DO 220 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 210 J=IPRARA(2,IB),IPRARA(5,IB)
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              JW=J-JP
              DO 200 I=IPRARA(1,IB),IPRARA(4,IB)+1
                IF (LA.LE.I .AND. I.LE.LB) THEN
                  IW=I-IP
                  IF     (INDX(IW,JW,K).EQ.-1) THEN
                    GGX(IW,JW,K)=1.0D0
                  ELSEIF (INDX(IW,JW,K).EQ. 0) THEN
                    GGX(IW,JW,K)=MIN(GGV(IW,JW,K),GGV(IW-1,JW,K))
COLD                GGX(IW,JW,K)=XX(6,IW)* (XX(2,IW-1)*GGV(IW  ,JW,K)
COLD &                                     +XX(2,IW  )*GGV(IW-1,JW,K))
                  ENDIF
                ENDIF
 200          CONTINUE
            ENDIF
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE

CD    -- GGYを設定する --
      LA=MYGJS+MYMJS
      LB=MYGJE-MYMJE
      IF (MYMJE.EQ.1) LB=LB+1
      DO 330 IB=1,IPRNB
        DO 320 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 310 J=IPRARA(2,IB),IPRARA(5,IB)+1
            IF (LA.LE.J .AND. J.LE.LB) THEN
              JW=J-JP
              DO 300 I=IPRARA(1,IB),IPRARA(4,IB)
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IW=I-IP
                  IF     (INDY(IW,JW,K).EQ.-1) THEN
                    GGY(IW,JW,K)=1.0D0
                  ELSEIF (INDY(IW,JW,K).EQ. 0) THEN
                    GGY(IW,JW,K)=MIN(GGV(IW,JW,K),GGV(IW,JW-1,K))
COLD                GGY(IW,JW,K)=YY(6,JW)*( YY(2,JW-1)*GGV(IW,JW  ,K)
COLD &                                     +YY(2,JW  )*GGV(IW,JW-1,K))
                  ENDIF
                ENDIF
 300          CONTINUE
            ENDIF
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE

CD    -- GGZを設定する --
      DO 430 IB=1,IPRNB
        DO 420 K=IPRARA(3,IB),IPRARA(6,IB)+1
          DO 410 J=IPRARA(2,IB),IPRARA(5,IB)
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              JW=J-JP
              DO 400 I=IPRARA(1,IB),IPRARA(4,IB)
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IW=I-IP
                  IF     (INDZ(IW,JW,K).EQ.-1) THEN
                    GGZ(IW,JW,K)=1.0D0
                  ELSEIF (INDZ(IW,JW,K).EQ. 0) THEN
                    GGZ(IW,JW,K)=MIN(GGV(IW,JW,K),GGV(IW,JW,K-1))
COLD                GGZ(IW,JW,K)=ZZ(6,K)*( ZZ(2,K-1)*GGV(IW,JW,K  )
COLD &                                    +ZZ(2,K  )*GGV(IW,JW,K-1))
                  ENDIF
                ENDIF
 400          CONTINUE
            ENDIF
 410      CONTINUE
 420    CONTINUE
 430  CONTINUE

      CALL VF_P3SRD2(GGX,DBUF,1)
      CALL VF_P3SRD2(GGY,DBUF,2)
      CALL VF_P3SRD2(GGZ,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
