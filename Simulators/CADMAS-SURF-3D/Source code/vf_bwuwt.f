      SUBROUTINE VF_BWUWT(XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWUWT:境界面の一部の接線方向流速を設定する
CD      (1)接線方向流速は流速を指定していない以下の条件のとき設定
CD      (2)スリップ、フリー、対数則、完全粗面 --

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APHYSI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    ANU(@FOR-3D@)    : IN  : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCVI(NUMB)       : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU(NUMI,NUMJ,NUMK),VV (NUMI,NUMJ,NUMK)
      DIMENSION WW(NUMI,NUMJ,NUMK),ANU(NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCVI(NUMB)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 境界面の流速を設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        IB =INDB(3,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

CD      -- スリップおよびフリー --
        IF     (IB.EQ.1 .OR. IB.EQ.4) THEN
          IF     (NS.EQ.1) THEN
            BCV(L)=(VV(I  ,J  ,K  )+VV(I  ,J+1,K  ))*0.5D0
            BCW(L)=(WW(I  ,J  ,K  )+WW(I  ,J  ,K+1))*0.5D0
          ELSEIF (NS.EQ.2) THEN
            BCV(L)=(VV(I-1,J  ,K  )+VV(I-1,J+1,K  ))*0.5D0
            BCW(L)=(WW(I-1,J  ,K  )+WW(I-1,J  ,K+1))*0.5D0
          ELSEIF (NS.EQ.3) THEN
            BCU(L)=(UU(I  ,J  ,K  )+UU(I+1,J  ,K  ))*0.5D0
            BCW(L)=(WW(I  ,J  ,K  )+WW(I  ,J  ,K+1))*0.5D0
          ELSEIF (NS.EQ.4) THEN
            BCU(L)=(UU(I  ,J-1,K  )+UU(I+1,J-1,K  ))*0.5D0
            BCW(L)=(WW(I  ,J-1,K  )+WW(I  ,J-1,K+1))*0.5D0
          ELSEIF (NS.EQ.5) THEN
            BCU(L)=(UU(I  ,J  ,K  )+UU(I+1,J  ,K  ))*0.5D0
            BCV(L)=(VV(I  ,J  ,K  )+VV(I  ,J+1,K  ))*0.5D0
          ELSEIF (NS.EQ.6) THEN
            BCU(L)=(UU(I  ,J  ,K-1)+UU(I+1,J  ,K-1))*0.5D0
            BCV(L)=(VV(I  ,J  ,K-1)+VV(I  ,J+1,K-1))*0.5D0
          ENDIF

CD      -- ノンスリップ --
        ELSEIF (IB.EQ.2) THEN

CD      -- 流速固定 --
        ELSEIF (IB.EQ.3) THEN

CD      -- 造波境界 --
        ELSEIF (IB.EQ.5) THEN
C         * 水位固定のみの処理
          IF     (NS.EQ.1) THEN
            IF (I.EQ.2) THEN
              IF ((IBCTYP(2,1).EQ.-3 .AND. MTBTYP .EQ.3) .OR.
     &            (IBCTYP(2,1).EQ.-4 .AND. MTBTYP2.EQ.3) ) THEN
                BCV(L)=(VV(I  ,J  ,K  )+VV(I  ,J+1,K  ))*0.5D0
                BCW(L)=(WW(I  ,J  ,K  )+WW(I  ,J  ,K+1))*0.5D0
              ENDIF
            ENDIF
          ELSEIF (NS.EQ.2) THEN
            IF (I.EQ.NUMI) THEN
              IF ((IBCTYP(2,2).EQ.-3 .AND. MTBTYP .EQ.3) .OR.
     &            (IBCTYP(2,2).EQ.-4 .AND. MTBTYP2.EQ.3) ) THEN
                BCV(L)=(VV(I-1,J  ,K  )+VV(I-1,J+1,K  ))*0.5D0
                BCW(L)=(WW(I-1,J  ,K  )+WW(I-1,J  ,K+1))*0.5D0
              ENDIF
            ENDIF
          ELSEIF (NS.EQ.3) THEN
            IF (J.EQ.2) THEN
              IF ((IBCTYP(2,3).EQ.-3 .AND. MTBTYP .EQ.3) .OR.
     &            (IBCTYP(2,3).EQ.-4 .AND. MTBTYP2.EQ.3) ) THEN
                BCU(L)=(UU(I  ,J  ,K  )+UU(I+1,J  ,K  ))*0.5D0
                BCW(L)=(WW(I  ,J  ,K  )+WW(I  ,J  ,K+1))*0.5D0
              ENDIF
            ENDIF
          ELSEIF (NS.EQ.4) THEN
            IF (J.EQ.NUMJ) THEN
              IF ((IBCTYP(2,4).EQ.-3 .AND. MTBTYP .EQ.3) .OR.
     &            (IBCTYP(2,4).EQ.-4 .AND. MTBTYP2.EQ.3) ) THEN
                BCU(L)=(UU(I  ,J-1,K  )+UU(I+1,J-1,K  ))*0.5D0
                BCW(L)=(WW(I  ,J-1,K  )+WW(I  ,J-1,K+1))*0.5D0
              ENDIF
            ENDIF
          ELSE
            CALL VF_A2ERR('VF_BWUWT','P.G ERROR.')
          ENDIF

CD      -- 対数則および完全粗面 --
        ELSEIF (IB.EQ.6 .OR. IB.EQ.8) THEN
          IF     (NS.EQ.1) THEN
            V1=(VV(I  ,J  ,K  )+VV(I  ,J+1,K  ))*0.5D0
            V2=(WW(I  ,J  ,K  )+WW(I  ,J  ,K+1))*0.5D0
            DL=XX(2,I  )*0.5D0
            AN=ANU(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            V1=(VV(I-1,J  ,K  )+VV(I-1,J+1,K  ))*0.5D0
            V2=(WW(I-1,J  ,K  )+WW(I-1,J  ,K+1))*0.5D0
            DL=XX(2,I-1)*0.5D0
            AN=ANU(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            V1=(UU(I  ,J  ,K  )+UU(I+1,J  ,K  ))*0.5D0
            V2=(WW(I  ,J  ,K  )+WW(I  ,J  ,K+1))*0.5D0
            DL=YY(2,J  )*0.5D0
            AN=ANU(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            V1=(UU(I  ,J-1,K  )+UU(I+1,J-1,K  ))*0.5D0
            V2=(WW(I  ,J-1,K  )+WW(I  ,J-1,K+1))*0.5D0
            DL=YY(2,J-1)*0.5D0
            AN=ANU(I,J-1,K)
          ELSEIF (NS.EQ.5) THEN
            V1=(UU(I  ,J  ,K  )+UU(I+1,J  ,K  ))*0.5D0
            V2=(VV(I  ,J  ,K  )+VV(I  ,J+1,K  ))*0.5D0
            DL=ZZ(2,K  )*0.5D0
            AN=ANU(I,J,K  )
          ELSEIF (NS.EQ.6) THEN
            V1=(UU(I  ,J  ,K-1)+UU(I+1,J  ,K-1))*0.5D0
            V2=(VV(I  ,J  ,K-1)+VV(I  ,J+1,K-1))*0.5D0
            DL=ZZ(2,K-1)*0.5D0
            AN=ANU(I,J,K-1)
          ENDIF
          VA=SQRT(V1*V1+V2*V2)
          IF (VA.LT.ZERO .OR. AN.LT.ZERO) THEN
            V1=0.0D0
            V2=0.0D0
            DL=1.0D0
            AN=1.0D0
            VA=1.0D0
            VT=1.0D0
          ELSEIF (IB.EQ.6) THEN
            CALL VF_CLOGLW(DL,VA,VT)
          ELSE
            CALL VF_CLOGKS(DL,VA,BCVI(L),VT)
          ENDIF
          VA=DL/AN*VT*VT/VA
          V1=V1-VA*V1
          V2=V2-VA*V2
          IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
            BCV(L)=V1
            BCW(L)=V2
          ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
            BCU(L)=V1
            BCW(L)=V2
          ELSE
            BCU(L)=V1
            BCV(L)=V2
          ENDIF

CD      -- 放射境界 --
        ELSEIF (IB.EQ.7) THEN

CD      -- プログラムエラー --
        ELSE
          CALL VF_A2ERR('VF_BWUWT','P.G ERROR.')
        ENDIF
 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
