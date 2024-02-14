      SUBROUTINE VF_BWKELG(XX,YY,ZZ,UU,VV,WW,BCVI,AK,AE,DBUF,
     &                     NF,INDX,INDY,INDZ,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWKELG:対数則境界面に接するセル中心の乱流量を設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    BCVI(NUMB)       : IN  : R*8 : 流速の境界条件(壁面の粗さ)
CD    AK(@FOR-3D@)     : I/O : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : I/O : R*8 : 乱流エネルギ散逸
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),BCVI(NUMB)
      DIMENSION AK  (NUMI,NUMJ,NUMK),AE  (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 定数の計算 --
      CMR=1.0D0/SQRT(AKCMU)
      CKR=1.0D0/AKK0

CD    -- 乱流量を設定する --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.0) THEN
              LXM=INDX(I  ,J,K)
              LXP=INDX(I+1,J,K)
              LYM=INDY(I,J  ,K)
              LYP=INDY(I,J+1,K)
              LZM=INDZ(I,J,K  )
              LZP=INDZ(I,J,K+1)
              IXM=0
              IXP=0
              IYM=0
              IYP=0
              IZM=0
              IZP=0
              IF (LXM.GE.1) IXM=INDB(3,LXM)
              IF (LXP.GE.1) IXP=INDB(3,LXP)
              IF (LYM.GE.1) IYM=INDB(3,LYM)
              IF (LYP.GE.1) IYP=INDB(3,LYP)
              IF (LZM.GE.1) IZM=INDB(3,LZM)
              IF (LZP.GE.1) IZP=INDB(3,LZP)
              IF (IXM.EQ.6 .OR. IXP.EQ.6 .OR. 
     &            IYM.EQ.6 .OR. IYP.EQ.6 .OR. 
     &            IZM.EQ.6 .OR. IZP.EQ.6 .OR. 
     &            IXM.EQ.8 .OR. IXP.EQ.8 .OR. 
     &            IYM.EQ.8 .OR. IYP.EQ.8 .OR. 
     &            IZM.EQ.8 .OR. IZP.EQ.8     ) THEN
                UC=(UU(I,J,K)+UU(I+1,J,K))*0.5D0
                VC=(VV(I,J,K)+VV(I,J+1,K))*0.5D0
                WC=(WW(I,J,K)+WW(I,J,K+1))*0.5D0
                VX=SQRT(VC*VC+WC*WC)
                VY=SQRT(UC*UC+WC*WC)
                VZ=SQRT(UC*UC+VC*VC)
                DX=XX(2,I)*0.5D0
                DY=YY(2,J)*0.5D0
                DZ=ZZ(2,K)*0.5D0
                SX=0.0D0
                UT=0.0D0
                IF (IXM.EQ.6 .OR. IXP.EQ.6) THEN
                  IF (VX.GE.ZERO .AND. ANU0.GE.ZERO)
     &                                      CALL VF_CLOGLW(DX,VX,UT)
                  SX=1.0D0
                  IF (IXM.EQ.6 .AND. IXP.EQ.6) SX=2.0D0
                ENDIF
                SY=0.0D0
                VT=0.0D0
                IF (IYM.EQ.6 .OR. IYP.EQ.6) THEN
                  IF (VY.GE.ZERO .AND. ANU0.GE.ZERO)
     &                                      CALL VF_CLOGLW(DY,VY,VT)
                  SY=1.0D0
                  IF (IYM.EQ.6 .AND. IYP.EQ.6) SY=2.0D0
                ENDIF
                SZ=0.0D0
                WT=0.0D0
                IF (IZM.EQ.6 .OR. IZP.EQ.6) THEN
                  IF (VZ.GE.ZERO .AND. ANU0.GE.ZERO)
     &                                      CALL VF_CLOGLW(DZ,VZ,WT)
                  SZ=1.0D0
                  IF (IYM.EQ.6 .AND. IYP.EQ.6) SZ=2.0D0
                ENDIF
                SXM=0.0D0
                UTM=0.0D0
                IF (IXM.EQ.8) THEN
                  IF (VX.GE.ZERO) CALL VF_CLOGKS(DX,VX,BCVI(LXM),UTM)
                  SXM=1.0D0
                ENDIF
                SXP=0.0D0
                UTP=0.0D0
                IF (IXP.EQ.8) THEN
                  IF (VX.GE.ZERO) CALL VF_CLOGKS(DX,VX,BCVI(LXP),UTP)
                  SXP=1.0D0
                ENDIF
                SYM=0.0D0
                VTM=0.0D0
                IF (IYM.EQ.8) THEN
                  IF (VY.GE.ZERO) CALL VF_CLOGKS(DY,VY,BCVI(LYM),VTM)
                  SYM=1.0D0
                ENDIF
                SYP=0.0D0
                VTP=0.0D0
                IF (IYP.EQ.8) THEN
                  IF (VY.GE.ZERO) CALL VF_CLOGKS(DY,VY,BCVI(LYP),VTP)
                  SYP=1.0D0
                ENDIF
                SZM=0.0D0
                WTM=0.0D0
                IF (IZM.EQ.8) THEN
                  IF (VZ.GE.ZERO) CALL VF_CLOGKS(DZ,VZ,BCVI(LZM),WTM)
                  SZM=1.0D0
                ENDIF
                SZP=0.0D0
                WTP=0.0D0
                IF (IZP.EQ.8) THEN
                  IF (VZ.GE.ZERO) CALL VF_CLOGKS(DZ,VZ,BCVI(LZP),WTP)
                  SZP=1.0D0
                ENDIF
                SR=1.0D0/(SX+SY+SZ+SXM+SXP+SYM+SYP+SZM+SZP)
                AK(I,J,K)=CMR*( UT *UT        *SX
     &                         +VT *VT        *SY
     &                         +WT *WT        *SZ
     &                         +UTM*UTM       *SXM
     &                         +UTP*UTP       *SXP
     &                         +VTM*VTM       *SYM
     &                         +VTP*VTP       *SYP
     &                         +WTM*WTM       *SZM
     &                         +WTP*WTP       *SZP)*SR
                AE(I,J,K)=CKR*( UT *UT *UT /DX*SX
     &                         +VT *VT *VT /DY*SY
     &                         +WT *WT *WT /DZ*SZ
     &                         +UTM*UTM*UTM/DX*SXM
     &                         +UTP*UTP*UTP/DX*SXP
     &                         +VTM*VTM*VTM/DY*SYM
     &                         +VTP*VTP*VTP/DY*SYP
     &                         +WTM*WTM*WTM/DZ*SZM
     &                         +WTP*WTP*WTP/DZ*SZP)*SR
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD2(AK,DBUF,0)
      CALL VF_P3SRD2(AE,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
