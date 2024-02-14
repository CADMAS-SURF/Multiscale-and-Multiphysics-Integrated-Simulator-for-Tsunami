      SUBROUTINE SETZB(DZBUF,ZBED,ZBEDN,HDEP,XC,YC,ZC,GV0,GX0,GY0,
     $                 GV,GX,GY,GVD,GXD,GYD,HU,HV,HW,UU,VV,WW,FF,
     $                 GZ,GZ0,YCOSP,HH,CSEDI,GXBDH,GYBDH,BUF,
     $                 GX_ML,GY_ML,GZ_ML,HDEP_ML,HH_ML,WRK1,WRK2,WRK3,
     $                 INDP,INDU,INDV,INDW,KF,KP,KG,
     $                 KIBDH,KJBDH,IBUF,MX_ML,MY_ML,MZ_ML,
     $                 IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
     $                 IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     $                 INDU_ML,INDV_ML,INDW_ML,INDP_ML,NC)
C======================================================================
C     地形変化量を反映
C======================================================================
      use mod_list,only: DEALLOC_LIST,ALLOC_LIST,ALLOC_LIST2,
     $                   COUNT_MLWAL,LLWALL,LLWALP,LLWALB,LLOFL,HHOFL
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'FILE.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'SEDIMENT.h'
      include 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::DZBUF(MX,MY)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::GV0(MX,MY,MZ),GX0(MX,MY,MZ),GY0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(IN)::ZBED(MX,MY),ZBEDN(MX,MY)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::GVD(MX,MY,MZ),GXD(MX,MY,MZ),GYD(MX,MY,MZ)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GZ0(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(IN)::YCOSP(MY)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::CSEDI(MX,MY,MZ)
      REAL(8),INTENT(IN)::GXBDH(MX,MY),GYBDH(MX,MY)
      INTEGER,INTENT(IN)::KIBDH(MX,MY),KJBDH(MX,MY)
      INTEGER,INTENT(IN)::KF(MX,MY),KP(MX,MY)
      INTEGER,INTENT(INOUT)::KG(MX,MY)
      INTEGER,INTENT(OUT)::NC
C
      INTEGER::IBUF(*)
      REAL(8):: BUF(*)
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML
      INTEGER,INTENT(IN)::
     $   IEAS_ML,IWES_ML,JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML
      INTEGER,INTENT(IN)::IEAS,IWES,JNOR,JSOU,KBOT,KTOP
      INTEGER::
     $   INDU_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDV_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDW_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1)
      REAL(8)::
     $   GX_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   GY_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   GZ_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   HDEP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1),
     $   HH_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8)::WRK1(MX,MY,MZ),WRK2(MX,MY,MZ),WRK3(MX,MY,MZ)
C
C ... Local変数
      INTEGER::KGOLD(MX,MY)
      REAL(8)::HDEPOLD(MX,MY),BF1(MX,MY,MZ),BF2(MX,MY)
      REAL(8),PARAMETER:: EPSZ=0.0D0, GVMIN1=0.05D0, GVMIN2=0.1D-4
      REAL(8)::HDEPT,HDEP0,GVMIN
      REAL(8)::CVOL1,CVOL2,CNEW,DH1,DH2,DHNEW
      INTEGER::I,J,K,L,M,N,NCOUNT,NCOUNT2,NCOUNTP,NCOUNTP1,NCOUNTK,IERR
      INTEGER:: I1,I2,J1,J2,K1,K2,MLWALX,MLWALY
      LOGICAL:: LINOUT,LLEFT,LRIGHT
C
      INTEGER:: ICHILD,IPARNT,MLWALL_OLD,MLWALP_OLD,MLWALB_OLD,MLOFL_OLD
      INTEGER::IDUMMY(3,1)
      REAL(8)::RDUMMY(3)
C
C
C ... KG,HDEPのバックアップ
      KGOLD  =KG
      HDEPOLD=HDEP
      GVMIN=GVMIN1
      IF(MZ.EQ.3) GVMIN=GVMIN2
C
C
      CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
      CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $            GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $            INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,1)
C
C----------------------------------------------------------------------
C     1. 地形変化量をバッファ変数DZBUFに格納(Δzチルダの計算)
C----------------------------------------------------------------------
      DO J=2,MYM
      DO I=2,MXM
         DZBUF(I,J) = DZBUF(I,J) + ( ZBED(I,J)-ZBEDN(I,J) )
      ENDDO
      ENDDO
C
C
C ... 地形変化セルの数のカウンター変数の初期化(NCOUNTPは領域分割で通信の必要あるセル数)
      NCOUNT=0
      NCOUNT2=0
      NCOUNTP=0
      NCOUNTK=0
      MLWALX=0
      MLWALY=0
C
C
C----------------------------------------------------------------------
C     2. 海底インデックスKG,水深HDEP,有効体積率GV0,セルインデックスINDPの更新
C----------------------------------------------------------------------
      DO J=2,MYM
      DO I=2,MXM
         IF( ABS(DZBUF(I,J)).GT.EPSZ ) THEN
            NCOUNT=NCOUNT+1
            IF( I.EQ.2.OR.I.EQ.MXM.OR.J.EQ.2.OR.J.EQ.MYM )
     $      NCOUNTP=NCOUNTP+1
C
            HDEP0=HDEP(I,J)
            HDEPT=HDEP(I,J)+DZBUF(I,J)
            K=KG(I,J)
C
C           (1) 海底セルが上に移動
C
            IF( HDEPT.GE.ZC(1,K).AND.K.LT.MZM ) THEN
               HDEP(I,J)=MIN(HDEPT,ZC(1,K+1)-GVMIN*ZC(4,K+1))
               GV0(I,J,K)=1.0D0
               GV0(I,J,K+1)=(ZC(1,K+1)-HDEP(I,J))*ZC(6,K+1)
               GV(I,J,K)=GV0(I,J,K)*GVD(I,J,K)
               GV(I,J,K+1)=GV0(I,J,K+1)*GVD(I,J,K+1)
               INDP(I,J,K)=0 ! 構造セルに変化
               INDW(I,J,K-1)=-4
               INDW(I,J,K)=-2
               KG(I,J)=K+1
               NCOUNTK=NCOUNTK+1
C
C              濃度の更新(浮遊砂量を保存させるように平均化)
               DH1  =MAX(MIN(ZC(1,K+1),HH(I,J)),ZC(1,K))-ZC(1,K)
               DH2  =MIN(ZC(1,K+1),HH(I,J))-HDEP0
               CVOL1=CSEDI(I,J,K  )*DH1
               CVOL2=CSEDI(I,J,K+1)*DH2
               DHNEW=MAX(MIN(ZC(1,K+1),HH(I,J)),HDEP(I,J))-HDEP(I,J)
               CNEW =(CVOL1+CVOL2)/MAX(DHNEW,ZLIMSD)
               CSEDI(I,J,K  )=0.0D0
               CSEDI(I,J,K+1)=MIN(CNEW,CMAXSD)
C
C              流速の更新
               UU(I-1,J,K)=0.0D0
               UU(I  ,J,K)=0.0D0
               VV(I,J-1,K)=0.0D0
               VV(I,J  ,K)=0.0D0
               WW(I,J,K)  =0.0D0
C
C           (2) 海底セルが下に移動
C
            ELSEIF( HDEPT.LE.ZC(1,K-1)-GVMIN*ZC(4,K-1).AND.K.GT.2 ) THEN
               HDEP(I,J)=MAX(HDEPT,ZC(1,K-2))
               GV0(I,J,K)=1.0D0
               GV0(I,J,K-1)=(ZC(1,K-1)-HDEP(I,J))*ZC(6,K-1)
               GV(I,J,K)=GV0(I,J,K)*GVD(I,J,K)
               GV(I,J,K-1)=GV0(I,J,K-1)*GVD(I,J,K-1)
               INDP(I,J,K-1)=1 ! 流体セルに変化
               IF( K.GT.3 ) INDW(I,J,K-3)=-4
               INDW(I,J,K-2)=-2
               INDW(I,J,K-1)=1
               KG(I,J)=K-1
               NCOUNTK=NCOUNTK+1
C
C              濃度の更新(浮遊砂量を保存させるように平均化)
               DH1  =MIN(ZC(1,K),HH(I,J))-HDEP0
               CVOL1=CSEDI(I,J,K  )*DH1
               DHNEW=MIN(ZC(1,K),HH(I,J))-HDEP(I,J)
               CNEW =CVOL1/MAX(DHNEW,ZLIMSD)
               CSEDI(I,J,K  )=MIN(CNEW,CMAXSD)
               CSEDI(I,J,K-1)=MIN(CNEW,CMAXSD)
C
C              流速の更新
               UU(I-1,J,K-1)=0.0D0
               UU(I  ,J,K-1)=0.0D0
               VV(I,J-1,K-1)=0.0D0
               VV(I,J  ,K-1)=0.0D0
               WW(I,J,K-1)  =0.0D0
C
C           (3) 海底セルが移動しない
C
            ELSE
               HDEP(I,J)=MIN(MAX(HDEPT,ZC(1,K-1)),ZC(1,K)-GVMIN*ZC(4,K))
               GV0(I,J,K)=(ZC(1,K)-HDEP(I,J))*ZC(6,K)
               GV(I,J,K)=GV0(I,J,K)*GVD(I,J,K)
C
C              濃度の更新(浮遊砂量を保存させるように平均化)
               DH1  =MIN(ZC(1,K),HH(I,J))-HDEP0
               CVOL1=CSEDI(I,J,K)*DH1
               DHNEW=MAX(MIN(ZC(1,K),HH(I,J)),HDEP(I,J))-HDEP(I,J)
               CNEW =CVOL1/MAX(DHNEW,ZLIMSD)
               CSEDI(I,J,K)=MIN(CNEW,CMAXSD)
C
            ENDIF
C
            DZBUF(I,J)=HDEPT-HDEP(I,J)
         ENDIF
      ENDDO
      ENDDO
C
      IF( NPROC.GT.1 ) THEN
      NCOUNTP1=NCOUNTP
      CALL MPI_ALLREDUCE(NCOUNTP1,NCOUNTP,1,MPI_INTEGER,MPI_SUM,
     &        CHILDCOMM,IERR )
C
      NCOUNTP1=NCOUNT
      CALL MPI_ALLREDUCE(NCOUNTP1,NCOUNT2,1,MPI_INTEGER,MPI_MAX,
     &        CHILDCOMM,IERR )
      ENDIF
      NC=NCOUNT
C
C
C ... 領域分割計算のための通信1
      IF( NCOUNTP.GT.0 ) THEN
         N=1
         L=0
         CALL CP_DSR_DC2(MX,MY,1,L,N,HDEP)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,GV0)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,GV)
         BF1=DBLE(INDP)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
         INDP=NINT(BF1)
         BF2=DBLE(KG)
         CALL CP_DSR_DC2(MX,MY,1,L,N,BF2)
         KG=NINT(BF2)
         L=3
         BF1=DBLE(INDW)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
         INDW=NINT(BF1)
      ENDIF
C
C
C----------------------------------------------------------------------
C     3(a). インデックスINDU,面透過率GX0の更新
C----------------------------------------------------------------------
      IF( NCOUNT.GT.0 ) THEN
c         write(*,*) 'kg-in:',kg(1,2),kg(2,2)
c         write(*,*) 'kg-ou:',kg(101,2),kg(102,2)
c         write(*,*) 'indp-in:',indp(1,2,5),indp(2,2,5)
c         write(*,*) 'indp-ou:',indp(101,2,5),indp(102,2,5)
      DO J=2,MYM
      DO I=1,MXM
         IF( HDEPOLD(I  ,J).EQ.HDEP(I  ,J) .AND.
     $       HDEPOLD(I+1,J).EQ.HDEP(I+1,J) ) CYCLE
C
         K1=MIN(KG(I,J),KGOLD(I,J),KG(I+1,J),KGOLD(I+1,J))
         K2=MAX(KG(I,J),KGOLD(I,J),KG(I+1,J),KGOLD(I+1,J))
C
         DO K=K1,K2
C           流入出境界かどうか
            LINOUT=INDU(I,J,K).EQ.-1 .OR. INDU(I,J,K).EQ.0
            LLEFT =INDP(I,J,K).EQ.1
            LRIGHT=INDP(I+1,J,K).EQ.1
C
C ......... 流入出境界または両側が流体計算セルの場合
            IF(LINOUT.OR.(LLEFT.AND.LRIGHT) )THEN
               I1=I
               I2=I+1
C
C              流入出境界の場合IとI+1から内部の計算セルを選択
               IF( LINOUT.AND.LLEFT  ) I2=I1
               IF( LINOUT.AND.LRIGHT ) I1=I2
C
C              流入出境界だが流体セルに隣接しない場合 ... 何もしない
               IF( .NOT.(LLEFT.OR.LRIGHT) ) THEN
C              NOTHING TO DO
C
C              構造データの設定がない or 構造物データの天端よりも上 ... 流速計算点
               ELSEIF( K.GT.KIBDH(I,J) ) THEN
                  IF(.NOT.LINOUT) INDU(I,J,K)=1
                  GX0(I,J,K)=XC(7,I,J)*GV0(I1,J,K)+XC(8,I,J)*GV0(I2,J,K)
                  GX(I,J,K)=GX0(I,J,K)*GXD(I,J,K)
C
C              構造データの天端を含む ... 流速計算点(天端補正あり)
               ELSEIF( K.EQ.KIBDH(I,J) ) THEN
                  IF(.NOT.LINOUT) INDU(I,J,K)=1
                  GX0(I,J,K)=MIN(GXBDH(I,J),
     $                      XC(7,I,J)*GV0(I1,J,K)+XC(8,I,J)*GV0(I2,J,K))
                  GX(I,J,K)=GX0(I,J,K)*GXD(I,J,K)
C
C              構造データの天端よりも下 ... 板境界
               ELSE
                  IF(.NOT.LINOUT) INDU(I,J,K)=-3
                  GX0(I,J,K)=1.0D0
                  GX(I,J,K)=GX0(I,J,K)*GXD(I,J,K)
               ENDIF
C
C ......... 片側のみ構造物 ... 壁面
            ELSE IF( LLEFT.OR.LRIGHT ) THEN
               INDU(I,J,K)=-2
               GX0(I,J,K)=1.0D0
               GX(I,J,K)=GX0(I,J,K)*GXD(I,J,K)
C
C ......... 両側とも構造物 ... 構造物の内点
            ELSE
               INDU(I,J,K)=-4
               GX0(I,J,K)=1.0D0
               GX(I,J,K)=GX0(I,J,K)*GXD(I,J,K)
C                 
            ENDIF
         ENDDO
      ENDDO
      ENDDO
      ENDIF
C
C
C----------------------------------------------------------------------
C     3(b). インデックスINDV,面透過率GY0の更新
C----------------------------------------------------------------------
      IF( NCOUNT.GT.0 ) THEN
      DO J=1,MYM
      DO I=2,MXM
         IF( HDEPOLD(I,J  ).EQ.HDEP(I,J  ) .AND.
     $       HDEPOLD(I,J+1).EQ.HDEP(I,J+1) ) CYCLE
C
         K1=MIN(KG(I,J),KGOLD(I,J),KG(I,J+1),KGOLD(I,J+1))
         K2=MAX(KG(I,J),KGOLD(I,J),KG(I,J+1),KGOLD(I,J+1))
C
         DO K=K1,K2
C           流入出境界かどうか
            LINOUT=INDV(I,J,K).EQ.-1 .OR. INDV(I,J,K).EQ.0
            LLEFT =INDP(I,J,K).EQ.1
            LRIGHT=INDP(I,J+1,K).EQ.1
C
C ......... 流入出境界または両側が流体計算セルの場合
            IF(LINOUT.OR.(LLEFT.AND.LRIGHT) )THEN
               J1=J
               J2=J+1
C
C              流入出境界の場合IとJ+1から内部の計算セルを選択
               IF( LINOUT.AND.LLEFT  ) J2=J1
               IF( LINOUT.AND.LRIGHT ) J1=J2
C
C              流入出境界だが流体セルに隣接しない場合 ... 何もしない
               IF( .NOT.(LLEFT.OR.LRIGHT) ) THEN
C              NOTHING TO DO
C
C              構造データの設定がない or 構造物データの天端よりも上 ... 流速計算点
               ELSEIF( K.GT.KJBDH(I,J) ) THEN
                  IF(.NOT.LINOUT) INDV(I,J,K)=1
                  GY0(I,J,K)=YC(7,J)*GV0(I,J1,K)+YC(8,J)*GV0(I,J2,K)
                  GY(I,J,K)=GY0(I,J,K)*GYD(I,J,K)
C
C              構造データの天端を含む ... 流速計算点(天端補正あり)
               ELSEIF( K.EQ.KJBDH(I,J) ) THEN
                  IF(.NOT.LINOUT) INDV(I,J,K)=1
                  GY0(I,J,K)=MIN(GYBDH(I,J),
     $                      YC(7,J)*GV0(I,J1,K)+YC(8,J)*GV0(I,J2,K))
                  GY(I,J,K)=GY0(I,J,K)*GYD(I,J,K)
C
C              構造データの天端よりも下 ... 板境界
               ELSE
                  IF(.NOT.LINOUT) INDV(I,J,K)=-3
                  GY0(I,J,K)=1.0D0
                  GY(I,J,K)=GY0(I,J,K)*GYD(I,J,K)
               ENDIF
C
C ......... 片側のみ構造物 ... 壁面
            ELSE IF( LLEFT.OR.LRIGHT ) THEN
               INDV(I,J,K)=-2
               GY0(I,J,K)=1.0D0
               GY(I,J,K)=GY0(I,J,K)*GYD(I,J,K)

C ......... 両側とも構造物 ... 構造物の内点
            ELSE
               INDV(I,J,K)=-4
               GY0(I,J,K)=1.0D0
               GY(I,J,K)=GY0(I,J,K)*GYD(I,J,K)
C                 
            ENDIF
         ENDDO
      ENDDO
      ENDDO
      ENDIF
C
C
C
C ... 領域分割計算のための通信2
      IF( NCOUNTP.GT.0 ) THEN
         L=1
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,GX0)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,GX)
C
         L=2
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,GY0)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,GY)
C
         L=1
         BF1=DBLE(INDU)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
         INDU=NINT(BF1)
C
         L=2
         BF1=DBLE(INDV)
         CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
         INDV=NINT(BF1)
      ENDIF
C
C
C----------------------------------------------------------------------
C     4. リストベクトルの更新
C----------------------------------------------------------------------
      MLWALL_OLD=MLWALL
      MLWALP_OLD=MLWALP
      MLWALB_OLD=MLWALB
      MLOFL_OLD =MLOFL
C
      IF( NCOUNTK.GT.0 ) THEN
C ... 配列を一旦DEALLOCATEして再度ALLOCATEして作り直す
C
      CALL DEALLOC_LIST(0)
C
      CALL COUNT_MLWAL(MLWALL,MLWALL1,MLWALP,MX,MY,MZ,
     $                 INDU,INDV,INDW,LSTOCDS)
C
      CALL ALLOC_LIST(MLWALL,MLWALP,LP)
C
      CALL MKIND2(INDP,INDU,INDV,INDW,WRK1,WRK2,WRK3,
     $            LLWALL,LLWALP,1)
C
      ENDIF
C
      IF( NCOUNT.GT.0 ) THEN
C
      CALL DEALLOC_LIST(1)
C
      CALL MKIND4(GV0,GX0,GY0,XC,YC,ZC,HDEP,RDUMMY,
     $            INDP,INDU,INDV,IDUMMY,IDUMMY,0,1)
C
      CALL ALLOC_LIST2(MLWALB,MLOFL,LP)
C
C ... 防潮堤用リストベクトルLLWALBを設定する
      CALL MKIND4(GV0,GX0,GY0,XC,YC,ZC,HDEP,HHOFL,
     $            INDP,INDU,INDV,LLWALB,LLOFL,1,1)
C
      ENDIF
C
C ... メッセージ出力
      if( MLWALL_OLD.NE.MLWALL ) write(6,85) 'MLWALL',MLWALL_OLD,MLWALL
      if( MLWALP_OLD.NE.MLWALP ) write(6,85) 'MLWALP',MLWALP_OLD,MLWALP
      if( MLWALB_OLD.NE.MLWALB ) write(6,85) 'MLWALB',MLWALB_OLD,MLWALB
      if( MLOFL_OLD .NE.MLOFL  ) write(6,85) 'MLOFL ',MLOFL_OLD ,MLOFL
   85 format(a6,' changed:',I6,' -> ',I6)
C
C
C
      CALL CLHUVWR(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $             GZ0,XC,YC,ZC,YCOSP,HH,HDEP,
     $             INDU,INDV,INDW,LLWALB,KF,KG)
C
C----------------------------------------------------------------------
C     5. 線流量と流体積率の更新
C----------------------------------------------------------------------
      IF( NCOUNT.GT.0 ) THEN
         DO J=2,MYM
         DO I=2,MXM
            IF( HH(I,J).LT.HDEP(I,J) ) THEN
               HH(I,J)=HDEP(I,J)+EPSH
            ENDIF
         ENDDO
         ENDDO
C
         CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
      ENDIF
C
C
C
C----------------------------------------------------------------------
C     6. ネスティング境界のインデックスとポロシティの更新
C----------------------------------------------------------------------
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
C
C     INDU,INDV,INDW,GX,GY,GZを子に送信する。
      IF(ICHILD.GE.0) THEN
         CALL CP_SNDIND(INDU,INDV,INDW,INDP,IBUF,
     $                  GX0,GY0,GZ0,GV0,BUF,
     $                  MX,MY,MZ,
     $                  IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     $                  NESML(2),NESML(3),NESML(1),NESML(4))
         CALL CP_SNDDEP(HDEP,HH,BUF,MX,MY,
     $                  IEAS,IWES,JSOU,JNOR,
     $                  NESML(2),NESML(3),NESML(1),NESML(4),
     $                  ICHILD)
      ENDIF
C
C     INDU,INDV,INDW,GX,GY,GZを親から受信する。
      IF(IPARNT.GE.0) THEN
         CALL CP_RCVIND(INDU_ML,INDV_ML,INDW_ML,INDP_ML,IBUF,
     $                  GX_ML,GY_ML,GZ_ML,WRK1,BUF,
     $                  MX_ML,MY_ML,MZ_ML,
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
     $                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4))
         CALL CP_RCVDEP(HDEP_ML,HH_ML,BUF,MX_ML,MY_ML,
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     $                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     $                  IPARNT)
      ENDIF
C
C
      RETURN
      END
