      SUBROUTINE SOLVER_OFFLINESD(INDP,INDU,INDV,INDW,INDP_ML,INDU_ML,
     $                            INDV_ML,INDW_ML,KF,KG,KF_ML,KG_ML,
     $                            KH,KP,MX_ML,MY_ML,MZ_ML,LLWALL,
     $                            LLWALP,LLWALB,I_ML,J_ML,K_ML,
     $                            I_NS,J_NS,K_NS,IEAS,IWES,JNOR,JSOU,
     $                            KBOT,KTOP,IEAS_ML,IWES_ML,
     $                            JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML,IBUF,
     $                            SHLSD,USSD,WEXSD,EXSDD,EXSDE,
     $                            QBX,QBY,CSEDI,CSEDIN,CSDAVE,
     $                            ZBED,ZBEDN,ZBED0,
     $                            CSD_ML,ZBD_ML,CSDBCN,ZBDBCN,
     $                            UU,VV,WW,HU,HV,HW,FF,HH,HX,
     $                            HDEP,HDEP_ML,HHBCN,
     $                            GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,
     $                            GX_ML,GY_ML,GZ_ML,XC,XCP,
     $                            YC,YCOS,YCOSP,ZC,XC_REF,YC_REF,
     $                            XC_ML,YC_ML,ZC_ML,AMNG,TMU,
     $                            WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $                            VLEND,TMEND,FREND,HHOFL,LLOFL,BUF,
     $                            GXBDH,GYBDH,KIBDH,KJBDH)
C======================================================================
C     土砂移動オフライン計算の時間積分処理に関するメインルーチン
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'MATRIX.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'mpif.h'
      INCLUDE 'DRIFT.h'
      INCLUDE 'AIRI.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML
      INTEGER,INTENT(INOUT)::
     $   IEAS_ML,IWES_ML,JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML
      INTEGER,INTENT(INOUT)::IEAS,IWES,JNOR,JSOU,KBOT,KTOP
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),XCP(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY)
      REAL(8),INTENT(IN)::XC_REF(8,MX),YC_REF(8,MY)
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML)
      REAL(8),INTENT(INOUT)::
     $   GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ),
     $   GV0(MX,MY,MZ),GX0(MX,MY,MZ),GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   GY_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   GZ_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1)
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::AMNG(MX,MY)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDV_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDW_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP),
     $                       LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY)
C
      REAL(8),INTENT(INOUT)::VLEND(MX,MY,16),TMEND(MX,MY,6)
      REAL(8),INTENT(INOUT)::FREND(MX,MY,2+NFRAGL)
C
      REAL(8),INTENT(INOUT)::WRK1(MX,MY,MZ),WRK2(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK3(MX,MY,MZ),WRK4(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK5(MX,MY,MZ),WRK6(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK7(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::HHOFL(MLOFL)
      INTEGER,INTENT(INOUT)::LLOFL(3,MLOFL)
C
      REAL(8),INTENT(INOUT)::CSEDI(MX,MY,MZ),CSEDIN(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::CSDAVE(MX,MY)
      REAL(8),INTENT(INOUT)::WEXSD(MX,MY)
      REAL(8),INTENT(INOUT)::SHLSD(MX,MY),USSD(MX,MY)
      REAL(8),INTENT(INOUT)::EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(INOUT)::ZBED(MX,MY),ZBEDN(MX,MY),ZBED0(MX,MY)
      REAL(8),INTENT(INOUT)::QBX(MX,MY),QBY(MX,MY)
      REAL(8),INTENT(INOUT)::
     $   CSD_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $          KBOT_ML-1:KTOP_ML+1),
     $   ZBD_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::CSDBCN(NXY,MZ,4),ZBDBCN(MX,MY)
C
      INTEGER,INTENT(INOUT)::
     $   KF_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1),
     $   KG_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      INTEGER,INTENT(INOUT)::IBUF(*)
      REAL(8),INTENT(INOUT)::
     $   HDEP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::BUF(*)
      REAL(8),INTENT(INOUT)::HHBCN(MX,MY)
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML)
      INTEGER,INTENT(INOUT)::I_NS(2,MX),J_NS(2,MY),K_NS(2,MZ)
      REAL(8),INTENT(INOUT)::GXBDH(MX,MY),GYBDH(MX,MY)
      INTEGER,INTENT(INOUT)::KIBDH(MX,MY),KJBDH(MX,MY)
C
      INTEGER::ITRACE=1
C
      REAL(8)::DT0
      INTEGER::I,J,K
      INTEGER::ISTEP1,IFLAG
      INTEGER::ICHILD,IPARNT
      INTEGER::IERR,IAD,JAD,KAD
C
      INTEGER::MXFL,MYFL,MZFL
      REAL(8)::TMFL1,TMFL2
      REAL(4)::HHFL1(MX,MY),HHFL2(MX,MY)
      REAL(4)::UUFL1(MX,MY,MZ),UUFL2(MX,MY,MZ)
      REAL(4)::VVFL1(MX,MY,MZ),VVFL2(MX,MY,MZ)
      REAL(4)::WWFL1(MX,MY,MZ),WWFL2(MX,MY,MZ)
      REAL(8)::CIP
      INTEGER::IDUM2D(MX,MY)
      REAL(8)::DUM2D(MX,MY)
      REAL(8)::DUM2D_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8)::DUM2DBCN(MX,MY)
      REAL(8)::DUM3D(MX,MY,MZ)
      REAL(8)::DUM3D_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $                  KBOT_ML-1:KTOP_ML+1)
      REAL(8)::DUM3DBCN(NXY,MZ,4)
      INTEGER::IDUM3DA(MX,MY,MZA)
      REAL(8)::DUM3DA(MX,MY,MZA)
      real(8)::ZBEDwrk(MX,MY)
C
C ... ダミー変数をゼロクリア
      IDUM2D=0
      DUM2D=0.0D0
      DUM2D_ML=0.0D0
      DUM2DBCN=0.0D0
      DUM3D=0.0D0
      DUM3D_ML=0.0D0
      DUM3DBCN=0.0D0
      IDUM3DA=0
      DUM3DA=0.0D0
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
C
C ... オフライン土砂移動計算用ファイルをオープン
      call flnam('.osd')
      OPEN(IFLSD,FILE=trim(CFLNM),STATUS='OLD',
     $     FORM='UNFORMATTED',ERR=999)
      WRITE(LP,*) 'OPEN OFFLINE-SD-CALC FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLSD
C ... 解析領域サイズを確認
      READ(IFLSD) MXFL,MYFL,MZFL
      IF(MXFL.NE.MX .OR. MYFL.NE.MY .OR. MZFL.NE.MZ)THEN
        CALL ERRMSG('SOLVER_OFFLINESD',7200)
        WRITE(LP,*) 'INPUT ERROR: OFFLINE-SD-CALC FILE'
        WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
        WRITE(LP,*) 'FILE NUMBER=',IFLSD
        WRITE(LP,*) 'MX,MY,MZ=',MXFL,MYFL,MZFL
        CALL ABORT1('')
      ENDIF
C ... まず最初の2時刻分のデータを読み込む
      READ(IFLSD) TMFL1
      READ(IFLSD) ((HHFL1(I,J),I=1,MX),J=1,MY)
      READ(IFLSD) (((UUFL1(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
      READ(IFLSD) (((VVFL1(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
      READ(IFLSD) (((WWFL1(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
      READ(IFLSD) TMFL2
      READ(IFLSD) ((HHFL2(I,J),I=1,MX),J=1,MY)
      READ(IFLSD) (((UUFL2(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
      READ(IFLSD) (((VVFL2(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
      READ(IFLSD) (((WWFL2(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
C ... オフライン計算はファイルに含まれる時間の範囲内で行う
      IF(TIME.LT.TMFL1) TIME=TMFL1
C
C ... ファイルのオープンと初期分布の出力
      CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,
     $            DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,
     $            FF,HH,
     $            SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $            EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $            INDP,KF,KP,KG,DUM2D,DUM2D,DUM2D,VLEND,TMEND,FREND,
     $            HDEP,DUM2DBCN,DUM3DBCN,DUM3DBCN,IDUM2D,DUM2D,
     $            DUM3DA,DUM3DA,DUM3DA,DUM3DA,DUM3DA,DUM3DA,
     $            DUM3DA,DUM3DA,DUM3DA,IDUM3DA,0)
C
      CALL ZERCLR(TMU,MXYZ,0.0D0)
C
C
C
C######################################################################
C#                                                                    #
C#       時間積分ループの始まり                                       #
C#                                                                    #
C######################################################################
      WRITE(LP,*) '+--------------------------+'
      WRITE(LP,*) '|  START TIME INTEGRATION  |'
      WRITE(LP,*) '+--------------------------+'
      ISTEP1 = ISTEP+1

      DO 100 ISTEP=ISTEP1,MAXSTP
C
C----------------------------------------------------------------------
C     (1) 時間刻みの設定
C----------------------------------------------------------------------
         CALL SETDT(XC,YC,ZC,GV,GX,GY,GZ,UU,VV,WW,TMU,
     $              INDP,INDU,INDV,INDW,IAD,JAD,KAD)
C
         IF(NSIZE.GT.1) THEN
           DT0 = DT
           CALL MPI_ALLREDUCE( DT0,DT,1,MPI_DOUBLE_PRECISION,MPI_MIN,
     $                         comm_model,IERR )
         END IF
C
         DTV = DT
C
C----------------------------------------------------------------------
C     (2) 新しい時刻の水位および流速を設定する
C----------------------------------------------------------------------
        TIME = TIME+DTV
C
C ..... 前ステップの水位情報
        KH=KF
        HX=HH
C
C ..... 必要に応じてファイルを読み込む
        DO
           IF(TIME.LT.TMFL2) EXIT
           TMFL1=TMFL2
           HHFL1=HHFL2
           UUFL1=UUFL2
           VVFL1=VVFL2
           WWFL1=WWFL2
           READ(IFLSD,END=300) TMFL2
           READ(IFLSD) ((HHFL2(I,J),I=1,MX),J=1,MY)
           READ(IFLSD) (((UUFL2(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
           READ(IFLSD) (((VVFL2(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
           READ(IFLSD) (((WWFL2(I,J,K),I=1,MX),J=1,MY),K=2,MZM)
        ENDDO
C
C ..... 補間して現在のHH,UU,VV,WWを求める
        CIP=(TIME-TMFL1)/(TMFL2-TMFL1)
        DO J=1,MY
        DO I=1,MX
          HH(I,J)=HHFL1(I,J)+(HHFL2(I,J)-HHFL1(I,J))*CIP
          DO K=2,MZM
            UU(I,J,K)=UUFL1(I,J,K)+(UUFL2(I,J,K)-UUFL1(I,J,K))*CIP
            VV(I,J,K)=VVFL1(I,J,K)+(VVFL2(I,J,K)-VVFL1(I,J,K))*CIP
            WW(I,J,K)=WWFL1(I,J,K)+(WWFL2(I,J,K)-WWFL1(I,J,K))*CIP
          ENDDO
        ENDDO
        ENDDO
        GOTO 400
C
  300   CONTINUE
C
C ..... ファイルの最終時刻分のデータで計算する
        DT=TMFL1-(TIME-DTV)
        DTV=DT
        TIME=TMFL1
        REND=TMFL1
        HH=HHFL1
        UU=UUFL1
        VV=VVFL1
        WW=WWFL1
C
  400   CONTINUE
C
C ..... FF,KF,KPを求める
        CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
C
C ..... FF,HH
        CALL CP_DSR_DC2(MX,MY,MZ,0,1,FF)
        CALL CP_DSR_FFF(FF)
C
C
C----------------------------------------------------------------------
C     (3) ネスティング処理
C----------------------------------------------------------------------
C
        IFLAG = 1
C
C ... 親領域から子領域へ
C
        IF(ICHILD.GE.0) THEN
          CALL CP_SNDML2NS(KF,KG,IBUF,
     1                     DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,
     $                     DUM2D,HDEP,CSEDI,ZBEDwrk,BUF,MX,MY,MZ,
     3                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                     NESML(2),NESML(3),NESML(1),NESML(4),IFLAG)
        END IF
C
        IF(IPARNT.GE.0.OR.IPFLG.NE.0) THEN
          CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,DUM3D_ML,DUM3D_ML,DUM3D_ML,
     1                     DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM2D_ML,
     $                     HDEP_ML,CSD_ML,ZBD_ML,BUF,MX_ML,MY_ML,MZ_ML,
     3                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     $                     KBOT_ML,KTOP_ML,NOVRLP(2),NOVRLP(3),
     4                     NOVRLP(1),NOVRLP(4),IFLAG)
          IF(IFLAG.EQ.8) IFLAG=1
C
          CALL CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                    INDU,INDV,INDP,XC_ML,YC_ML,ZC_ML,XC_REF,
     2                    YC_REF,ZC,GX_ML,GX,GY_ML,GY,I_ML,J_ML,K_ML,
     3                    I_NS,J_NS,K_NS,KF_ML,KG_ML,KF,KG,
     5                    DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM3D_ML,
     3                    DUM3D_ML,DUM3D_ML,
     *                    DUM2D_ML,HDEP_ML,HDEP,CSD_ML,ZBD_ML, 
     6                    DUM2DBCN,DUM3DBCN,DUM3DBCN,
     *                    DUM3DBCN,DUM3DBCN,DUM3DBCN,
     *                    DUM3DBCN,DUM3DBCN,CSDBCN,ZBDBCN,
     8                    MX_ML,MY_ML,MZ_ML,MX,MY,MZ,IEAS_ML,IWES_ML,
     9                    JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,IFLAG)
        END IF
C
C ... 子領域から親領域へ
C
        IF(IPARNT.GE.0) THEN
          CALL CP_BCNS2ML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                    INDU,INDV,INDW,INDP,XC_ML,YC_ML,ZC_ML,
     2                    XC_REF,YC_REF,ZC,GX_ML,GY_ML,GZ_ML,GX,GY,GZ,
     4                    GV,I_ML,J_ML,K_ML,KF_ML,KG_ML,I_NS,J_NS,KF,KG,
     5                    DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM2D,HDEP,
     6                    DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM3D_ML,
     $                    DUM2D_ML,HDEP_ML,CSEDI,ZBEDwrk,CSD_ML,ZBD_ML,
     $                    DUM3D,DUM3D,DUM3D_ML,DUM3D_ML,
     7                    MX_ML,MY_ML,MZ_ML,MX,MY,MZ,IEAS_ML,IWES_ML,
     8                    JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,IFLAG)
C
          CALL CP_SNDNS2ML(DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM3D_ML,
     1                     DUM3D_ML,DUM3D_ML,DUM3D_ML,DUM2D_ML,
     2                     CSD_ML,ZBD_ML,BUF,MX_ML,MY_ML,MZ_ML,
     3                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     4                     KBOT_ML,KTOP_ML,NOVRLP(2),NOVRLP(3),
     5                     NOVRLP(1),NOVRLP(4),IFLAG)
        END IF
C
        IF(ICHILD.GE.0) THEN
          CALL CP_RCVNS2ML(DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,
     1                     DUM2D,CSEDI,ZBEDwrk,BUF,
     2                     MX,MY,MZ,IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                     NESML(2),NESML(3),NESML(1),NESML(4),IFLAG)
        END IF
C
C
C----------------------------------------------------------------------
C     (4) 線流量を計算
C----------------------------------------------------------------------
C ... 水平方向の線流量を計算
        CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,DUM2D,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,1)
C ... 鉛直方向の線流量を計算
        CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,DUM2D,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,2)
C
C
C----------------------------------------------------------------------
C     (5) 乱流粘性係数の計算
C----------------------------------------------------------------------
        IF(LTURB.EQ.1) THEN
          CALL CLTMU(WRK1,WRK2,UU,VV,WW,WRK1,WRK2,WRK3,XC,YC,ZC,
     $               INDU,INDV,INDW,INDP,KF,KP,KG,TMU)
        END IF
C
C ..... CSEDI,ZBED,TMUについての領域分割の通信
        IFLAG=3
        CALL CP_NEIBCOM(DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,
     $                  CSEDI,ZBED,WRK1,WRK2,
     $                  TMU,DUM2D,DUM2D,DUM2D,DUM2D,DUM2D,KF,KP,IFLAG)
C
C
C----------------------------------------------------------------------
C     (6) 地形変化モデルの計算
C----------------------------------------------------------------------
        IF(LSEDI.EQ.1) THEN
C ....... シールズ数の計算
          CALL CLSHL(SHLSD,USSD,UU,VV,HU,HV,XC,YC,ZC,GV0,HH,HDEP,AMNG,
     $               KF,KG,GXBDH,GYBDH,KIBDH,KJBDH)
C
C ....... 交換砂量の計算
          CALL CLWEX(WEXSD,EXSDE,EXSDD,SHLSD,USSD,CSEDI,ZBED,
     $               ZC,GV,HH,HX,HDEP,INDP,KF,KH,KG)
C
C ....... 浮遊砂濃度の計算
          CALL CLSEDI(CSEDI,CSEDIN,CSDAVE,WEXSD,EXSDE,EXSDD,ZBED,
     $                HU,HV,HW,TMU,TMU,TMU,XC,YC,ZC,XCP,YCOS,YCOSP,
     $                GV,GX,GY,GZ,HH,HX,HDEP,INDP,INDU,INDV,INDW,
     $                LLWALL,LLWALP,KF,KH,KG,KP,CSDBCN,
     $                WRK1,WRK2,WRK3,WRK4,WRK5)
          if(itrace.ne.0) write(6,*) 'clsedi,istep,time=',istep,time
C
C ....... 掃流砂高さの計算
          CALL CLBED(ZBED,ZBEDN,QBX,QBY,SHLSD,WEXSD,UU,VV,HU,HV,
     $               XC,YC,ZC,HH,HDEP,KF,KG,GX,GY,INDU,INDV,
     $               MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     $               I_ML,J_ML,I_NS,J_NS,XC_ML,YC_ML,XC,YC,
     $               KF_ML,KG_ML,KF,KG,XC_REF,YC_REF,
     $               IEAS,IWES,JSOU,JNOR,
     $               IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,NESML,BUF,
     $               GXBDH,GYBDH,KIBDH,KJBDH)
          if(itrace.ne.0) write(6,*) 'clbed,istep,time=',istep,time
        END IF
C
C
C----------------------------------------------------------------------
C     (7) 終了判定
C----------------------------------------------------------------------
C
        IF( TIME .GE. REND ) GO TO 200
C
C
C----------------------------------------------------------------------
C     (8) 結果の出力
C----------------------------------------------------------------------
        CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,
     $              DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,
     $              FF,HH,
     $              SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $              EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $              INDP,KF,KP,KG,DUM2D,DUM2D,DUM2D,VLEND,TMEND,FREND,
     $              HDEP,DUM2DBCN,DUM3DBCN,DUM3DBCN,IDUM2D,DUM2D,
     $              DUM3DA,DUM3DA,DUM3DA,DUM3DA,DUM3DA,DUM3DA,
     $              DUM3DA,DUM3DA,DUM3DA,IDUM3DA,1)
C
C
  100 CONTINUE
C######################################################################
C#                                                                    #
C#       時間積分ループの終わり                                       #
C#                                                                    #
C######################################################################
  200 CONTINUE
      CALL FTIMER(30,1)
C
C
C----------------------------------------------------------------------
C     (9) 最終結果の出力とファイルのクローズ
C----------------------------------------------------------------------
      CALL FTIMER(60,0)
      IF(RFILE(3).GT.0.0D0) CLOSE(IFLBO,STATUS='KEEP')
      CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,
     $            DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,DUM3D,
     $            FF,HH,
     $            SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $            EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $            INDP,KF,KP,KG,DUM2D,DUM2D,DUM2D,VLEND,TMEND,FREND,
     $            HDEP,DUM2DBCN,DUM3DBCN,DUM3DBCN,IDUM2D,DUM2D,
     $            DUM3DA,DUM3DA,DUM3DA,DUM3DA,DUM3DA,DUM3DA,
     $            DUM3DA,DUM3DA,DUM3DA,IDUM3DA,2)
      CALL FTIMER(60,1)
C
      RETURN
C
C
C ... ファイルオープンエラー
  999 CONTINUE
      CALL ERRMSG('SOLVER_OFFLINESD',7201)
      WRITE(LP,*) 'FILE OPEN ERROR: OFFLINE-SD-CALC FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLSD
      CALL ABORT1('')
C
      END
