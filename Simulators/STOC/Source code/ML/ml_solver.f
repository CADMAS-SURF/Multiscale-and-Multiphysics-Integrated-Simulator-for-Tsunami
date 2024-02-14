      SUBROUTINE SOLVER(XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,
     $                  XC_REF,YC_REF,
     $                  XC_ML,YC_ML,ZC_ML,GV,GX,GY,GZ,
     $                  GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,
     $                  CMD,CDD,COE1D,COE2D,GX_ML,GY_ML,GZ_ML,
     $                  UU,VV,WW,HU,HV,HW,PP,TT,CC,Q2,QL,
     $                  TMU,DKXX,DKXY,DKYY,DKM,DKH,FF,RHOW,HH,
     $                  TMUBW,TIMBW,
     $                  PATM,DPS,QQ,QW,WX,WY,UN,VN,WN,TN,CN,Q2P,QLP,
     $                  CSEDI,CSEDIN,CSDAVE,SHLSD,USSD,WEXSD,
     $                  EXSDE,EXSDD,ZBED,ZBEDN,ZBED0,QBX,QBY,DZBUF,
     $                  CSD_ML,ZBD_ML,CSDBCN,ZBDBCN,
     $                  GXBDH,GYBDH,KIBDH,KJBDH,
     $                  WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,WRK8,VLEND,
     $                  TMEND,FREND,FRIC,HDEP,AMNG,CD,HX,HY,HHW,
     $                  RWWB,RWWF,INDP,INDU,INDV,INDW,
     $                  INDP_ML,INDU_ML,INDV_ML,INDW_ML,
     $                  KF,KG,KP,KH,KF_ML,KG_ML,IBUF,
     $                  UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     $                  HH_ML,HDEP_ML,BUF,
     $                  HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,Q2BCN,QLBCN,
     $                  I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     $                  MX_ML,MY_ML,MZ_ML,
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
     $                  IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     $                  HTDST1,HTDST2,IDST,TMDST,KBLC,
     $                  ZCA,GVA,GXA,GYA,GZA,
     $                  PPA,UUA,VVA,WWA,UNA,VNA,WNA,AKA,EPA,
     $                 TMUA,AKNA,EPNA,FFA,FFNA,GVNA,HUA,HVA,HWA,UUBCAIR,
     $                  VVBCAIR,UUBCAIRB,VVBCAIRB,UUBCAIRF,VVBCAIRF,
     $              AKBCAIR,EPBCAIR,AKBCAIRB,EPBCAIRB,AKBCAIRF,EPBCAIRF,
     $                  AD0A,AL0A,ADA,ALA,AUA,BBA,DPA,
     $                  INDPA,INDUA,INDVA,INDWA,KFA,KFNA,
     $                  FALLWX,FALLWY,FALLWZ,DHX,DHY,CFALLWX,CFALLWY,
     $                  DFALLWNX,DFALLWNY,DFALLWTX,DFALLWTY)
C======================================================================
C     時間積分計算の処理に関するメインルーチン
C
C     (0) ファイルのオープンと初期分布の出力
C
C        (1) 時間刻みの設定
C        (2) 開境界潮位の設定
C        (3) 流速uの計算
C        (4) 流速vの計算
C        (5) 流速wの計算
C        (6) 水面位置の計算
C        (7) エネルギー式の計算
C        (8) 濃度輸送式の計算
C        (9) 圧力の計算
C        (98) 建物破壊(DSのみ)
C        (99) 閉塞処理(DSのみ)
C        (10) 終了判定
C        (11) 結果の出力
C
C     (12) 最終結果の出力とファイルのクローズ
C
C======================================================================
      use mod_comm,only: comm_mlicdsmg2fc
      use mod_list,only: LLWALL,LLWALP,LLWALB,LLOFL,HHOFL
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
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
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'AGENT.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML
      INTEGER,INTENT(INOUT)::
     $   IEAS_ML,IWES_ML,JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML
      INTEGER,INTENT(INOUT)::IEAS,IWES,JNOR,JSOU,KBOT,KTOP
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),XCP(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY),YSIN(MY),YSINP(MY)
      REAL(8),INTENT(IN)::XC_REF(8,MX),YC_REF(8,MY)
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML)
      REAL(8),INTENT(INOUT)::
     $   GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ),
     $   GV0(MX,MY,MZ),GX0(MX,MY,MZ),GY0(MX,MY,MZ),GZ0(MX,MY,MZ),
     $   GVD(MX,MY,MZ),GXD(MX,MY,MZ),GYD(MX,MY,MZ),GZD(MX,MY,MZ),
     $   CMD(MX,MY,MZ),CDD(MX,MY,MZ),COE1D(MX,MY,MZ),COE2D(MX,MY,MZ)
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
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::Q2(MX,MY,MZ),QL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::DKXX(MX,MY,MZ),DKXY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::DKYY(MX,MY,MZ),DKM(MX,MY,MZ),DKH(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),FF(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FRIC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HY(MX,MY),HHW(MX,MY)
      REAL(8),INTENT(INOUT)::QQ(MX,MY,MZ),QW(MX,MY)
      REAL(8),INTENT(INOUT)::TMUBW(MX,MY),TIMBW(MX,MY)
      REAL(8),INTENT(INOUT)::PATM(MX,MY),DPS(MX,MY),WX(MX,MY),WY(MX,MY)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::RWWB(MX,MY,9),RWWF(MX,MY,9)
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
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY)
C
      REAL(8),INTENT(INOUT)::UN(MX,MY,MZ),VN(MX,MY,MZ),WN(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TN(MX,MY,MZ),CN(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::Q2P(MX,MY,MZ),QLP(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::VLEND(MX,MY,16),TMEND(MX,MY,6)
      REAL(8),INTENT(INOUT)::FREND(MX,MY,2+NFRAGL)
C
      REAL(8),INTENT(INOUT)::WRK1(MX,MY,MZ),WRK2(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK3(MX,MY,MZ),WRK4(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK5(MX,MY,MZ),WRK6(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK7(MX,MY,MZ),WRK8(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::CSEDI(MX,MY,MZ),CSEDIN(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::CSDAVE(MX,MY)
      REAL(8),INTENT(INOUT)::WEXSD(MX,MY)
      REAL(8),INTENT(INOUT)::SHLSD(MX,MY),USSD(MX,MY)
      REAL(8),INTENT(INOUT)::EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(INOUT)::ZBED(MX,MY),ZBEDN(MX,MY),ZBED0(MX,MY)
      REAL(8),INTENT(INOUT)::QBX(MX,MY),QBY(MX,MY)
      REAL(8),INTENT(INOUT)::DZBUF(MX,MY)
      REAL(8),INTENT(INOUT)::
     $   CSD_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $          KBOT_ML-1:KTOP_ML+1),
     $   ZBD_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::CSDBCN(NXY,MZ,4),ZBDBCN(MX,MY)
      REAL(8),INTENT(INOUT)::GXBDH(MX,MY),GYBDH(MX,MY)
      INTEGER,INTENT(INOUT)::KIBDH(MX,MY),KJBDH(MX,MY)
C
      REAL(8):: ZCA(8,MZA)
      REAL(8):: GXA(MX,MY,MZA),GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8):: GVA(MX,MY,MZA)
      REAL(8):: PPA(MX,MY,MZA)
      REAL(8):: UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8):: UNA(MX,MY,MZA),VNA(MX,MY,MZA),WNA(MX,MY,MZA)
      REAL(8):: AKA(MX,MY,MZA),EPA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8):: AKNA(MX,MY,MZA),EPNA(MX,MY,MZA)
      REAL(8):: FFA(MX,MY,MZA)
      REAL(8):: FFNA(MX,MY,MZA),GVNA(MX,MY,MZA)
      REAL(8):: HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8):: UUBCAIR(NXY,MZA,4),VVBCAIR(NXY,MZA,4)
      REAL(8):: UUBCAIRB(NXY,MZA,4),VVBCAIRB(NXY,MZA,4)
      REAL(8):: UUBCAIRF(NXY,MZA,4),VVBCAIRF(NXY,MZA,4)
      REAL(8):: AKBCAIR(NXY,MZA,4),EPBCAIR(NXY,MZA,4)
      REAL(8):: AKBCAIRB(NXY,MZA,4),EPBCAIRB(NXY,MZA,4)
      REAL(8):: AKBCAIRF(NXY,MZA,4),EPBCAIRF(NXY,MZA,4)
      REAL(8):: AD0A(MX,MY,MZA),AL0A(3,MX,MY,MZA)
      REAL(8):: ADA(MX,MY,MZA),ALA(3,MX,MY,MZA),AUA(3,MX,MY,MZA)
      REAL(8):: BBA(MX,MY,MZA),DPA(MX,MY,MZA)
      INTEGER:: INDUA(MX,MY,MZA),INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER:: INDPA(MX,MY,MZA)
      INTEGER:: KFA(MX,MY),KFNA(MX,MY)
C
      REAL(8):: FALLWX(MX,MY),FALLWY(MX,MY),FALLWZ(MX,MY)
      REAL(8):: DHX(MX,MY),DHY(MX,MY)
      REAL(8):: CFALLWX(MX,MY),CFALLWY(MX,MY)
      REAL(8):: DFALLWNX(MX,MY),DFALLWNY(MX,MY)
      REAL(8):: DFALLWTX(MX,MY),DFALLWTY(MX,MY)
C
      INTEGER,INTENT(INOUT)::
     $   KF_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1),
     $   KG_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      INTEGER,INTENT(INOUT)::IBUF(*)
      REAL(8),INTENT(INOUT)::
     $   UU_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   VV_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   WW_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   TT_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   CC_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   Q2_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   QL_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1)
      REAL(8),INTENT(INOUT)::
     $   HH_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::
     $   HDEP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::BUF(*)
      REAL(8),INTENT(INOUT)::HHBCN(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4),
     $   WWBCN(NXY,MZ,4),TTBCN(NXY,MZ,4),CCBCN(NXY,MZ,4),
     $   Q2BCN(NXY,MZ,4),QLBCN(NXY,MZ,4)
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML)
      INTEGER,INTENT(INOUT)::I_NS(2,MX),J_NS(2,MY),K_NS(2,MZ)
C
      INTEGER::ITRACE=1
      INTEGER::IEND(2),IEND1(2),IEE
C
      INTEGER::IFINISH
C
      REAL(8)::DT0
      INTEGER::I,IAD,ICHILD,IERR,IFLAG,IPARNT,ISTEP1
      INTEGER::J,JAD,K,KAD,N3D,N2D,NC
      REAL(8),ALLOCATABLE::BF2(:,:)
! ... 流速の前回値
      REAL(8),ALLOCATABLE::HUOMZ(:,:),HVOMZ(:,:)
C
      REAL(8),INTENT(INOUT)::HTDST1(MX,MY),HTDST2(MX,MY)
      INTEGER,INTENT(INOUT)::IDST(MX,MY)
      REAL(8),INTENT(INOUT)::TMDST(MX,MY)
      INTEGER,INTENT(INOUT)::KBLC(MX,MY)
C
C ... 補間処理用局所変数
      REAL(8),ALLOCATABLE::UB_ML(:,:,:),VB_ML(:,:,:),WB_ML(:,:,:),
     $                     TB_ML(:,:,:),CB_ML(:,:,:)
      REAL(8),ALLOCATABLE::HB_ML(:,:)
      REAL(8),ALLOCATABLE::UF_ML(:,:,:),VF_ML(:,:,:),WF_ML(:,:,:),
     $                     TF_ML(:,:,:),CF_ML(:,:,:)
      REAL(8),ALLOCATABLE::HF_ML(:,:)
      real(8),ALLOCATABLE::ZBEDwrk(:,:)
      INTEGER::ITRMTX1,ITRMTXAIR
      REAL(8)::RNRMTX1,RNRMTXAIR
      INTEGER:: ISZ,JSZ,KSZ
c
c add 20130423(s)
c      INTEGER,PARAMETER :: INTERVAL=10
c      CHARACTER(5) :: cfile
c      real(8) :: hhnow,hhnxt,uu1
c      INCLUDE 'BOUNDR.h'
c add 20130423(e)
C
      ISZ=(IEAS_ML+1)-(IWES_ML-1)+1
      JSZ=(JNOR_ML+1)-(JSOU_ML-1)+1
      KSZ=(KTOP_ML+1)-(KBOT_ML-1)+1
      ALLOCATE(BF2(MX,MY),ZBEDwrk(MX,MY),HUOMZ(MX,MY),HVOMZ(MX,MY),
     $         UB_ML(ISZ,JSZ,KSZ),VB_ML(ISZ,JSZ,KSZ),WB_ML(ISZ,JSZ,KSZ),
     $         TB_ML(ISZ,JSZ,KSZ),CB_ML(ISZ,JSZ,KSZ),HB_ML(ISZ,JSZ),
     $         UF_ML(ISZ,JSZ,KSZ),VF_ML(ISZ,JSZ,KSZ),WF_ML(ISZ,JSZ,KSZ),
     $         TF_ML(ISZ,JSZ,KSZ),CF_ML(ISZ,JSZ,KSZ),HF_ML(ISZ,JSZ),
     $         STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('ML_SOLVER',6090)
        WRITE(LP,*) 'CANNOT ALLOCATE BF2,...'
        CALL ABORT1('')
      END IF
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
      CALL CP_HHML2NS(HH,HDEP,HHBCN,HHW,KF,XC_REF,YC_REF,I_ML,J_ML,
     $                MX_ML,MY_ML,IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,0)
      if(itrace.ne.0) then
        IF(ICHILD.GE.0) WRITE(6,601) IWES,IEAS,JSOU,JNOR,KBOT,KTOP
        IF(IPARNT.GE.0) WRITE(6,602) IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,
     $                               KBOT_ML,KTOP_ML
 601  FORMAT('IWES,IEAS,JSOU,JNOR,KBOT,KTOP                  =',6I5)
 602  FORMAT('IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML=',6I5)
      end if
C
      TIME   = RSTART
C
C ... 境界条件ファイルの初期設定
      IF(IPFLG.NE.0) THEN
        N2D = (IEAS_ML-IWES_ML+3)*(JNOR_ML-JSOU_ML+3)
        N3D = N2D*(KTOP_ML-KBOT_ML+3)
        CALL ZERCLR(UF_ML,N3D,0.0D0)
        CALL ZERCLR(VF_ML,N3D,0.0D0)
        CALL ZERCLR(WF_ML,N3D,0.0D0)
        IF(LTEMP.EQ.1) CALL ZERCLR(TF_ML,N3D,0.0D0)
        IF(LCONC.EQ.1) CALL ZERCLR(CF_ML,N3D,0.0D0)
        CALL ZERCLR(HF_ML,N2D,0.0D0)
  320   CONTINUE
        IPFLG = 1
        DO 300 I=1,2
          K = I
          CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,
     1                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     $                     HH_ML,HDEP_ML,CSD_ML,ZBD_ML,BUF,
     2                     MX_ML,MY_ML,MZ_ML,
     3                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     $                                                     KTOP_ML,
     4                     NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),K)
          IF(K.NE.I) THEN
             CALL ERRMSG('ML_SOLVER',6091)
             WRITE(LP,*) 'ERROR : READING BCI FILE I,K=',I,K
             CALL ABORT1('')
          ENDIF
          CALL CP_INTMLVAL(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,
     1                     UF_ML,VF_ML,WF_ML,TF_ML,CF_ML,HF_ML,
     2                     UB_ML,VB_ML,WB_ML,TB_ML,CB_ML,HB_ML,
     3                     KF_ML,ZC_ML,MX_ML,MY_ML,MZ_ML,
     4                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     5                                             KTOP_ML,K)
          IF(I.EQ.2.AND.TIMVF.GT.TIME) GO TO 310
  300   CONTINUE
        GO TO 320
  310   CONTINUE
        WRITE(LP,*) '### TIMVF,TIMVB =',TIMVF,TIMVB
        WRITE(06,*) '@@@ TIMVF,TIMVB =',TIMVF,TIMVB
      END IF
C
      CALL SEABOT(HU,HV,UU,VV,FF,HH,HDEP,GX,GY,XC,YC,ZC,YCOSP,
     $            INDU,INDV,INDP,KF,KP,KG)
C
C ... 初期の境界条件を設定
C
      CALL FTIMER(33,0)
      CALL SETTBL
      CALL FTIMER(33,1)
      CALL BCINLF(FF,ZC,INDP,HH,HDEP,HHBCN,-1)
      CALL BCINLV(UU,VV,WW,FF,GX0,GY0,INDU,INDV,INDW,XC,YC,ZC,
     $            UUBCN,VVBCN,MZ_ML,INDU_ML,INDV_ML,
     $            I_NS,J_NS,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $            JNOR_ML,KBOT_ML,KTOP_ML,-1)
      CALL CLDENS(RHOW,TT,CC,GV,ZC,INDP,KF)
      CALL BCSURF(PP,UU,VV,WW,HH,TT,QQ,QW,PATM,DPS,WX,WY,CD,
     $            XC,YC,ZC,YCOSP,GV,GX,GY,GZ,HDEP,RWWB,RWWF,
     $            INDU,INDV,INDP,KF,KP,KG,KH,0)
C
C     乱流粘性係数を計算
      IF(LTURB.EQ.1) THEN
        CALL CLTMU(WRK1,WRK2,UU,VV,WW,WRK1,WRK2,WRK3,XC,YC,ZC,
     $             INDU,INDV,INDW,INDP,KF,KP,KG,TMU)
        DKXX = TMU
        DKXY = TMU
        DKYY = TMU
        DKM  = TMU
        DKH  = TMU
      ELSEIF(LTURB.EQ.3) THEN
C
        CALL ZERCLR(WRK1,MXYZ,0.0D0)
        CALL ZERCLR(WRK2,MXYZ,0.0D0)
        DO 350 K=1,MZ
        DO 350 J=1,MY
        DO 350 I=1,MX
          IF(INDP(I,J,K).GT.0) THEN
            IF(K.LE.KF(I,J)) THEN
              WRK1(I,J,K) = 0.5D0*QL(I,J,K)/Q2(I,J,K)
              WRK2(I,J,K) = SQRT(2.0D0*Q2(I,J,K))
            END IF
          END IF
  350   CONTINUE
C       WRK1=RL,WRK2=RQ
        CALL CLKMKH(WRK2,WRK1,UU,VV,WW,RHOW,XC,YC,ZC,XCP,INDU,INDV,INDW,
     $              INDP,KF,KP,KG,DKXX,DKYY,DKXY,DKM,DKH)
      END IF
C ... 全変数の通信
      IFLAG = 0
      CALL FTIMER(70,0)
      CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     $                WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
      CALL FTIMER(70,1)
C
C     ファイルのオープンと初期分布の出力
      CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,PP,RHOW,
     $            TT,CC,Q2,QL,DKM,FF,HH,
     $            SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $            EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $            INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $            HDEP,HHBCN,UUBCN,VVBCN,IDST,TMDST,
     $            UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,0)
C
      IF(LAIR.EQ.1) THEN
C     風場のポアソン方程式の係数を作成
      CALL MKCOE1_AIR(AD0A,AL0A,XC,YC,ZCA,XCP,INDPA,INDUA,INDVA,INDWA)
C
      CALL CLTMU_AIR(TMUA,AKA,EPA,UUA,VVA,WWA,XC,YC,ZCA,
     $               INDUA,INDVA,INDWA,INDPA,WRK1,WRK2,WRK3)
      ENDIF
C
      CALL FTIMER(28,1)
C
C ... オフライン土砂移動計算用ファイルをオープン
      IF(LSEDI.EQ.0 .AND. MOFFLNSD.EQ.1) CALL OUTOFFLNSD(HH,UU,VV,WW,0)
C
      CALL FTIMER(30,0)
C
C<<<<< (START) STOC-DM VERSION  <<<<<<<
      IF( NB_SD.GE.0 )THEN
      CALL COM_DRIFT1 (XC,YC,ZC)       !  領域情報・計算条件の通信
      CALL COM_DRIFT2 (HDEP,HH,UU,VV,IDST,KBLC)
C                                      !  物理量(HT,HH,UU,VV)の通信
      ENDIF
      IF( NB_SD_MAIN.EQ.1 )THEN
      CALL COM_DRIFT3(TIME,REND)       !  時刻の通信:<Area01>
      ENDIF
      IFINISH = 0
      CALL COM_DRIFT4(IFINISH)         !  漂流計算との同期:<全領域>
C<<<<<  (END)  STOC-DM VERSION  <<<<<<<
C
C ... マルチエージェントモデルへのデータの送信
      CALL COM_MA1(XC,YC,ZC,HDEP)
      CALL COM_MA2(HH,HDEP,HU,HV,0)
C
      IEND1(:)=0
      CALL MPI_ALLREDUCE(IEND1,IEND,2,MPI_INTEGER,MPI_MAX,
     $                   comm_mlicdsmg2fc,IERR)
C
C
C
C######################################################################
C#                                                                    #
C#       時間積分ループの始まり                                       #
C#                                                                    #
C######################################################################
      WRITE(LP,*) '+--------------------------------------+'
      WRITE(LP,*) '|  START TIME INTEGRATION (Leap frog)  |'
      WRITE(LP,*) '+--------------------------------------+'
      ISTEP1 = ISTEP+1

C=================================================FOR OIL_PARTICLE START
      CALL OIL_COMM_STEP
C=================================================FOR OIL_PARTICLE END
      DO 100 ISTEP=ISTEP1,MAXSTP
C
C----------------------------------------------------------------------
C     (1) 時間刻みの設定
C----------------------------------------------------------------------
         CALL FTIMER(31,0)
         CALL SETDT(XC,YC,ZC,GV,GX,GY,GZ,UU,VV,WW,TMU,
     $              INDP,INDU,INDV,INDW,IAD,JAD,KAD)
C
        DT0 = DT
        CALL MPI_ALLREDUCE( DT0,DT,1,MPI_DOUBLE_PRECISION,MPI_MIN,
     $                      comm_mlicdsmg2fc,IERR )
C
         DTV = DT
         CALL FTIMER(31,1)
C
C----------------------------------------------------------------------
C     (2) 境界水位と流速の境界条件を設定
C----------------------------------------------------------------------
C ..... Leapfrog
C       TIME = TIME+0.5D0*DTV
        TIME = TIME+1.5D0*DTV
C
        CALL FTIMER(32,0)
ckt ---
        if(nsomer(1).ne.3.and.nsomer(2).ne.3.and.
     *     nsomer(3).ne.3.and.nsomer(4).ne.3) then
        CALL BCTIDE(PP,UU,VV,WW,RHOW,FF,HH,PATM,ZC,KF,KP,KG,
     $              INDU,INDV,INDW)
        endif
ckt ---
        CALL BCINLF(FF,ZC,INDP,HH,HDEP,HHBCN,0)
        CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
        CALL FTIMER(32,1)
C
C ..... FF,HH
        CALL FTIMER(78,0)
        CALL CP_DSR_DC2(MX,MY,MZ,0,1,FF)
        CALL CP_DSR_FFF(FF)
        CALL CP_DSR_DC2(MX,MY,1,0,1,HH)
        CALL FTIMER(78,1)
C
C----------------------------------------------------------------------
C     (3) 流速のX方向成分の計算(X方向の運動方程式)
C----------------------------------------------------------------------
C       TIME = TIME+0.5D0*DTV
        TIME = TIME-0.5D0*DTV
C
        CALL FTIMER(33,0)
        CALL SETTBL
        CALL FTIMER(33,1)
        CALL BCINLV(UU,VV,WW,FF,GX0,GY0,INDU,INDV,INDW,XC,YC,ZC,
     $              UUBCN,VVBCN,MZ_ML,INDU_ML,INDV_ML,
     $              I_NS,J_NS,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $              JNOR_ML,KBOT_ML,KTOP_ML,0)
        CALL BCSUR2(UU,VV,WW,HU,HV,HW,GV,GX,GY,GZ,HH,KF,KP,KG,KH,
     $              INDU,INDV,INDP,XC,YC,ZC,YCOS,0)
C
C ..... UU,VV,WW ...
        IFLAG = 4
        CALL FTIMER(74,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(74,1)
C=================================================FOR OIL_PARTICLE START
        CALL OIL_COMM_PHYS(HDEP,HH,UU,VV,WX,WY,KF,WRK1,MX,MY,MZ)
C=================================================FOR OIL_PARTICLE END
        CALL CLHUVWD(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GV0,GX0,GY0,GZ0,
     $              GXD,GYD,GZD,CMD,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,0)
C
        IF( LBRKW.GT.0 ) THEN
           CALL CLBRKW(TMUBW,TIMBW,HH,HX,HDEP,UU,VV,HU,HV,XC,YC,ZC,
     $                 INDU,INDV,INDW,INDP,KF,KG)
        ENDIF
        IF( IFALLW.GT.0 ) THEN
           CALL CLFALLW(FALLWX,FALLWY,FALLWZ,DHX,DHY,CFALLWX,CFALLWY,
     $                  DFALLWNX,DFALLWNY,DFALLWTX,DFALLWTY,
     $                  HU,HV,UU,VV,HH,FF,GV0,GX0,GY0,XC,YC,ZC,
     $                  INDU,INDV,LLWALB,KF,KG,WRK1,WRK2,WRK3,WRK4)
        ENDIF
C
        DO 110 J =2,MYM
        DO 110 I =2,MXM
          HX(I,J) = HH(I,J)
          HY(I,J) = HH(I,J)
  110   CONTINUE
C
        CALL FTIMER(37,0)
        CALL CLUEQ(UU,VV,WW,HU,HV,HW,PP,WX,WY,CD,XC,YC,ZC,
     $             XCP,YCOS,YCOSP,YSIN,YSINP,
     $             GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GXD,
     $             CMD,CDD,COE1D,COE2D,
     $             UN,DKXX,DKXY,DKM,TMUBW,HH,FF,FALLWX,DHX,
     $             INDP,INDU,INDV,INDW,
     $             LLWALL,LLWALP,LLWALB,KF,KP,KG,WRK1,WRK2,WRK3,WRK4,
     $             WRK5,WRK6,WRK7,WRK8,FRIC,WN,HDEP,AMNG,PATM,HX,      ! WNをDIMPに使用
     $             UUBCN,VVBCN)
        CALL FTIMER(37,1)
        if(itrace.ne.0) write(6,*) 'clueq,istep,time=',istep,time
C
C----------------------------------------------------------------------
C     (4) 流速のY方向成分の計算(Y方向の運動方程式)
C----------------------------------------------------------------------
        CALL FTIMER(38,0)
        CALL CLVEQ(UU,VV,WW,HU,HV,HW,PP,WX,WY,CD,XC,YC,ZC,
     $             XCP,YCOS,YCOSP,YSIN,YSINP,
     $             GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GYD,
     $             CMD,CDD,COE1D,COE2D,
     $             VN,DKXY,DKYY,DKM,TMUBW,HH,FF,FALLWY,DHY,
     $             INDP,INDU,INDV,INDW,
     $             LLWALL,LLWALP,LLWALB,KF,KP,KG,WRK1,WRK2,WRK3,WRK4,
     $             WRK5,WRK6,WRK7,WRK8,FRIC,WN,HDEP,AMNG,PATM,HY,      ! WNをDIMPに使用
     $             UUBCN,VVBCN)
        CALL FTIMER(38,1)
        if(itrace.ne.0) write(6,*) 'clveq,istep,time=',istep,time
C
C ... 流速(U,V)を通信する .............................................
        UU = UN
        VV = VN
C
C----------------------------------------------------------------------
C     (3*,4*) 分散波モデルの計算(X,Y方向の運動方程式)
C----------------------------------------------------------------------

        IF(LDISP.EQ.1) THEN
          HUOMZ(:,:) = HU(:,:,MZ)
          HVOMZ(:,:) = HV(:,:,MZ)

          CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,1)


C ポテンシャル関数PSIの計算
          CALL DISP_CLUV(HU(1,1,MZ),HV(1,1,MZ),
     $                   HUOMZ,HVOMZ,HH,HDEP,XC,YC,INDU,INDV,
     $                   ICHILD,IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C PSIにより流量の各層への振り分け処理
          CALL DISP_DIVUV
     $       (UU,VV,FF,GX0,GY0,HH,HDEP,XC,YC,ZC,INDU,INDV)

          CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $                GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $                INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,1)
        ENDIF
C
C
      IFLAG = 1
C ... UU,VV ...
      CALL FTIMER(71,0)
      CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     $                WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
      CALL FTIMER(71,1)
C
C ... 親領域から子領域へ
C
      IF(ICHILD.GE.0) THEN
         CALL FTIMER(40,0)
         CALL CP_SNDML2NS(KF,KG,IBUF,
     1                    UU,VV,WW,TT,CC,Q2,QL,
     $                    HH,HDEP,CSEDI,ZBEDwrk,BUF,
     2                    MX,MY,MZ,
     3                    IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                    NESML(2),NESML(3),NESML(1),NESML(4),IFLAG)
         CALL FTIMER(40,1)
      END IF
C
      IF(IPARNT.GE.0.OR.IPFLG.NE.0) THEN
         CALL FTIMER(41,0)
         CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,
     1                    UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     $                    HH_ML,HDEP_ML,CSD_ML,ZBD_ML,BUF,
     2                    MX_ML,MY_ML,MZ_ML,
     3                    IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     $                                                    KTOP_ML,
     4                    NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IFLAG)
         IF(IFLAG.EQ.8) IFLAG=1
C ... ( ファイルからの値を補間 ）
         CALL CP_INTMLVAL(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,
     1                    UF_ML,VF_ML,WF_ML,TF_ML,CF_ML,HF_ML,
     2                    UB_ML,VB_ML,WB_ML,TB_ML,CB_ML,HB_ML,
     3                    KF_ML,ZC_ML,MX_ML,MY_ML,MZ_ML,
     4                    IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     5                                             KTOP_ML,IFLAG)
C
         CALL FTIMER(41,1)
C
         CALL FTIMER(42,0)
         CALL CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                   INDU,INDV,INDP,
     1                   XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     2                   GX_ML,GX,GY_ML,GY,
     3                   I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     4                   KF_ML,KG_ML,KF,KG,
     5                   UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     *                   HH_ML,HDEP_ML,HDEP,CSD_ML,ZBD_ML,
     6                   HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,
     *                   Q2BCN,QLBCN,CSDBCN,ZBDBCN,
     8                   MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     9                   IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     A                   KBOT_ML,KTOP_ML,IFLAG)
         CALL FTIMER(42,1)
      END IF
C
C ... 子領域から親領域へ
C
      IF(IPARNT.GE.0) THEN
         CALL FTIMER(43,0)
         CALL CP_BCNS2ML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                   INDU,INDV,INDW,INDP,
     2                   XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     3                   GX_ML,GY_ML,GZ_ML,GX,GY,GZ,GV,
     4                   I_ML,J_ML,K_ML,KF_ML,KG_ML,
     $                   I_NS,J_NS,KF,KG,
     5                   UU,VV,WW,TT,CC,HH,HDEP,
     6                   UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                   CSEDI,ZBEDwrk,CSD_ML,ZBD_ML,
     $                   Q2,QL,Q2_ML,QL_ML,
     7                   MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     8                   IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     9                   KBOT_ML,KTOP_ML,IFLAG)
         CALL FTIMER(43,1)
C
         CALL FTIMER(44,0)
         CALL  CP_SNDNS2ML(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     1                     HH_ML,CSD_ML,ZBD_ML,BUF,
     2                     MX_ML,MY_ML,MZ_ML,
     3                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     *                                                     KTOP_ML,
     4                     NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     5                     IFLAG)
         CALL FTIMER(44,1)
      END IF
C
        IF(ICHILD.GE.0) THEN
          CALL FTIMER(45,0)
          CALL CP_RCVNS2ML(UU,VV,WW,TT,CC,Q2,QL,
     1                     HH,CSEDI,ZBEDwrk,BUF,
     2                     MX,MY,MZ,
     3                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                     NESML(2),NESML(3),NESML(1),NESML(4),
     5                     IFLAG)
          CALL FTIMER(45,1)
        END IF
C ....................................................................
C
C ..... オーバーラップ領域部の流速を重み付け平均する
C
cc        CALL CP_UVML2NS(UU,VV,UN,VN,INDU,INDV,UUBCN,VVBCN,KF,
cc     $                  XC_REF,YC_REF,ZC)
C
        CALL BCINLV(UU,VV,WW,FF,GX0,GY0,INDU,INDV,INDW,XC,YC,ZC,
     $              UUBCN,VVBCN,MZ_ML,INDU_ML,INDV_ML,
     $              I_NS,J_NS,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $              JNOR_ML,KBOT_ML,KTOP_ML,0)
        CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,1)
C
C
C----------------------------------------------------------------------
C     (5) 流速のZ方向成分の計算(連続の式)
C----------------------------------------------------------------------
C
        CALL FTIMER(39,0)
        CALL CLWEQ(WW,HU,HV,XC,YC,ZC,YCOS,YCOSP,GZ,INDW,KF)
        CALL FTIMER(39,1)
        if(itrace.ne.0) write(6,*) 'clweq,istep,time=',istep,time
C
        CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,2)
C
        DO 120 J =2,MYM
        DO 120 I =2,MXM
          IF(HX(I,J).EQ.HDEP(I,J)+EPSH.OR.
     $       HY(I,J).EQ.HDEP(I,J)+EPSH) THEN
            HH(I,J) = HDEP(I,J)+EPSH
          END IF
  120   CONTINUE
C
C
C// 2005.02.19 ( 新しい時刻の値を流速計算セルより上の層にも設定)
C
        CALL BCSUR2(UU,VV,WW,HU,HV,HW,GV,GX,GY,GZ,HH,KF,KP,KG,KH,
     $              INDU,INDV,INDP,XC,YC,ZC,YCOS,0)
C
C ..... UU,VV,WW ...
        IFLAG = 4
        CALL FTIMER(74,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     $                  WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(74,1)
C
C
C----------------------------------------------------------------------
C     (6) 水面位置の計算
C----------------------------------------------------------------------
C       TIME = (N+1)*DT+(1/2)*DT
        TIME = TIME+0.5D0*DTV
        CALL FTIMER(33,0)
        CALL SETTBL
        CALL FTIMER(33,1)
C
        DO 299 J=1,MY
        DO 299 I=1,MX
          HX(I,J) = HH(I,J)
          KH(I,J) = KF(I,J)
 299    CONTINUE
C
        CALL FTIMER(36,0)
        CALL CLSURF(HH,KF,XC,YC,ZC,YCOS,GV,GX,GY,GZ,UU,VV,WW,
     $              HU,HV,HW,PP,RHOW,HDEP,PATM,DPS,WX,WY,WRK1,
     $              INDU,INDV,INDP,KG,KP,0)
        if(itrace.ne.0) write(6,*) 'clsurf,istep,time=',istep,time
        CALL FTIMER(36,1)
C
Cmove(20130910)
        CALL SEABOT(HU,HV,UU,VV,FF,HH,HDEP,GX,GY,XC,YC,ZC,YCOSP,
     $              INDU,INDV,INDP,KF,KP,KG)
Cmove(20130910)
C
C ..... HH,KF,KH
        CALL FTIMER(78,0)
        CALL CP_DSR_DC2(MX,MY,1,0,1,HH)
        BF2=DBLE(KF)
        CALL CP_DSR_DC2(MX,MY,1,0,1,BF2)
        KF=NINT(BF2)
        BF2=DBLE(KH)
        CALL CP_DSR_DC2(MX,MY,1,0,1,BF2)
        KH=NINT(BF2)
        CALL FTIMER(78,1)
C
C ..... 津波計算の開境界処理(自由透過条件)
        CALL FTIMER(49,0)
        IF(LTYPH.EQ.0) THEN
          CALL BOUND(HU,HV,FF,PP,RHOW,HH,HX,HDEP,XC,YC,ZC,
     $               YCOSP,INDU,INDV,KF,KG)
C ..... 台風計算の開境界処理(気圧偏差+自由透過条件)
        ELSE
          CALL BOUNDS(HU,HV,FF,PP,RHOW,HH,HX,HDEP,PATM,XC,YC,ZC,
     $                YCOSP,INDU,INDV,KF,KG)
        END IF
        CALL FTIMER(49,1)
C
        IF(LAIR.EQ.1) THEN
           FFNA=FFA
           GVNA=GVA
           KFNA=KFA
C
C ........ SET HYOURYUUBUTSU
C
C ........ SET FFA,KFA
           CALL KFSURF_AIR(FFA,KFA,HH,ZCA,INDPA)
C
C ........ BOUNDARY SET
           CALL BCSURF_AIR(HH,ZCA,INDPA,INDUA,INDVA,INDWA,KFA,KFNA)
           CALL BCINLV_AIR(UUA,VVA,UUBCAIR,VVBCAIR,UUBCAIRB,VVBCAIRB,
     $                     UUBCAIRF,VVBCAIRF,AKBCAIR,EPBCAIR,AKBCAIRB,
     $                     EPBCAIRB,AKBCAIRF,EPBCAIRF,INDUA,INDVA)
C
C ........ SET HUA,HVA,HWA
           CALL CLHUVW_AIR(HUA,HVA,HWA,UUA,VVA,WWA,FFA,GXA,GYA,GZA,
     $                     XC,YC,YCOSP,INDUA,INDVA,INDWA,KFA)
C
C ........ COPY (N+1) -> (N)
           UNA = UUA
           VNA = VVA
           WNA = WWA
C
C ........ CALC UUA~
           CALL CLUEQ_AIR(UUA,UNA,VNA,WNA,HUA,HVA,HWA,PPA,FFA,
     $                    TMUA,UUBCAIR,GVA,GXA,GYA,GZA,XC,YC,ZCA,
     $                    XCP,YCOS,YSIN,INDPA,INDUA,INDVA,INDWA,KFA,
     $                    WRK1,WRK2,WRK3,WRK4,WRK5)
C
C ........ CALC VVA~
           CALL CLVEQ_AIR(VVA,UNA,VNA,WNA,HUA,HVA,HWA,PPA,FFA,
     $                    TMUA,VVBCAIR,GVA,GXA,GYA,GZA,XC,YC,ZCA,
     $                    XCP,YCOSP,YSINP,INDPA,INDUA,INDVA,INDWA,KFA,
     $                    WRK1,WRK2,WRK3,WRK4,WRK5)
C
C ........ CALC WWA~
           CALL CLWEQ_AIR(WWA,UNA,VNA,WNA,HUA,HVA,HWA,PPA,FFA,
     $                    TMUA,GVA,GXA,GYA,GZA,XC,YC,ZCA,
     $                    YCOS,INDPA,INDUA,INDVA,INDWA,KFA,
     $                    WRK1,WRK2,WRK3,WRK4,WRK5)
C
C ........ SET HUA,HVA,HWA(2)
           CALL CLHUVW_AIR(HUA,HVA,HWA,UUA,VVA,WWA,FFA,GXA,GYA,GZA,
     $                     XC,YC,YCOSP,INDUA,INDVA,INDWA,KFA)
C
C ........ MATRIX SOLVER
           CALL MKCOE2_AIR(ADA,ALA,AUA,BBA,AD0A,AL0A,UUA,VVA,WWA,
     $                     HUA,HVA,HWA,FFA,GVA,FFNA,GVNA,
     $                     GXA,GYA,GZA,XC,YC,ZCA,XCP,
     $                     INDUA,INDVA,INDWA,INDPA,KFA)
C
           CALL ZERCLR(DPA,MXY*MZA,0.0D0)
           ITRMTX1=ITRMTX
           RNRMTX1=RNRMTX
           CALL QBICGS(DPA,ADA,ALA,AUA,BBA,WRK1,WRK2,WRK3,WRK4,WRK5,
     $                 WRK6,WRK7,INDPA,MZA)
           ITRMTXAIR=ITRMTX
           RNRMTXAIR=RNRMTX
           ITRMTX=ITRMTX1
           RNRMTX=RNRMTX1
C
C ........ UPDATE PPA,UUA,VVA,WWA
           CALL UPUVWP_AIR(UUA,VVA,WWA,PPA,DPA,XC,YC,ZCA,
     $                     INDPA,INDUA,INDVA,INDWA,KFA)
C
           IF(LTURBA.EQ.2) THEN
              AKNA = AKA
              EPNA = EPA
C
C ........... SET HUA,HVA,HWA(3)
              CALL CLHUVW_AIR(HUA,HVA,HWA,UUA,VVA,WWA,FFA,GXA,GYA,GZA,
     $                        XC,YC,YCOSP,INDUA,INDVA,INDWA,KFA)
C
C ........... k-εモデル
              CALL CLKEPS_AIR(AKA,EPA,AKNA,EPNA,UUA,VVA,WWA,HUA,HVA,HWA,
     $                        FFA,TMUA,AKBCAIR,EPBCAIR,GVA,GXA,GYA,GZA,
     $                        XC,YC,ZCA,YCOS,
     $                        INDPA,INDUA,INDVA,INDWA,
     $                        WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7)
           ENDIF
C
           CALL CLTMU_AIR(TMUA,AKA,EPA,UUA,VVA,WWA,XC,YC,ZCA,
     $                    INDUA,INDVA,INDWA,INDPA,WRK1,WRK2,WRK3)
        ENDIF
C
        IFLAG = 2
C
C ... 親領域から子領域へ
C
        IF(ICHILD.GE.0) THEN
           CALL FTIMER(50,0)
           CALL CP_SNDML2NS(KF,KG,IBUF,
     1                      UU,VV,WW,TT,CC,Q2,QL,
     $                      HH,HDEP,CSEDI,ZBEDwrk,BUF,
     2                      MX,MY,MZ,
     3                      IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                      NESML(2),NESML(3),NESML(1),NESML(4),IFLAG)
           CALL FTIMER(50,1)
        END IF
C
        IF(IPARNT.GE.0.OR.IPFLG.NE.0) THEN
           CALL FTIMER(51,0)
           CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,
     1                      UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     $                      HH_ML,HDEP_ML,CSD_ML,ZBD_ML,BUF,
     2                      MX_ML,MY_ML,MZ_ML,
     3                      IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     $                                                      KTOP_ML,
     4                    NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IFLAG)
         IF(IFLAG.EQ.8) IFLAG=2
C ... ( ファイルからの値を補間 ）
           CALL CP_INTMLVAL(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,
     1                      UF_ML,VF_ML,WF_ML,TF_ML,CF_ML,HF_ML,
     2                      UB_ML,VB_ML,WB_ML,TB_ML,CB_ML,HB_ML,
     3                      KF_ML,ZC_ML,MX_ML,MY_ML,MZ_ML,
     4                      IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     5                                             KTOP_ML,IFLAG)
C
           CALL FTIMER(51,1)
C
           CALL FTIMER(52,0)
           CALL CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                     INDU,INDV,INDP,
     1                     XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     2                     GX_ML,GX,GY_ML,GY,
     3                     I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     4                     KF_ML,KG_ML,KF,KG,
     5                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     *                     HH_ML,HDEP_ML,HDEP,CSD_ML,ZBD_ML,
     6                     HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,
     *                     Q2BCN,QLBCN,CSDBCN,ZBDBCN,
     8                     MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     9                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     A                     KBOT_ML,KTOP_ML,IFLAG)
           CALL FTIMER(52,1)
C
C ........ オーバーラップ領域の水位
           CALL CP_HHML2NS(HH,HDEP,HHBCN,HHW,KF,XC_REF,YC_REF,I_ML,J_ML,
     $                  MX_ML,MY_ML,IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,1)
           CALL BCINLF(FF,ZC,INDP,HH,HDEP,HHBCN,1)
        END IF
C
C
        IF(IPARNT.GE.0) THEN
           CALL FTIMER(53,0)
           CALL CP_BCNS2ML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                     INDU,INDV,INDW,INDP,
     2                     XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     3                     GX_ML,GY_ML,GZ_ML,GX,GY,GZ,GV,
     4                     I_ML,J_ML,K_ML,KF_ML,KG_ML,
     $                     I_NS,J_NS,KF,KG,
     5                     UU,VV,WW,TT,CC,HH,HDEP,
     6                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                     CSEDI,ZBEDwrk,CSD_ML,ZBD_ML,
     $                     Q2,QL,Q2_ML,QL_ML,
     7                     MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     8                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     9                     KBOT_ML,KTOP_ML,IFLAG)
           CALL FTIMER(53,1)
C
           CALL FTIMER(54,0)
           CALL  CP_SNDNS2ML(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,Q2_ML,QL_ML,
     1                       HH_ML,CSD_ML,ZBD_ML,BUF,
     2                       MX_ML,MY_ML,MZ_ML,
     3                       IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     *                                                       KTOP_ML,
     4                       NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     5                       IFLAG)
           CALL FTIMER(54,1)
        END IF
C
        IF(ICHILD.GE.0) THEN
          CALL FTIMER(55,0)
          CALL CP_RCVNS2ML(UU,VV,WW,TT,CC,Q2,QL,
     1                     HH,CSEDI,ZBEDwrk,BUF,
     2                     MX,MY,MZ,
     3                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                     NESML(2),NESML(3),NESML(1),NESML(4),
     5                     IFLAG)
          CALL FTIMER(55,1)
        END IF
C
        CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
C
Cmove(20130906)
C        CALL SEABOT(HU,HV,UU,VV,FF,HH,HDEP,GX,GY,XC,YC,ZC,YCOSP,
C     $              INDU,INDV,INDP,KF,KP,KG)
Cmove(20130906)
C
        CALL FTIMER(56,0)
        CALL BCSURF(PP,UU,VV,WW,HH,TT,QQ,QW,PATM,DPS,WX,WY,CD,
     $              XC,YC,ZC,YCOSP,GV,GX,GY,GZ,HDEP,RWWB,RWWF,
     $              INDU,INDV,INDP,KF,KP,KG,KH,1)
        CALL FTIMER(56,1)
C
C ..... UU,VV ...
        IFLAG = 1
        CALL FTIMER(71,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     $                  WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(71,1)
C
C ..... WW,PP,WX,WY,CD,PATM,HH,KF,KP ...
        IFLAG=2
        CALL FTIMER(72,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     $                  WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(72,1)
C
C----------------------------------------------------------------------
C     (7) エネルギー式の計算
C----------------------------------------------------------------------
C        KH=KF(OLD),HX=HH(OLD)
C
         IF(LTEMP.EQ.1) THEN
           CALL FTIMER(57,0)
           CALL CLENGY(TT,TN,HU,HV,HW,DKXX,DKYY,DKH,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,GV,GX,GY,GZ,HH,HX,HDEP,QQ,
     $                 INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KH,KG,KP,
     $                 TTBCN,WRK1,WRK2,WRK3,WRK4,WRK5)
           if(itrace.ne.0) write(6,*) 'clengy,istep,time=',istep,time
           CALL FTIMER(57,1)
         END IF
C
C----------------------------------------------------------------------
C     (8) 濃度輸送式の計算
C----------------------------------------------------------------------
         IF(LCONC.EQ.1) THEN
           CALL FTIMER(58,0)
           CALL CLCONC(CC,CN,HU,HV,HW,DKXX,DKYY,DKH,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,GV,GX,GY,GZ,HH,HX,HDEP,QW,
     $                 INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KH,KG,KP,
     $                 CCBCN,WRK1,WRK2,WRK3,WRK4,WRK5)
           if(itrace.ne.0) write(6,*) 'clconc,istep,time=',istep,time
           CALL FTIMER(58,1)
         END IF
C
C----------------------------------------------------------------------
C     (9) 地形変化モデルの計算
C----------------------------------------------------------------------
         IF(LSEDI.EQ.1) THEN
C ........ シールズ数の計算
           CALL CLSHL(SHLSD,USSD,UU,VV,HU,HV,XC,YC,ZC,GV0,HH,HDEP,AMNG,
     $                KF,KG,GXBDH,GYBDH,KIBDH,KJBDH)
C
C ........ 交換砂量の計算
           CALL CLWEX(WEXSD,EXSDE,EXSDD,SHLSD,USSD,CSEDI,ZBED,
     $                ZC,GV,HH,HX,HDEP,INDP,KF,KH,KG)
C
C ........ 浮遊砂濃度の計算
           CALL CLSEDI(CSEDI,CSEDIN,CSDAVE,WEXSD,EXSDE,EXSDD,ZBED,
     $                 HU,HV,HW,DKXX,DKYY,DKH,XC,YC,ZC,XCP,YCOS,YCOSP,
     $                 GV,GX,GY,GZ,HH,HX,HDEP,INDP,INDU,INDV,INDW,
     $                 LLWALL,LLWALP,KF,KH,KG,KP,CSDBCN,
     $                 WRK1,WRK2,WRK3,WRK4,WRK5)
           if(itrace.ne.0) write(6,*) 'clsedi,istep,time=',istep,time
C
C ........ 掃流砂高さの計算
           CALL CLBED(ZBED,ZBEDN,QBX,QBY,SHLSD,WEXSD,UU,VV,HU,HV,
     $                XC,YC,ZC,HH,HDEP,KF,KG,GX,GY,INDU,INDV,
     $                MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     $                I_ML,J_ML,I_NS,J_NS,XC_ML,YC_ML,XC,YC,
     $                KF_ML,KG_ML,KF,KG,XC_REF,YC_REF,
     $                IEAS,IWES,JSOU,JNOR,
     $                IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,NESML,BUF,
     $                GXBDH,GYBDH,KIBDH,KJBDH)
           if(itrace.ne.0) write(6,*) 'clbed,istep,time=',istep,time
C
C ........ 地形変化によるポーラス値とHDEPの変更等
           IF( MFDBCKSD.EQ.1 ) THEN
           CALL SETZB(DZBUF,ZBED,ZBEDN,HDEP,XC,YC,ZC,GV0,GX0,GY0,
     $                GV,GX,GY,GVD,GXD,GYD,HU,HV,HW,UU,VV,WW,FF,
     $                GZ,GZ0,YCOSP,HH,CSEDI,GXBDH,GYBDH,
     $               BUF,GX_ML,GY_ML,GZ_ML,HDEP_ML,HH_ML,WRK1,WRK2,WRK3,
     $                INDP,INDU,INDV,INDW,KF,KP,KG,
     $                KIBDH,KJBDH,IBUF,MX_ML,MY_ML,MZ_ML,
     $                IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
     $                IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     $                INDU_ML,INDV_ML,INDW_ML,INDP_ML,NC)
C
           ENDIF
C
         ELSEIF(MOFFLNSD.EQ.1) THEN
           CALL OUTOFFLNSD(HH,UU,VV,WW,1)
         ENDIF
C
C
C----------------------------------------------------------------------
C     (98) 建物破壊
C----------------------------------------------------------------------
         IF( LSTOCDS.EQ.1 ) THEN
            CALL DSTRY(XC,YC,ZC,
     &                 HDEP,HH,KF,KG,KP,KH,GV,GX,GY,GZ,
     $                 GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,FF,AMNG,
     &                 INDP,INDU,INDV,INDW,
     &                 HTDST1,HTDST2,IDST,TMDST,
     &                 LLWALL)
         ENDIF
C
C----------------------------------------------------------------------
C     (99) 閉塞処理
C----------------------------------------------------------------------
         IF( LSTOCDS.EQ.1 ) THEN
            CALL BLOCK(GV0,GX0,GY0,INDP,KBLC)
         ENDIF
C
C----------------------------------------------------------------------
C     (9) 圧力の計算
C----------------------------------------------------------------------
        CALL FTIMER(48,0)
        CALL CLPRES(RHOW,PP,TT,CC,GV0,HH,PATM,XC,YC,ZC,INDP,KF,KG)
        CALL FTIMER(48,1)
        if(itrace.ne.0) write(6,*) 'clpres,istep,time=',istep,time
C
C
C----------------------------------------------------------------------
C     (9-2) Mellor-Yamadaモデルの計算
C----------------------------------------------------------------------
         IF(LTURB.EQ.3) THEN
           CALL FTIMER(58,0)
           CALL CLMYEQ(Q2,Q2P,QL,QLP,UU,VV,WW,DKXX,DKXY,DKYY,DKM,DKH,
     $                 HU,HV,HW,HX,HDEP,KH,
     $                 RHOW,GV,GX,GY,GZ,XC,YC,ZC,XCP,YCOS,YCOSP,
     $                 HH,WX,WY,CD,INDU,INDV,INDW,INDP,
     $                 LLWALL,LLWALP,KF,KG,KP,
     $                 WRK1,WRK2,WRK3,WRK4,WRK5,Q2BCN,QLBCN)
c           if(itrace.ne.0) write(6,*) 'clconc,istep,time=',istep,time
           CALL FTIMER(58,1)
         END IF
C
C----------------------------------------------------------------------
C     (10) 乱流粘性係数の計算
C----------------------------------------------------------------------         C
         IF(LTURB.EQ.1) THEN
           CALL FTIMER(59,0)
           CALL CLTMU(WRK1,WRK2,UU,VV,WW,WRK1,WRK2,WRK3,XC,YC,ZC,
     $                INDU,INDV,INDW,INDP,KF,KP,KG,TMU)
           DKXX = TMU
           DKXY = TMU
           DKYY = TMU
           DKM  = TMU
           DKH  = TMU
           CALL FTIMER(59,1)
         END IF
C
C ..... PP,RHOW,TT,CC,TMU ...
        IFLAG=3
        CALL FTIMER(73,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     $                  WRK1,WRK2,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(73,1)
C
C
      CALL COM_MA2(HH,HDEP,HU,HV,0)
C
C----------------------------------------------------------------------
C     (11) 終了判定
C----------------------------------------------------------------------
C       TIME = (N+1)*DT
        TIME = TIME-0.5D0*DTV
C
        IF( LAIR.NE.1 ) THEN
           WRITE(LP,1000) ISTEP,TIME,DT,VELMAX
        ELSE
           WRITE(LP,2000) ISTEP,TIME,DT,VELMAX,
     $                    VELMAXAIR,ITRMTXAIR,RNRMTXAIR
        ENDIF
 1000    FORMAT('STEP=',I7,' TIME=',1P,E12.5,' DT=',E9.2,
     $          ' VMAX=',E10.3)
 2000    FORMAT('STEP=',I7,' TIME=',1P,E12.5,' DT=',E9.2,
     $          ' VMAX=',E10.3,
     $          ' (AIR)VMAX=',E10.3,' ITER=',I3,' !B-A*X!=',E8.1)
C
         IF(LTYPH.NE.0) THEN
           CALL CALEN(IEND1(1))
           IF(IEND1(1).EQ.1) IFINISH=1
         END IF
C
C<<<<< (START) STOC-DM VERSION  <<<<<<<
      IF( TIME .GE. REND .OR. ISTEP.EQ.MAXSTP ) THEN
         IFINISH = 1
      ENDIF
      IF( NB_SD.GE.0 )THEN
      CALL COM_DRIFT2 (HDEP,HH,UU,VV,IDST,KBLC)
C                                      !  物理量(HT,HH,UU,VV)の通信
      ENDIF
      IF( NB_SD_MAIN.EQ.1 )THEN
      CALL COM_DRIFT3(TIME,REND)       !  時刻の通信:<Area01>
      ENDIF
      CALL COM_DRIFT4(IFINISH)         !  漂流計算との同期:<全領域>
C
      IEND1(1) = IFINISH
C
      WTM2=MPI_WTIME()
      IEND1(2)=0
      IF(WTM2-WTM1.GT.ETIME) IEND1(2)=1
      CALL MPI_ALLREDUCE(IEND1,IEND,2,MPI_INTEGER,MPI_MAX,
     $                   comm_mlicdsmg2fc,IERR)
      IFINISH = IEND(1)
      IEE     = IEND(2)
      IF( IFINISH.EQ.1.AND.ETIME.LT.1.0D30 ) IEE=1
      IF( IEE.EQ.1 ) THEN
         call flnam('.ars')
         write(lp,*) trim(CFLNM)
         OPEN(IFLAR,FILE=trim(CFLNM),STATUS='UNKNOWN',
     $        FORM='FORMATTED')
         IF( IFINISH.EQ.1 ) THEN
            WRITE(IFLAR,'(I10,1X,F10.2)') -999,TIME
            CLOSE(IFLAR)
            OPEN(IFLAR,FILE='stop_ars',STATUS='UNKNOWN')
            CLOSE(IFLAR)
         ELSE
            WRITE(IFLAR,'(I10,1X,F10.2)') ISTEP,TIME
            CLOSE(IFLAR)
         ENDIF
         IF( LREST.EQ.0 ) IREST(IREST0)=0
         IF( LREST.EQ.1 ) RREST(IREST0)=-1.0D30
      ENDIF
      IF( IFINISH .EQ. 1 .OR. IEE.EQ.1 ) GO TO 200
C<<<<<  (END)  STOC-DM VERSION  <<<<<<<
C
C
C----------------------------------------------------------------------
C     (11) 結果の出力
C----------------------------------------------------------------------
         CALL FTIMER(60,0)
         CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,PP,RHOW,
     $               TT,CC,Q2,QL,DKM,FF,HH,
     $               SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $               EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $               INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $               HDEP,HHBCN,UUBCN,VVBCN,IDST,TMDST,
     $               UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,1)
         CALL FTIMER(60,1)
C
         IF(RFILE(3).GT.0.0D0) THEN
           IF(TIME.GE.RFILE(1)-0.1D0*DT) RFILE(1)=RFILE(1)+RFILE(3)
           IF(RFILE(1).GT.RFILE(2)) RFILE(1)=1.0D10
         END IF
C
c add 20130423(s)
c         IF(MOD(ISTEP,INTERVAL).EQ.0) THEN
c         write(cfile,'(i4.4)') ISTEP/INTERVAL
c         open(11,file=cfile,status='unknown')
c         do j=2,MYM
c         write(11,'(1p4e13.4e3)')
c     $   ((XC(1,i,j),YC(1,j),HH(i,j),HDEP(i,j)),i=2,MXM)
c         write(11,*)
c         enddo
c         close(11)
c         ENDIF
!         do j=2,MYM
!         if( time<120.0d0*2.0d0 ) then 
!            hhnow=2.5d0*sin(2.0d0*3.1415926535898d0/120.0d0*time)
!         else 
!            hhnow=0.0d0
!         endif 
!         if( time+dt<120.0d0*2.0d0 ) then 
!            hhnxt=2.5d0*sin(2.0d0*3.1415926535898d0/120.0d0*(time+dt))
!         else 
!            hhnxt=0.0d0
!         endif 
!         uu1=(hu(2,j,MZ)+xc(4,2,j)/dt*(hhnxt-hhnow))
!     $      /(hhnow-hdep(2,j))
!         RINLT(1,1) = uu1
!c
!         hh(2,j)=hhnow
!         enddo

!!         if( time<50.0d0*2.0d0 ) then 
!!            hhnow=7.d0*sin(2.0d0*3.1415926535898d0/50.0d0*time)
!!         else 
!!            hhnow=0.0d0
!!         endif 
!!         if( time+dt<50.0d0*2.0d0 ) then 
!!            hhnxt=7.d0*sin(2.0d0*3.1415926535898d0/50.0d0*(time+dt))
!!         else 
!!            hhnxt=0.0d0
!!         endif 
!!         hh(100,100)=2.d0*hhnow
!!         hh(101,100)=hhnow
!!         hh(99,100)=hhnow
!!         hh(100,101)=hhnow
!!         hh(100,99)=hhnow
c add 20130423(e)
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
C     (11) 最終結果の出力とファイルのクローズ
C----------------------------------------------------------------------
      CALL FTIMER(60,0)
      IF(RFILE(3).GT.0.0D0) CLOSE(IFLBO,STATUS='KEEP')
      CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,PP,RHOW,
     $            TT,CC,Q2,QL,DKM,FF,HH,
     $            SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $            EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $            INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $            HDEP,HHBCN,UUBCN,VVBCN,IDST,TMDST,
     $            UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,2)
      CALL FTIMER(60,1)
C
      CALL COM_MA2(HH,HDEP,HU,HV,-1)
C
C ... オフライン土砂移動計算用ファイルをクローズ
      IF(LSEDI.EQ.0 .AND. MOFFLNSD.EQ.1) CALL OUTOFFLNSD(HH,UU,VV,WW,2)
C
c      j=2
c      write(lp,*) 'uua j=',j,' istep=',istep
c      write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
c      do k=mza,1,-1
c         write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(uua(i,j,k),i=1,mxm)
c      enddo
c      write(lp,*) 'wwa j=',j,' istep=',istep
c      write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mx)
c      do k=mza,1,-1
c         write(lp,'(i4,a1,<mx>f8.3)') k,'|',(wwa(i,j,k),i=1,mx)
c      enddo
C
      DEALLOCATE(BF2,ZBEDwrk,HUOMZ,HVOMZ,
     $           UB_ML,VB_ML,WB_ML,TB_ML,CB_ML,HB_ML,
     $           UF_ML,VF_ML,WF_ML,TF_ML,CF_ML,HF_ML)
C
      RETURN
      END
