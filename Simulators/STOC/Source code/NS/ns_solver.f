      SUBROUTINE SOLVER(XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,
     $                  XC_REF,YC_REF,
     $                  XC_ML,YC_ML,ZC_ML,GV,GX,GY,GZ,
     $                  GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,
     $                  CMD,CDD,COE1D,COE2D,GX_ML,GY_ML,GZ_ML,
     $                  UU,VV,WW,HU,HV,HW,PP,TT,CC,AK,EP,TMU,FF,RHOW,HH,
     $                  TMUBW,TIMBW,
     $                  PATM,DPS,QQ,QW,WX,WY,UN,VN,WN,TN,CN,AKN,EPN,RL,
     $                  CSEDI,CSEDIN,CSDAVE,SHLSD,USSD,WEXSD,
     $                  EXSDE,EXSDD,ZBED,ZBEDN,ZBED0,QBX,QBY,DZBUF,
     $                  CSD_ML,ZBD_ML,CSDBCN,ZBDBCN,
     $                  GXBDH,GYBDH,KIBDH,KJBDH,
     $                  AD0,AL0,AD,AL,AU,BB,DP,
     $                  WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,WRK8,VLEND,
     $                  TMEND,FREND,FRIC,HDEP,AMNG,CD,HX,HY,HHW,
     $                  RMMB,RMMF,INDP,INDU,INDV,INDW,INDK,
     $                  INDP_ML,INDU_ML,INDV_ML,INDW_ML,
     $                  KF,KG,KP,KH,KF_ML,KG_ML,IBUF,
     $                  UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     $                  HH_ML,HDEP_ML,BUF,
     $                  HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,AKBCN,EPBCN,
     $                  I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     $                  MX_ML,MY_ML,MZ_ML,
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
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
C        (3) 運動方程式の計算
C        (4) 連続の式・運動方程式の計算
C        (5) エネルギー式の計算
C        (6) 濃度輸送式の計算
C        (7) 乱流エネルギー・乱流エネルギー散逸式の計算
C        (8) 乱流粘性係数の計算
C        (9) 終了判定
C        (10) 結果の出力
C
C     (11) 最終結果の出力とファイルのクローズ
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
      INCLUDE 'TURBR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'mpif.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'DRIFT.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'AGENT.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML
      INTEGER,INTENT(INOUT)::
     $   IEAS_ML,IWES_ML,JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML
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
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),RHOW(MX,MY,MZ),FRIC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HY(MX,MY),HHW(MX,MY)
      REAL(8),INTENT(INOUT)::QQ(MX,MY,MZ),QW(MX,MY)
      REAL(8),INTENT(INOUT)::TMUBW(MX,MY),TIMBW(MX,MY)
      REAL(8),INTENT(INOUT)::PATM(MX,MY),DPS(MX,MY),WX(MX,MY),WY(MX,MY)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::RMMB(MX,MY,9),RMMF(MX,MY,9)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDK(MX,MY,MZ)
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
      REAL(8),INTENT(INOUT)::AKN(MX,MY,MZ),EPN(MX,MY,MZ),RL(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::AD0(MX,MY,MZ),AL0(3,MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AD(MX,MY,MZ),AL(3,MX,MY,MZ),AU(3,MX,MY,MZ)
      REAL(8),INTENT(INOUT)::BB(MX,MY,MZ),DP(MX,MY,MZ)
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
     $   AK_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1),
     $   EP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $         KBOT_ML-1:KTOP_ML+1)
      REAL(8),INTENT(INOUT)::
     $   HH_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::
     $   HDEP_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8),INTENT(INOUT)::BUF(*)
      REAL(8),INTENT(INOUT)::HHBCN(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4),
     $   WWBCN(NXY,MZ,4),TTBCN(NXY,MZ,4),CCBCN(NXY,MZ,4),
     $   AKBCN(NXY,MZ,4),EPBCN(NXY,MZ,4)
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML)
      INTEGER,INTENT(INOUT)::I_NS(2,MX),J_NS(2,MY),K_NS(2,MZ)
C
      INTEGER::ITRACE=1
      INTEGER::IEND(2),IEND1(2),IEE
C
C<<<<< (START) STOC-DM VERSION  <<<<<<<
      INTEGER::IFINISH
C<<<<<  (END)  STOC-DM VERSION  <<<<<<<
C
      REAL(8)::C1,C2,DT0
      INTEGER::I,IAD,ICHILD,IERR,IFLAG,IPARNT,IST0,ISTEP1
      INTEGER::J,JAD,K,KAD,NC
      INTEGER::IEAS,IWES,JNOR,JSOU,KBOT,KTOP
      REAL(8),ALLOCATABLE::BF2(:,:)
      REAL(8)::DUMMY1(1,1)
      INTEGER::IDUMMY1(1,1),IDUMMY2(1,1)
      INTEGER::ITRMTX1,ITRMTXAIR
      REAL(8)::RNRMTX1,RNRMTXAIR
C
C
      ALLOCATE(BF2(MX,MY),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_SOLVER',6100)
        WRITE(LP,*) 'CANNOT ALLOCATE BF2'
        CALL ABORT1('')
      END IF
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
      CALL CP_HHML2NS(HH,HDEP,HHBCN,HHW,KF,XC_REF,YC_REF,I_ML,J_ML,
     $                MX_ML,MY_ML,IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,0)
      if(itrace.ne.0) then
        WRITE(6,602) IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML
 602  FORMAT('IWES_ML,IEAS_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML=',6I5)
      end if
C
      TIME   = RSTART
C
      IF( NB_SC.GT.0 ) THEN
         CALL CADMAS_SEND(UU,VV,WW,FF,ZC)
         CALL CADMAS_RECV
      ENDIF
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
CCC      CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
      CALL BCINLV(UU,VV,WW,FF,GX0,GY0,INDU,INDV,INDW,XC,YC,ZC,
     $            UUBCN,VVBCN,MZ_ML,INDU_ML,INDV_ML,
     $            I_NS,J_NS,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $            JNOR_ML,KBOT_ML,KTOP_ML,-1)
      CALL CLDENS(RHOW,TT,CC,GV,ZC,INDP,KF)
c      write(*,*) ': bcsurf called at initial process'
      KH = KF
      CALL BCSURF(PP,UU,VV,WW,HH,TT,QQ,QW,PATM,DPS,WX,WY,CD,
     $            XC,YC,ZC,YCOSP,GV,GX,GY,GZ,HDEP,RMMB,RMMF,
     $            INDU,INDV,INDP,KF,KP,KG,KH,0)
      IF(LSURF.EQ.0) KH=KF
C
C     乱流粘性係数を計算
      IF(LTURB.EQ.1.OR.LTURB.EQ.2) THEN
C ... LESモデル.or.k-εモデル
        CALL CLTMU(AK,EP,UU,VV,WW,WRK1,WRK2,WRK3,XC,YC,ZC,
     $             INDU,INDV,INDW,INDP,KF,KP,KG,TMU)
C
        DO 371 K=1,MZ
        DO 371 J=1,MY
        DO 371 I=1,MX
           IF(K.LE.KF(I,J)) THEN
              IF(INDP(I,J,K).NE.0) THEN
                 TMU(I,J,K) = MIN(TMU(I,J,K),TVSMAX)
                 TMU(I,J,K) = MAX(TMU(I,J,K),TVSMIN)
              END IF
           END IF
  371   CONTINUE
      ELSE IF(LTURB.EQ.0) THEN
C ... 乱流モデルなし
        CALL ZERCLR(TMU,MXYZ,0.0D0)
      ELSE IF(LTURB.EQ.4) THEN
C ... SGS モデル
C       粘性係数、温度拡散、拡散係数をゼロセット
        ANUH = 0.0D0
        ANUV = 0.0D0
        ALPH = 0.0D0
        ALPV = 0.0D0
        DIFH = 0.0D0
        DIFV = 0.0D0
        CALL ZERCLR(TMU,MXYZ,0.0D0)
        DO 350 K=1,MZ
        DO 350 J=1,MY
        DO 350 I=1,MX
          RL(I,J,K)  = (XC(4,I,J)*YC(4,J)*ZC(4,K))**(1.0D0/3.0D0)
          TMU(I,J,K) = CSMG*RL(I,J,K)*SQRT(AK(I,J,K))
          TMU(I,J,K) = MAX(TMU(I,J,K),TVSMIN)
  350   CONTINUE
      END IF
C
C ... 全変数の通信
      IFLAG = 0
      CALL FTIMER(70,0)
      CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
      CALL FTIMER(70,1)
C
C     (0.a) ファイルのオープンと初期分布の出力
      CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,PP,RHOW,
     $            TT,CC,AK,EP,TMU,FF,HH,
     $            SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $            EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $            INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $            HDEP,HHBCN,UUBCN,VVBCN,IDUMMY1,DUMMY1,
     $            UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,0)
C
C     (0.b)ポアソン方程式の係数を作成
      CALL MKCOE1(AD0,AL0,XC,YC,ZC,XCP,GX,GY,GZ,GV,GV0,GVD,CMD,
     $            INDP,INDU,INDV,INDW)
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
      CALL COM_DRIFT2 (HDEP,HH,UU,VV,IDUMMY1,IDUMMY2)
                                       !  物理量(HT,HH,UU,VV)の通信
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
      IF( NB_SC.GT.0 ) THEN
         CALL CADMAS_SEND(UU,VV,WW,FF,ZC)
         CALL CADMAS_RECV
      ENDIF
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
      IST0   = ISTEP1

C=================================================FOR OIL_PARTICLE START
      CALL OIL_COMM_STEP
C=================================================FOR OIL_PARTICLE END

      DO 100 ISTEP=ISTEP1,MAXSTP
C----------------------------------------------------------------------
C     (1) 時間刻みの設定
C----------------------------------------------------------------------
        CALL FTIMER(31,0)
        CALL SETDT(XC,YC,ZC,GV,GX,GY,GZ,UU,VV,WW,TMU,
     $             INDP,INDU,INDV,INDW,IAD,JAD,KAD)
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
        IF(LSURF.EQ.1) THEN
ckt ---
        if(nsomer(1).ne.3.and.nsomer(2).ne.3.and.
     *     nsomer(3).ne.3.and.nsomer(4).ne.3) then
          CALL BCTIDE(PP,UU,VV,WW,RHOW,FF,HH,PATM,ZC,KF,KP,KG,
     $                INDU,INDV,INDW)
        endif
ckt ---
          CALL BCINLF(FF,ZC,INDP,HH,HDEP,HHBCN,0)
          CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
        END IF
        CALL FTIMER(32,1)
C
C----------------------------------------------------------------------
C     (3) 運動方程式の計算
C         圧力と同じ時間レベルの流速を計算C
C----------------------------------------------------------------------
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
C ..... FF,HH
        CALL FTIMER(78,0)
        CALL CP_DSR_DC2(MX,MY,MZ,0,1,FF)
        CALL CP_DSR_FFF(FF)
        CALL CP_DSR_DC2(MX,MY,1,0,1,HH)
        CALL FTIMER(78,1)
C
C ..... UU,VV,WW ...
        IFLAG = 4
        CALL FTIMER(74,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(74,1)
C=================================================FOR OIL_PARTICLE START
        CALL OIL_COMM_PHYS(HDEP,HH,UU,VV,WX,WY,KF,WRK1,MX,MY,MZ)
C=================================================FOR OIL_PARTICLE END
        CALL FTIMER(34,0)
        DO 140 NITER=1,MXITER
C
C----------------------------------------------------------------------
C     (3.0) 繰り返し時の流速を設定
C----------------------------------------------------------------------
          IF( NITER.EQ.1 ) THEN
            DO 150 K=1,MZ
            DO 150 J=1,MY
            DO 150 I=1,MX
              UN(I,J,K) = UU(I,J,K)
              VN(I,J,K) = VV(I,J,K)
              WN(I,J,K) = WW(I,J,K)
  150       CONTINUE
          ELSE
C ......... 時間刻みが変動するときは、
            C1 = DTOLD/(DTOLD+DT)
C            C1 = 0.5D0
            C2 = 1.0D0-C1
            DO 120 K=2,MZM
            DO 120 J=2,MYM
            DO 120 I=2,MXM
              UU(I,J,K) = C1*UU(I,J,K) + C2*UN(I,J,K)
              VV(I,J,K) = C1*VV(I,J,K) + C2*VN(I,J,K)
              WW(I,J,K) = C1*WW(I,J,K) + C2*WN(I,J,K)
  120       CONTINUE
          END IF
C
          CALL CLHUVWD(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GV0,GX0,GY0,GZ0,
     $                 GXD,GYD,GZD,CMD,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $                 INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,0)
C
C
          IF( LBRKW.GT.0 ) THEN
             CALL CLBRKW(TMUBW,TIMBW,HH,HX,HDEP,UU,VV,HU,HV,XC,YC,ZC,
     $                   INDU,INDV,INDW,INDP,KF,KG)
          ENDIF
          IF( IFALLW.GT.0 ) THEN
             CALL CLFALLW(FALLWX,FALLWY,FALLWZ,DHX,DHY,CFALLWX,CFALLWY,
     $                    DFALLWNX,DFALLWNY,DFALLWTX,DFALLWTY,
     $                    HU,HV,UU,VV,HH,FF,GV0,GX0,GY0,XC,YC,ZC,
     $                    INDU,INDV,LLWALB,KF,KG,WRK1,WRK2,WRK3,WRK4)
          ENDIF
C
          IF(LSURF.EQ.1) THEN
            DO 110 J =2,MYM
            DO 110 I =2,MXM
              HX(I,J) = HH(I,J)
              HY(I,J) = HH(I,J)
  110       CONTINUE
          END IF
C
C----------------------------------------------------------------------
C     (3.1) 仮流速のX方向成分の計算(WRK7)
C----------------------------------------------------------------------
C
          CALL FTIMER(37,0)
          CALL CLUEQ(UU,VV,WW,HU,HV,HW,PP,WX,WY,CD,XC,YC,ZC,
     $               XCP,YCOS,YCOSP,YSIN,YSINP,
     $               GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GXD,CMD,CDD,
     $               COE1D,COE2D,WRK7,TMU,TMUBW,HH,FF,FALLWX,DHX,
     $               INDP,INDU,INDV,INDW,
     $               LLWALL,LLWALP,LLWALB,KF,KP,KG,WRK1,WRK2,WRK3,WRK4,
     $               WRK5,WRK6,UN,DP,FRIC,WRK8,HDEP,AMNG,PATM,HX,
     $               UUBCN,VVBCN)
C ......  WRK7=UP
          CALL FTIMER(37,1)
        if(itrace.ne.0) write(6,*) 'clueq,istep,time=',istep,time
C
C----------------------------------------------------------------------
C     (3.2) 仮流速のY方向成分の計算(AD)
C----------------------------------------------------------------------
          CALL FTIMER(38,0)
          CALL CLVEQ(UU,VV,WW,HU,HV,HW,PP,WX,WY,CD,XC,YC,ZC,
     $               XCP,YCOS,YCOSP,YSIN,YSINP,
     $               GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GYD,CMD,CDD,
     $               COE1D,COE2D,AD,TMU,TMUBW,HH,FF,FALLWY,DHY,
     $               INDP,INDU,INDV,INDW,
     $               LLWALL,LLWALP,LLWALB,KF,KP,KG,WRK1,WRK2,WRK3,WRK4,
     $               WRK5,WRK6,VN,DP,FRIC,WRK8,HDEP,AMNG,PATM,HY,
     $               UUBCN,VVBCN)
C ......  AD=VP
          CALL FTIMER(38,1)
          if(itrace.ne.0) write(6,*) 'clveq,istep,time=',istep,time
C
C ... 流速(U,V)を通信する .............................................
C
        IFLAG = 1
C ..... UU,VV ...
        CALL FTIMER(71,0)
        CALL CP_NEIBCOM(WRK7,AD,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(71,1)
C
C ... 親領域から子領域へ
C
        IF(IPARNT.GE.0) THEN
          CALL FTIMER(41,0)
          CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,
     1                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     $                     HH_ML,HDEP_ML,CSD_ML,ZBD_ML,BUF,
     2                     MX_ML,MY_ML,MZ_ML,
     3                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     $                                                     KTOP_ML,
     4                    NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IFLAG)
          CALL FTIMER(41,1)
C
          CALL FTIMER(42,0)
          CALL CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                    INDU,INDV,INDP,
     1                    XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     2                    GX_ML,GX,GY_ML,GY,
     3                    I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     4                    KF_ML,KG_ML,KF,KG,
     5                    UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     *                    HH_ML,HDEP_ML,HDEP,CSD_ML,ZBD_ML,
     6                    HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,
     *                    AKBCN,EPBCN,CSDBCN,ZBDBCN,
     8                    MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     9                    IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     A                    KBOT_ML,KTOP_ML,IFLAG)
          CALL FTIMER(42,1)
        END IF
C
C ... 子領域から親領域へ(仮流速UU,VV)
C     WRK7=UU~(N+1),AD=VV~(N+1)
C
        IF(IPARNT.GE.0) THEN
          CALL FTIMER(43,0)
          CALL CP_BCNS2ML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                    INDU,INDV,INDW,INDP,
     2                    XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     3                    GX_ML,GY_ML,GZ_ML,GX,GY,GZ,GV,
     4                    I_ML,J_ML,K_ML,KF_ML,KG_ML,
     $                    I_NS,J_NS,KF,KG,
     5                    WRK7,AD,WW,TT,CC,HH,HDEP,
     6                    UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                    CSEDI,ZBED,CSD_ML,ZBD_ML,
     $                    AK,EP,AK_ML,EP_ML,
     7                    MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     8                    IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     9                    KBOT_ML,KTOP_ML,IFLAG)
          CALL FTIMER(43,1)
C
          CALL FTIMER(44,0)
          CALL  CP_SNDNS2ML(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     1                      HH_ML,CSD_ML,ZBD_ML,BUF,
     2                      MX_ML,MY_ML,MZ_ML,
     3                      IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     *                                                      KTOP_ML,
     4                      NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     5                      IFLAG)
          CALL FTIMER(44,1)
        END IF
C
C----------------------------------------------------------------------
C     (3.3) 仮流速のZ方向成分の計算(BB)
C----------------------------------------------------------------------
C
        CALL FTIMER(39,0)
        CALL CLWEQ(UU,VV,WW,HU,HV,HW,PP,TT,CC,RHOW,XC,YC,ZC,
     $             YCOS,YCOSP,
     $             GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GZD,CMD,CDD,
     $             COE1D,COE2D,BB,TMU,HH,FF,FALLWZ,
     $             INDP,INDU,INDV,INDW,
     $             LLWALL,LLWALP,KF,KP,KG,WRK1,WRK2,WRK3,WRK4,WRK5,
     $             WRK6,WN,FRIC,WRK8,HDEP,UUBCN,VVBCN,WWBCN)
C ..... BB=WP
        CALL FTIMER(39,1)
        if(itrace.ne.0) write(6,*) 'clweq,istep,time=',istep,time
C
C     (3.4) 仮流速Wをコピー
C
        DO 130 K=1,MZ
        DO 130 J=1,MY
        DO 130 I=1,MX
          UU(I,J,K) = WRK7(I,J,K)
          VV(I,J,K) = AD  (I,J,K)
          WW(I,J,K) = BB  (I,J,K)
  130   CONTINUE
C
        DO 135 J=2,MYM
        DO 135 I=2,MXM
         IF(HX(I,J).EQ.HDEP(I,J)+EPSH.OR.HY(I,J).EQ.HDEP(I,J)+EPSH) THEN
            HH(I,J) = HDEP(I,J)+EPSH
          END IF
  135   CONTINUE
C
C ..... オーバーラップ領域部の流速を重み付け平均する
C
cc        CALL CP_UVML2NS(UU,VV,WRK7,AD,INDU,INDV,UUBCN,VVBCN,KF,
cc     $                  XC_REF,YC_REF,ZC)
C
C----------------------------------------------------------------------
C     (3.5) 圧力方程式の係数と右辺を計算する
C----------------------------------------------------------------------
        CALL FTIMER(46,0)
        CALL MKCOE2(AD,AL,AU,BB,AD0,AL0,UU,VV,WW,PP,RHOW,PATM,DPS,
     $              XC,YC,ZC,XCP,GX,GY,GZ,HH,INDU,INDV,INDP,KF,KG,KP)
        CALL FTIMER(46,1)
C
C----------------------------------------------------------------------
C     (3.6) 圧力補正量に関するポアソン方程式を解く
C----------------------------------------------------------------------
        CALL FTIMER(47,0)
        CALL ZERCLR(DP,MXYZ,0.0D0)
        CALL QBICGS(DP,AD,AL,AU,BB,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $              INDP,MZ)
        IF(NITER.LT.MXITER) THEN
          WRITE(LP,1010) ISTEP,TIME,DT,ITRMTX,RNRMTX,NITER
        END IF
        CALL FTIMER(47,1)
        if(itrace.ne.0) write(6,*) 'qbicgs,istep,time=',istep,time
C
C----------------------------------------------------------------------
C     (3.7) 流速と圧力を更新する
C----------------------------------------------------------------------
        CALL FTIMER(48,0)
        CALL UPUVWP(UU,VV,WW,PP,DP,HH,PATM,GV,GV0,GVD,CMD,XC,YC,ZC,
     $              INDP,INDU,INDV,INDW,KF,KG,KP)
C
        CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,1)
        CALL CLHUVW(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $              GZ0,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $              INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,2)
C
        CALL BCINLV(UU,VV,WW,FF,GX0,GY0,INDU,INDV,INDW,XC,YC,ZC,
     $              UUBCN,VVBCN,MZ_ML,INDU_ML,INDV_ML,
     $              I_NS,J_NS,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $              JNOR_ML,KBOT_ML,KTOP_ML,0)
C
        IF( LSURF.EQ.1 ) THEN
          CALL BCSUR2(UU,VV,WW,HU,HV,HW,GV,GX,GY,GZ,HH,KF,KP,KG,KH,
     $                INDU,INDV,INDP,XC,YC,ZC,YCOS,0)
        END IF
        CALL FTIMER(48,1)
C
  140   CONTINUE
        CALL FTIMER(34,1)
C
C ..... UU,VV,WW ...
        IFLAG = 4
        CALL FTIMER(74,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(74,1)
C
C----------------------------------------------------------------------
C     (4) 水面位置の計算
C----------------------------------------------------------------------
        IF( LSURF.EQ.1 ) THEN
C         TIME = (N+1)*DT+(1/2)*DT
          TIME = TIME+0.5D0*DTV
          CALL FTIMER(33,0)
          CALL SETTBL
          CALL FTIMER(33,1)
C
          CALL BCINLF(FF,ZC,INDP,HH,HDEP,HHBCN,0)
C
C ..... FF,HH
          CALL FTIMER(78,0)
          CALL CP_DSR_DC2(MX,MY,MZ,0,1,FF)
          CALL CP_DSR_DC2(MX,MY,1,0,1,HH)
          CALL FTIMER(78,1)
C
          DO 115 J = 1,MY
          DO 115 I = 1,MX
            HX(I,J) = HH(I,J)
            KH(I,J) = KF(I,J)
  115     CONTINUE
C
          CALL FTIMER(36,0)
          CALL CLSURF(HH,KF,XC,YC,ZC,YCOS,GV,GX,GY,GZ,UU,VV,WW,
     $                HU,HV,HW,PP,RHOW,HDEP,PATM,DPS,WX,WY,WRK1,
     $                INDU,INDV,INDP,KG,KP,0)
          if(itrace.ne.0) write(6,*) 'clsurf,istep,time=',istep,time
          CALL FTIMER(36,1)
C
Cmove(20130910)
          CALL SEABOT(HU,HV,UU,VV,FF,HH,HDEP,GX,GY,XC,YC,ZC,YCOSP,
     $                INDU,INDV,INDP,KF,KP,KG)
Cmove(20130910)
C
C ....... HH,KF,KH
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
C
C ..... 津波計算の開境界処理(自由透過条件)
          CALL FTIMER(49,0)
          IF(LTYPH.EQ.0) THEN
             CALL BOUND(HU,HV,FF,PP,RHOW,HH,HX,HDEP,XC,YC,ZC,
     $                  YCOSP,INDU,INDV,KF,KG)
C ..... 台風計算の開境界処理(気圧偏差+自由透過条件)
          ELSE
             CALL BOUNDS(HU,HV,FF,PP,RHOW,HH,HX,HDEP,PATM,XC,YC,ZC,
     $                   YCOSP,INDU,INDV,KF,KG)
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
C
          IF(IPARNT.GE.0) THEN
            CALL FTIMER(51,0)
            CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,
     1                       UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     $                       HH_ML,HDEP_ML,CSD_ML,ZBD_ML,BUF,
     2                       MX_ML,MY_ML,MZ_ML,
     3                       IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     $                                                       KTOP_ML,
     4                    NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IFLAG)
            CALL FTIMER(51,1)
C
            CALL FTIMER(52,0)
            CALL CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                      INDU,INDV,INDP,
     1                      XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     2                      GX_ML,GX,GY_ML,GY,
     3                      I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     4                      KF_ML,KG_ML,KF,KG,
     5                      UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     *                      HH_ML,HDEP_ML,HDEP,CSD_ML,ZBD_ML,
     6                      HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,
     *                      AKBCN,EPBCN,CSDBCN,ZBDBCN,
     8                      MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     9                      IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     A                      KBOT_ML,KTOP_ML,IFLAG)
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
     1                      INDU,INDV,INDW,INDP,
     2                      XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     3                      GX_ML,GY_ML,GZ_ML,GX,GY,GZ,GV,
     4                      I_ML,J_ML,K_ML,KF_ML,KG_ML,
     $                      I_NS,J_NS,KF,KG,
     5                      UU,VV,WW,TT,CC,HH,HDEP,
     6                      UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                      CSEDI,ZBED,CSD_ML,ZBD_ML,
     $                      AK,EP,AK_ML,EP_ML,
     7                      MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     8                      IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     9                      KBOT_ML,KTOP_ML,IFLAG)
            CALL FTIMER(53,1)
C
            CALL FTIMER(54,0)
            CALL  CP_SNDNS2ML(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     1                        HH_ML,CSD_ML,ZBD_ML,BUF,
     2                        MX_ML,MY_ML,MZ_ML,
     3                        IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     *                                                        KTOP_ML,
     4                        NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     5                        IFLAG)
            CALL FTIMER(54,1)
          END IF
C
          CALL KFSURF(HH,FF,ZC,INDU,INDV,INDP,KF,KP,KG)
C
Cmove(20130906)
C          CALL SEABOT(HU,HV,UU,VV,FF,HH,HDEP,GX,GY,XC,YC,ZC,YCOSP,
C     $                INDU,INDV,INDP,KF,KP,KG)
Cmove(20130906)
C
          CALL FTIMER(56,0)
c          write(*,*) ': bcsurf called at solve process'
          CALL BCSURF(PP,UU,VV,WW,HH,TT,QQ,QW,PATM,DPS,WX,WY,CD,
     $                XC,YC,ZC,YCOSP,GV,GX,GY,GZ,HDEP,RMMB,RMMF,
     $                INDU,INDV,INDP,KF,KP,KG,KH,1)
          CALL FTIMER(56,1)
C
        END IF
C
C ..... UU,VV ...
        IFLAG = 1
        CALL FTIMER(71,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(71,1)
C
C ..... WW,PP,WX,WY,CD,PATM,HH,KF,KP ...
        IFLAG=2
        CALL FTIMER(72,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(72,1)
C
C----------------------------------------------------------------------
C     (5) エネルギー式の計算
C----------------------------------------------------------------------
C        KH=KF(OLD),HX=HH(OLD)
C
         IF(LTEMP.EQ.1) THEN
c            write(*,*) '<<energy equation>>'
           CALL FTIMER(57,0)
           IF(LTURB.NE.4) THEN
              WRK6 = TMU
           ELSE
              DO 50 K=1,MZ
              DO 50 J=1,MY
              DO 50 I=1,MX
                 WRK6(I,J,K) = MIN(TMU(I,J,K),TVSVMX)
   50         CONTINUE
           END IF
C
           CALL CLENGY(TT,TN,HU,HV,HW,TMU,TMU,WRK6,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,GV,GX,GY,GZ,HH,HX,HDEP,QQ,
     $                 INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KH,KG,KP,
     $                 TTBCN,WRK1,WRK2,WRK3,WRK4,WRK5)
           if(itrace.ne.0) write(6,*) 'clengy,istep,time=',istep,time
           CALL FTIMER(57,1)
         END IF
C
C----------------------------------------------------------------------
C     (6) 濃度輸送式の計算
C----------------------------------------------------------------------
         IF(LCONC.EQ.1) THEN
c            write(*,*) '<<concentration equation>>'
           CALL FTIMER(58,0)
           IF(LTURB.NE.4) THEN
              WRK6 = TMU
           ELSE
              DO 60 K=1,MZ
              DO 60 J=1,MY
              DO 60 I=1,MX
                 WRK6(I,J,K) = MIN(TMU(I,J,K),TVSVMX)
   60         CONTINUE
           END IF
C
           CALL CLCONC(CC,CN,HU,HV,HW,TMU,TMU,WRK6,XC,YC,ZC,
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
     $                 HU,HV,HW,TMU,TMU,TMU,XC,YC,ZC,XCP,YCOS,YCOSP,
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
C          ポアソン方程式の係数を更新
           IF(NC.GT.0) THEN
              CALL MKCOE1(AD0,AL0,XC,YC,ZC,XCP,GX,GY,GZ,GV,GV0,GVD,CMD,
     $                    INDP,INDU,INDV,INDW)
           ENDIF
           ENDIF
C
         ELSEIF(MOFFLNSD.EQ.1) THEN
           CALL OUTOFFLNSD(HH,UU,VV,WW,1)
         ENDIF
C
C
C----------------------------------------------------------------------
C     (7) 乱流エネルギー・乱流エネルギー散逸式の計算
C----------------------------------------------------------------------
         IF(LTURB.EQ.2) THEN
c            write(*,*) '<<k-eps equation>>'
C ........ k-εモデル
           CALL FTIMER(12,0)
           CALL CLKEPS(AK,AKN,EP,EPN,UU,VV,WW,TMU,RHOW,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,HU,HV,HW,HX,HDEP,KH,
     $                 GV,GX,GY,GZ,HX,INDU,INDV,INDW,INDP,INDK,
     $                 LLWALL,LLWALP,KF,KG,KP,AKBCN,EPBCN,
     $                 WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,WRK8,IST0)
           CALL FTIMER(12,1)
         ELSE IF(LTURB.EQ.4) THEN
C ........ SGSモデル
c            write(*,*) '<<sgs equation>>'
           CALL FTIMER(12,0)
           CALL CLSGSM(AK,AKN,RL,UU,VV,WW,TMU,WX,WY,CD,RHOW,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,HU,HV,HW,HX,HDEP,KH,
     $                 GV,GX,GY,GZ,HX,INDU,INDV,INDW,INDP,INDK,
     $                 LLWALL,LLWALP,KF,KG,KP,AKBCN,
     $                 WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,WRK8,IST0)
           CALL FTIMER(12,1)
         END IF
C
C----------------------------------------------------------------------
C     (8) 乱流粘性係数の計算(LTUR=1:LESモデル,LTUR=2:k-εモデル)
C----------------------------------------------------------------------
         IF(LTURB.NE.0.AND.LTURB.NE.4) THEN
           CALL FTIMER(59,0)
           CALL CLTMU(AK,EP,UU,VV,WW,WRK1,WRK2,WRK3,XC,YC,ZC,
     $                INDU,INDV,INDW,INDP,KF,KP,KG,TMU)
           DO 370 K=1,MZ
           DO 370 J=1,MY
           DO 370 I=1,MX
              IF(K.LE.KF(I,J)) THEN
                 IF(INDP(I,J,K).NE.0) THEN
                    TMU(I,J,K) = MIN(TMU(I,J,K),TVSMAX)
                    TMU(I,J,K) = MAX(TMU(I,J,K),TVSMIN)
                 END IF
              END IF
  370      CONTINUE
           CALL FTIMER(59,1)
         END IF
C ..... PP,RHOW,TT,CC,TMU ...
        IFLAG=3
        CALL FTIMER(73,0)
        CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                  AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
        CALL FTIMER(73,1)
C
C
      CALL COM_MA2(HH,HDEP,HU,HV,0)
C
C----------------------------------------------------------------------
C     (9) 終了判定
C----------------------------------------------------------------------
C       TIME = (N+1)*DT
        IF(LSURF.EQ.1) TIME=TIME-0.5D0*DTV
        IF( LAIR.NE.1 ) THEN
           WRITE(LP,1000) ISTEP,TIME,DT,VELMAX,ITRMTX,RNRMTX
        ELSE
           WRITE(LP,2000) ISTEP,TIME,DT,VELMAX,ITRMTX,RNRMTX,
     $                    VELMAXAIR,ITRMTXAIR,RNRMTXAIR
        ENDIF
 1000   FORMAT('STEP=',I7,' TIME=',1P,E12.5,' DT=',E9.2,' VMAX=',E10.3,
     $         ' ITER=',I3,' !B-A*X!=',E8.1)
 1010   FORMAT('STEP=',I7,' TIME=',1P,E12.5,' DT=',E9.2,' ITER=',I3,
     $          ' !B-A*X!=',E8.1,' MXITER=')
 2000   FORMAT('STEP=',I7,' TIME=',1P,E12.5,' DT=',E9.2,' VMAX=',E10.3,
     $         ' ITER=',I3,' !B-A*X!=',E8.1,
     $          ' (AIR)VMAX=',E10.3,' ITER=',I3,' !B-A*X!=',E8.1)
C
C<<<<< (START) STOC-DM VERSION  <<<<<<<
      IF( TIME .GE. REND .OR. ISTEP.EQ.MAXSTP ) THEN
         IFINISH = 1
      ENDIF
      IF( NB_SD.GE.0 )THEN
      CALL COM_DRIFT2 (HDEP,HH,UU,VV,IDUMMY1,IDUMMY2)
                                       !  物理量(HT,HH,UU,VV)の通信
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
C     (10) 結果の出力
C----------------------------------------------------------------------
         CALL FTIMER(60,0)
         CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,PP,RHOW,
     $               TT,CC,AK,EP,TMU,FF,HH,
     $               SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $               EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $               INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $               HDEP,HHBCN,UUBCN,VVBCN,IDUMMY1,DUMMY1,
     $               UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,1)
         CALL FTIMER(60,1)
C
         IF( NB_SC.GT.0 ) THEN
            CALL CADMAS_SEND(UU,VV,WW,FF,ZC)
            CALL CADMAS_RECV
         ENDIF
  100 CONTINUE
C######################################################################
C#                                                                    #
C#       時間積分ループの終わり                                       #
C#                                                                    #
C######################################################################
  200 CONTINUE
C
      CALL FTIMER(30,1)
C
C
C----------------------------------------------------------------------
C     (11) 最終結果の出力とファイルのクローズ
C----------------------------------------------------------------------
      CALL FTIMER(60,0)
      CALL OUTPUT(XC_REF,YC_REF,ZC,GV0,GX0,GY0,GZ0,UU,VV,WW,PP,RHOW,
     $            TT,CC,AK,EP,TMU,FF,HH,
     $            SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $            EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $            INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $            HDEP,HHBCN,UUBCN,VVBCN,IDUMMY1,DUMMY1,
     $            UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,2)
      CALL FTIMER(60,1)
C
      CALL COM_MA2(HH,HDEP,HU,HV,-1)
C
C ... オフライン土砂移動計算用ファイルをクローズ
      IF(LSEDI.EQ.0 .AND. MOFFLNSD.EQ.1) CALL OUTOFFLNSD(HH,UU,VV,WW,2)
C
      DEALLOCATE(BF2)
C
      RETURN
      END
