      SUBROUTINE CLVEQ(UU,VV,WW,HU,HV,HW,PP,WX,WY,CD,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,YSIN,YSINP,
     $                 GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GYD,
     $                 CMD,CDD,COE1D,COE2D,
     $                 VP,TMUX,TMUY,TMUZ,TMUBW,HH,FF,FALLWY,DHY,
     $                 INDP,INDU,INDV,INDW,
     $                 LLWALL,LLWALP,LLWALB,KF,KP,KG,FU,FV,FW,VVX,VVZ,
     $                 DV,VN,DP,FRIC,DIMP,HDEP,AMNG,PATM,HY,
     $                 UUBCN,VVBCN)
C======================================================================
C     Y方向の運動量の保存式を陽に解いて流速Vを計算する
C     TMUX=DKXY,TMUY=DKYY,TMUZ=DKM
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'GRID.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HY(MX,MY)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY),YSIN(MY),YSINP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GVD(MX,MY,MZ),GYD(MX,MY,MZ)
      REAL(8),INTENT(IN)::CMD(MX,MY,MZ),CDD(MX,MY,MZ)
      REAL(8),INTENT(IN)::COE1D(MX,MY,MZ),COE2D(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      REAL(8),INTENT(IN)::GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::VP(MX,MY,MZ),VN(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::VVX(MX,MY,MZ),VVZ(MX,MY,MZ),DV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUX(MX,MY,MZ),TMUY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUZ(MX,MY,MZ),FRIC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUBW(MX,MY)
      REAL(8),INTENT(INOUT)::DIMP(MX,MY,MZ),DP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
      REAL(8),INTENT(IN)::FALLWY(MX,MY),DHY(MX,MY)
C
      REAL(8)::HH0,HH1,HH2,DH1,GV1,GV2,GVCM0,GVCM1,CDD1,CDD2,FRICD,FRICF
      REAL(8)::CORIU,DP1,RESIY,REXP,UUVK,WWVK,VABS,DHDTV,DV1,DIMP1
      INTEGER::I,J,K,M,N,IDIR
      INTEGER::IS,IE,JS,JE,KS,KE
C
      INTEGER,PARAMETER:: IDP1CR=0
C
C     局所配列
      REAL(8),ALLOCATABLE::TMUX2(:,:,:),TMUY2(:,:,:)
      INTEGER:: IERR
C
C
      ALLOCATE(TMUX2(MX,MY,MZ),TMUY2(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('ML_CLUEQ',6120)
        WRITE(LP,*) 'CANNOT ALLOCATE TMUX2,...'
        CALL ABORT1('')
      END IF
      TMUX2(:,:,:)=0.D0
      TMUY2(:,:,:)=0.D0
C
C
      DO 50 K=1,MZ
      DO 50 J=1,MY
      DO 50 I=1,MX
        VN(I,J,K) = VV(I,J,K)
   50 CONTINUE
C
      TMUX2=TMUX
      TMUY2=TMUY
      IF( LBRKW.GT.0 ) THEN
         DO K=2,MZM
         DO J=2,MYM
         DO I=2,MXM
            IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
               TMUX2(I,J,K)=TMUX(I,J,K)+TMUBW(I,J)
               TMUY2(I,J,K)=TMUY(I,J,K)+TMUBW(I,J)
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDIF
C
C----------------------------------------------------------------------
C     (1) 境界面上のVを設定する
C----------------------------------------------------------------------
C  .. MY2.5では対数則(TMUZ)を使用せず ..
      CALL BCVVXZ(VVX,VVZ,UU,VV,WW,TMUZ,XCP,YC,ZC,LLWALL,INDP,
     $            INDU,INDV,INDW,KG,GV,HH,UUBCN,VVBCN)
C
C ... VVX,VVZ
      CALL FTIMER(77,0)
      CALL CP_DSR_DC2(MX,MY,MZ,1,1,VVX)
      CALL CP_DSR_DC2(MX,MY,MZ,3,1,VVZ)
      CALL FTIMER(77,1)
C
C
C----------------------------------------------------------------------
C     (2) コントロールボリューム界面の運動量流束を計算する(板境界は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(DIMP,MXYZ,0.0D0)
      CALL ZERCLR(DV,MXYZ,0.0D0)
C
      CALL FLUXVX(FU,UU,VV,HU,VVX,FF,TMUX2,XC,YC,XCP,GV,GX,GV0,GX0,
     $            INDU,INDV,KF)
C
      CALL FLUXVY(FV,VV,HV,TMUY2,FF,YC,GV,GY,GV0,GY0,INDV,LLWALB,KF)
C
      CALL FLUXVZ(FW,UU,VV,WW,HW,VVZ,FF,TMUZ,YC,ZC,GV,GZ,GV0,HH,
     $            HDEP,DV,DIMP,AMNG,WX,WY,CD,INDV,INDW,KG,KP,KF)
C
C ... FU,FW
      CALL FTIMER(77,0)
      CALL CP_DSR_DC2(MX,MY,MZ,1,1,FU)
      CALL CP_DSR_DC2(MX,MY,MZ,3,1,FW)
      CALL FTIMER(77,1)
C
C
C----------------------------------------------------------------------
C     (3) 板境界における剪断応力を計算する
C----------------------------------------------------------------------
C  .. MY2.5ではスリップのみ ..
      IF(LTURB.NE.3) THEN
        CALL FLUXVP(DV,UU,VV,WW,TMUZ,XC,YC,ZC,INDV,LLWALP)
      END IF
C
C
C----------------------------------------------------------------------
C     (4) 圧力差を計算する
C----------------------------------------------------------------------
      CALL CLDP(DP,PP,PATM,FF,GV,GX,GY,ZC,INDU,INDV,LLWALB,KF,KG,2)
C
C
C----------------------------------------------------------------------
C     (5) 流速のy方向成分を計算する(自由流入出境界上の点は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(VP,MXYZ,0.0D0)
      REXP = 1.0D0-RIMP
C
      DO 100 K=2,MZM
      DO 100 J=1,MYM
      DO 100 I=2,MXM
C
         IF( K.LE.KF(I,J) .OR. K.LE.KF(I,J+1) ) THEN
         IF( INDV(I,J,K).GT.0.AND.J.GE.2 ) THEN
            HH1 = MAX(FF(I,J  ,K)-1.0D0+GV0(I,J  ,K),0.0D0)
            HH2 = MAX(FF(I,J+1,K)-1.0D0+GV0(I,J+1,K),0.0D0)
            HH0 = HH1*YC(8,J) + HH2*YC(7,J)
            GV1 = GVD(I,J,K)*YC(7,J) + GVD(I,J+1,K)*YC(8,J)
            GVCM0=GVD(I,J  ,K)+(1.0D0-GVD(I,J  ,K))*CMD(I,J  ,K)
            GVCM1=GVD(I,J+1,K)+(1.0D0-GVD(I,J+1,K))*CMD(I,J+1,K)
            GV2 = GVCM0*YC(7,J) + GVCM1*YC(8,J)
C
            DP1 = GV1*HH0/RHO*DP(I,J,K)*YC(5,J)
C
C ......... 水位が高い側の層厚が0のときに圧力差を発生させない
            IF( IDP1CR.EQ.1 ) THEN
            IF( HH(I,J).GT.HH(I,J+1) ) THEN
               IF( HH1*ZC(4,K).LE.EPSH ) DP1 =0.0D0
            ELSE
               IF( HH2*ZC(4,K).LE.EPSH ) DP1 =0.0D0
            ENDIF
            ENDIF
C
            UUVK= (UU(I  ,J,K)*YC(7,J)+UU(I  ,J+1,K)*YC(8,J))*0.5D0
     $          + (UU(I-1,J,K)*YC(7,J)+UU(I-1,J+1,K)*YC(8,J))*0.5D0
            WWVK= (WW(I,J,K  )*YC(7,J)+WW(I,J+1,K  )*YC(8,J))*0.5D0
     $          + (WW(I,J,K-1)*YC(7,J)+WW(I,J+1,K-1)*YC(8,J))*0.5D0
            CORIU = GV1*HH0*2.0D0*CEARTH*YSINP(J)*UUVK
C
            VABS = DSQRT(UUVK**2+VV(I,J,K)**2+WWVK**2)
            IF(LDISS.EQ.0) THEN
               CDD1=MAX(CDD(I,J,K),CDD(I,J+1,K))
               FRICD=0.5D0*CDD1*YC(5,J)*(1.0D0-GYD(I,J,K))
               FRICF=0.0D0
            ELSE
               CDD1=COE1D(I,J,K)*YC(7,J)+COE1D(I,J+1,K)*YC(8,J)
               CDD2=COE2D(I,J,K)*YC(7,J)+COE2D(I,J+1,K)*YC(8,J)
               FRICD=0.0D0
               FRICF=(CDD1+CDD2*GYD(I,J,K)*VABS)*GYD(I,J,K)
            ENDIF
            RESIY = HH0*REXP*((FRIC(I,J,K)+FRICD)*VABS+FRICF)*VV(I,J,K)
C
            DHDTV = VV(I,J,K)
     $            *((( HU(I,J  ,K)-HU(I-1,J  ,K) )*YC(8,J)
     $            +  ( HU(I,J+1,K)-HU(I-1,J+1,K) )*YC(7,J))*XCP(6,I,J)
     $            +  ( HV(I,J+1,K)-HV(I,J-1,K)   )*0.5D0
     $            *YC(5,J)/YCOSP(J)
     $            + (( HW(I,J  ,K)-HW(I,J  ,K-1) )*YC(8,J)
     $            +  ( HW(I,J+1,K)-HW(I,J+1,K-1) )*YC(7,J))*ZC(6,K) )
            IF(ISW(4).NE.0) DHDTV=0.0D0
C
            DV1 = DV(I,J,K) -DP1 - CORIU - RESIY + DHDTV
     $          + ( FU(I,J,K) - FU(I-1,J,K) ) * XCP(6,I,J)
     $          + ( FV(I,J+1,K) - FV(I,J,K) ) * YC(5,J)/YCOSP(J)
     $          + ( FW(I,J,K) - FW(I,J,K-1) ) * ZC(6,K)
            DIMP1 =DIMP(I,J,K)+HH0*RIMP*((FRIC(I,J,K)+FRICD)*VABS+FRICF)
C
            IF(IFALLW.GT.0) THEN
               DV1=DV1+FALLWY(I,J)*HH0*YC(5,J)/DHY(I,J)
            ENDIF
C
            DH1 = MAX(HH0,GZH*ZC(6,K))*GV2
            IF( HH0*GV2.LT.DH1 .AND. INDV(I,J,K-1).GT.0 ) THEN
               VP(I,J,K) = VP(I,J,K-1)
            ELSE
               VP(I,J,K) = (VN(I,J,K)+DTV*DV1/DH1)/(1.0D0+DTV*DIMP1)
C
C ............ GLHによる流速制限
               IF( HH0*ZC(4,K).LT.GLH.AND.ABS(VP(I,J,K)).GT.VVMAX ) THEN
                  VP(I,J,K) = SIGN(VVMAX,VP(I,J,K))
               ENDIF
            END IF
         ELSE
            VP(I,J,K) = VN(I,J,K)
         END IF
C
         IF( ISW(3).NE.0 .AND. K.EQ.MAX(KF(I,J),KF(I,J+1))
     $      .AND. INDV(I,J,K-1).EQ.1 ) THEN
               VP(I,J,K) = VP(I,J,K-1)
         END IF
C
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (6) 流量制限(遡上および防潮堤)
C----------------------------------------------------------------------
      CALL RUNUPY(VP,FF,HH,HDEP,GV,GX,GY,XC,YC,ZC,INDV,LLWALB,KF,KG)
C
C
C----------------------------------------------------------------------
C     (7) 自由流入出境界上の点にVPをコピー
C----------------------------------------------------------------------
      DO 300 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ...... 法線方向がY方向の面
         IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 310 K=KS,KE
            DO 310 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
                  VP(I,J,K) = VP(I,J-1,K)*GY(I,J-1,K)/GY(I,J,K)
               ELSE
                  VP(I,J,K) = VP(I,J+1,K)*GY(I,J+1,K)/GY(I,J,K)
               END IF
  310       CONTINUE
         END IF
  300 CONTINUE
C
C
      DEALLOCATE(TMUX2,TMUY2)
C
      RETURN
      END
