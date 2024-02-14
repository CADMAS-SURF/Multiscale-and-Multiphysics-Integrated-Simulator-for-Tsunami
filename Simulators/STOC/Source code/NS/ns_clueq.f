      SUBROUTINE CLUEQ(UU,VV,WW,HU,HV,HW,PP,WX,WY,CD,XC,YC,ZC,
     $                 XCP,YCOS,YCOSP,YSIN,YSINP,
     $                 GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GXD,CMD,CDD,
     $                 COE1D,COE2D,UP,TMU,TMUBW,HH,FF,FALLWX,DHX,
     $                 INDP,INDU,INDV,INDW,
     $                 LLWALL,LLWALP,LLWALB,KF,KP,KG,FU,FV,FW,UUY,UUZ,
     $                 DU,UN,DP,FRIC,DIMP,HDEP,AMNG,PATM,HX,
     $                 UUBCN,VVBCN)
C======================================================================
C     X方向の運動量の保存式を圧力項を陽に解いて仮流速Uを計算する
C     ※仮流速はUPUVWPルーチンで圧力補正量を用いて補正される
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
      INCLUDE 'MYCNST.h'
      INCLUDE 'GRID.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY),YSIN(MY),YSINP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GVD(MX,MY,MZ),GXD(MX,MY,MZ)
      REAL(8),INTENT(IN)::CMD(MX,MY,MZ),CDD(MX,MY,MZ)
      REAL(8),INTENT(IN)::COE1D(MX,MY,MZ),COE2D(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      REAL(8),INTENT(IN)::GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UN(MX,MY,MZ),UP(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UUY(MX,MY,MZ),UUZ(MX,MY,MZ),DU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),FRIC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUBW(MX,MY)
      REAL(8),INTENT(INOUT)::DIMP(MX,MY,MZ),DP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
      REAL(8),INTENT(IN)::FALLWX(MX,MY),DHX(MX,MY)
C
      REAL(8)::HH0,HH1,HH2,DH1,GV1,GV2,GVCM0,GVCM1,CDD1,CDD2,FRICD,FRICF
      REAL(8)::CORIV,DP1,RESIX,REXP,VABS,VVUK,WWUK,DHDTU,DU1,DIMP1
      INTEGER::I,J,K,M,N,IDIR
      INTEGER::IS,IE,JS,JE,KS,KE
C
      INTEGER,PARAMETER:: IDP1CR=0
C
C     局所配列
      REAL(8),ALLOCATABLE::TMUX(:,:,:),TMUZ(:,:,:)
      INTEGER:: IERR
C
C
      ALLOCATE(TMUX(MX,MY,MZ),TMUZ(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_CLUEQ',6130)
        WRITE(LP,*) 'CANNOT ALLOCATE TMUX,...'
        CALL ABORT1('')
      END IF
      TMUX(:,:,:)=0.D0
      TMUZ(:,:,:)=0.D0
C
C ... 鉛直方向粘性係数TMUZを設定
      IF(LTURB.NE.4) THEN
        TMUZ = TMU
      ELSE
        DO 200 K=1,MZ
        DO 200 J=1,MY
        DO 200 I=1,MX
          TMUZ(I,J,K) = MIN(TMU(I,J,K),TVSVMX)
  200   CONTINUE    
      END IF
C
      TMUX=TMU
      IF( LBRKW.GT.0 ) THEN
         DO K=2,MZM
         DO J=2,MYM
         DO I=2,MXM
            IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
               TMUX(I,J,K)=TMU(I,J,K)+TMUBW(I,J)
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDIF
C
C----------------------------------------------------------------------
C     (1) 境界面上のUを設定する
C----------------------------------------------------------------------
      CALL BCUUYZ(UUY,UUZ,UU,VV,WW,TMUX,XC,YC,ZC,LLWALL,INDP,
     $            INDU,INDV,INDW,KG,GV,HH,UUBCN,VVBCN)
C
C ... UUY,UUZ
      CALL CP_DSR_DC2(MX,MY,MZ,2,1,UUY)
      CALL CP_DSR_DC2(MX,MY,MZ,3,1,UUZ)
C
C
C----------------------------------------------------------------------
C     (2) コントロールボリューム界面の運動量流束を計算する(板境界は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(DIMP,MXYZ,0.0D0)
      CALL ZERCLR(DU,MXYZ,0.0D0)
C
      CALL FLUXUX(FU,UU,HU,TMUX,FF,XC,GV,GX,GV0,GX0,INDU,LLWALB,KF)
C
      CALL FLUXUY(FV,UU,VV,HV,UUY,FF,TMUX,XC,YC,XCP,GV,GY,GV0,GY0,
     $            INDU,INDV,KF)
C
      CALL FLUXUZ(FW,UU,VV,WW,HW,UUZ,FF,TMUZ,XC,ZC,GV,GZ,GV0,HH,
     $            HDEP,DU,DIMP,AMNG,WX,WY,CD,INDU,INDW,KG,KP,KF)
C
C ... FV,FW
      CALL CP_DSR_DC2(MX,MY,MZ,2,1,FV)
      CALL CP_DSR_DC2(MX,MY,MZ,3,1,FW)
C
C
C----------------------------------------------------------------------
C     (3) 板境界における剪断応力を計算する
C----------------------------------------------------------------------
      CALL FLUXUP(DU,UU,VV,WW,TMUX,XC,YC,ZC,INDU,LLWALP)
C
C
C----------------------------------------------------------------------
C     (4) 圧力差を計算する
C----------------------------------------------------------------------
      CALL CLDP(DP,PP,PATM,FF,GV,GX,GY,ZC,INDU,INDV,LLWALB,KF,KG,1)
C
C
C----------------------------------------------------------------------
C     (5) 仮流速のx方向成分を計算する(自由流入出境界上の点は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(UP,MXYZ,0.0D0)
      REXP = 1.0D0-RIMP
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=1,MXM
C
         IF( K.LE.KF(I,J) .OR. K.LE.KF(I+1,J) ) THEN
         IF( INDU(I,J,K).GT.0.AND.I.GE.2 ) THEN
            HH1 = MAX(FF(I  ,J,K)-1.0D0+GV0(I  ,J,K),0.0D0)
            HH2 = MAX(FF(I+1,J,K)-1.0D0+GV0(I+1,J,K),0.0D0)
            HH0 = HH1*XC(8,I,J) + HH2*XC(7,I,J)
            GV1 = GVD(I,J,K)*XC(7,I,J) + GVD(I+1,J,K)*XC(8,I,J)
            GVCM0=GVD(I  ,J,K)+(1.0D0-GVD(I  ,J,K))*CMD(I  ,J,K)
            GVCM1=GVD(I+1,J,K)+(1.0D0-GVD(I+1,J,K))*CMD(I+1,J,K)
            GV2 = GVCM0*XC(7,I,J) + GVCM1*XC(8,I,J)
C
            DP1 = GV1*HH0/RHO*DP(I,J,K)*XC(5,I,J)
C
C ......... 水位が高い側の層厚が0のときに圧力差を発生させない
            IF( IDP1CR.EQ.1 ) THEN
            IF( HH(I,J).GT.HH(I+1,J) ) THEN
               IF( HH1*ZC(4,K).LE.EPSH ) DP1 =0.0D0
            ELSE
               IF( HH2*ZC(4,K).LE.EPSH ) DP1 =0.0D0
            ENDIF
            ENDIF
C
            VVUK= (VV(I,J  ,K)*XCP(7,I,J  )+VV(I+1,J  ,K)*XCP(8,I,J  ))
     $          *0.5D0
     $          + (VV(I,J-1,K)*XCP(7,I,J-1)+VV(I+1,J-1,K)*XCP(8,I,J-1))
     $          *0.5D0
            WWUK= (WW(I,J,K  )*XC(7,I,J)+WW(I+1,J,K  )*XC(8,I,J))*0.5D0
     $          + (WW(I,J,K-1)*XC(7,I,J)+WW(I+1,J,K-1)*XC(8,I,J))*0.5D0
            CORIV = GV1*HH0*2.0D0*CEARTH*YSIN(J)*VVUK
C
            VABS = DSQRT(UU(I,J,K)**2+VVUK**2+WWUK**2)
            IF(LDISS.EQ.0) THEN
               CDD1=MAX(CDD(I,J,K),CDD(I+1,J,K))
               FRICD=0.5D0*CDD1*XC(5,I,J)*(1.0D0-GXD(I,J,K))
               FRICF=0.0D0
            ELSE
               CDD1=COE1D(I,J,K)*XC(7,I,J)+COE1D(I+1,J,K)*XC(8,I,J)
               CDD2=COE2D(I,J,K)*XC(7,I,J)+COE2D(I+1,J,K)*XC(8,I,J)
               FRICD=0.0D0
               FRICF=(CDD1+CDD2*GXD(I,J,K)*VABS)*GXD(I,J,K)
            ENDIF
            RESIX = HH0*REXP*((FRIC(I,J,K)+FRICD)*VABS+FRICF)*UU(I,J,K)
C
            DHDTU = UU(I,J,K)
     $            *( ( HU(I+1,J,K)-HU(I-1,J,K)   )*0.5D0   *XC(5,I,J)
     $            + (( HV(I  ,J,K)-HV(I  ,J-1,K) )*XC(8,I,J)
     $            +  ( HV(I+1,J,K)-HV(I+1,J-1,K) )*XC(7,I,J))
     $            *YC(6,J)/YCOS(J)
     $            + (( HW(I  ,J,K)-HW(I  ,J,K-1) )*XC(8,I,J)
     $            +  ( HW(I+1,J,K)-HW(I+1,J,K-1) )*XC(7,I,J))*ZC(6,K) )
            IF(ISW(4).NE.0) DHDTU=0.0D0
C
            DU1 = DU(I,J,K) -DP1 + CORIV - RESIX + DHDTU
     $          + ( FU(I+1,J,K) - FU(I,J,K) ) * XC(5,I,J)
     $          + ( FV(I,J,K) - FV(I,J-1,K) ) * YC(6,J)/YCOS(J)
     $          + ( FW(I,J,K) - FW(I,J,K-1) ) * ZC(6,K)
            DIMP1 =DIMP(I,J,K)+HH0*RIMP*((FRIC(I,J,K)+FRICD)*VABS+FRICF)
C
            IF(IFALLW.GT.0) THEN
               DU1=DU1+FALLWX(I,J)*HH0*XC(5,I,J)/DHX(I,J)
            ENDIF
C
            DH1 = MAX(HH0,GZH*ZC(6,K))*GV2
            IF( HH0*GV2.LT.DH1 .AND. INDU(I,J,K-1).GT.0 ) THEN
               UP(I,J,K) = UP(I,J,K-1)
            ELSE
               UP(I,J,K) = (UN(I,J,K)+DTV*DU1/DH1)/(1.0D0+DTV*DIMP1)
C
C ............ GLHによる流速制限
               IF( HH0*ZC(4,K).LT.GLH.AND.ABS(UP(I,J,K)).GT.VVMAX ) THEN
                  UP(I,J,K) = SIGN(VVMAX,UP(I,J,K))
               ENDIF
            END IF
         ELSE
            UP(I,J,K) = UN(I,J,K)
         END IF
C
         IF( ISW(3).NE.0 .AND. K.EQ.MAX(KF(I,J),KF(I+1,J))
     $      .AND. INDU(I,J,K-1).EQ.1 ) THEN
               UP(I,J,K) = UP(I,J,K-1)
         END IF
C
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (6) 流量制限(遡上および防潮堤)
C----------------------------------------------------------------------
      CALL RUNUPX(UP,FF,HH,HDEP,GV,GX,GY,XC,YC,ZC,INDU,LLWALB,KF,KG)
C
C
C----------------------------------------------------------------------
C     (7) 自由流入出境界上の点にUPをコピー
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
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 310 K=KS,KE
            DO 310 J=JS,JE
               IF( INDP(I,J,K).GT.0 ) THEN
                  UP(I,J,K) = UP(I-1,J,K)*GX(I-1,J,K)/GX(I,J,K)
               ELSE
                  UP(I,J,K) = UP(I+1,J,K)*GX(I+1,J,K)/GX(I,J,K)
               END IF
  310       CONTINUE
         END IF
  300 CONTINUE
C
      DEALLOCATE(TMUX,TMUZ)
C
      RETURN
      END
