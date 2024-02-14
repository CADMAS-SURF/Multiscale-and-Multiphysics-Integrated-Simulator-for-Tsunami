      SUBROUTINE CLWEQ(UU,VV,WW,HU,HV,HW,PP,TT,CC,RHOW,XC,YC,ZC,
     $                 YCOS,YCOSP,
     $                 GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GZD,CMD,CDD,
     $                 COE1D,COE2D,WP,TMU,HH,FF,FALLWZ,
     $                 INDP,INDU,INDV,INDW,
     $                 LLWALL,LLWALP,KF,KP,KG,FU,FV,FW,WWX,WWY,
     $                 DW,WN,FRIC,DP,HDEP,UUBCN,VVBCN,WWBCN)
C======================================================================
C     Z方向の運動量の保存式を圧力項を陽に解いて仮流速Wを計算する
C     ※仮流速はUPUVWPルーチンで圧力補正量を用いて補正される
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY),YCOSP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GVD(MX,MY,MZ),GZD(MX,MY,MZ)
      REAL(8),INTENT(IN)::CMD(MX,MY,MZ),CDD(MX,MY,MZ)
      REAL(8),INTENT(IN)::COE1D(MX,MY,MZ),COE2D(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      REAL(8),INTENT(IN)::GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WN(MX,MY,MZ),WP(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WWX(MX,MY,MZ),WWY(MX,MY,MZ),DW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),DP(MX,MY,MZ),FRIC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
      REAL(8),INTENT(INOUT)::WWBCN(NXY,MZ,4)
      REAL(8),INTENT(IN)::FALLWZ(MX,MY)
C
      REAL(8)::HH0,HH1,HH2,GV1,GV2,GVCM0,GVCM1,CDD1,CDD2,FRICD,FRICF
      REAL(8)::DP1,RESIZ,REXP,VABS,UUWK,VVWK,DHDTW,DW1,DIMP1,GRAV1,RHOZ
      INTEGER::I,J,K,M,N,IDIR
      INTEGER::IS,IE,JS,JE,KS,KE
C
C     局所配列
      REAL(8),ALLOCATABLE::TMUZ(:,:,:)
      INTEGER:: IERR
C
C
      ALLOCATE(TMUZ(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_CLWEQ',6150)
        WRITE(LP,*) 'CANNOT ALLOCATE TMUZ,...'
        CALL ABORT1('')
      END IF
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
C----------------------------------------------------------------------
C     (0) 密度を計算する
C----------------------------------------------------------------------
      CALL CLDENS(RHOW,TT,CC,GV,ZC,INDP,KF)
C
C----------------------------------------------------------------------
C     (1) 境界面上のWを設定する
C----------------------------------------------------------------------
      CALL BCWWXY(WWX,WWY,UU,VV,WW,TMU,XC,YC,ZC,KG,LLWALL,INDP,
     $            INDU,INDV,INDW,UUBCN,VVBCN,WWBCN)
C
C ... WWX,WWY
      CALL CP_DSR_DC2(MX,MY,MZ,1,1,WWX)
      CALL CP_DSR_DC2(MX,MY,MZ,2,1,WWY)
C
C
C----------------------------------------------------------------------
C     (2) コントロールボリューム界面の運動量流束を計算する(板境界は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(DW,MXYZ,0.0D0)
C
      CALL FLUXWX(FU,UU,WW,HU,WWX,FF,TMU,XC,ZC,GV,GX,GV0,GX0,
     $            INDU,INDW,KF)
C
      CALL FLUXWY(FV,VV,WW,HV,WWY,FF,TMU,YC,ZC,GV,GY,GV0,GY0,
     $            INDV,INDW,KF)
C
      CALL FLUXWZ(FW,WW,HW,TMUZ,FF,ZC,GZ,INDW,KF)
C
C ... FU,FV
      CALL CP_DSR_DC2(MX,MY,MZ,1,1,FU)
      CALL CP_DSR_DC2(MX,MY,MZ,2,1,FV)
C
C
C----------------------------------------------------------------------
C     (3) 板境界における剪断応力を計算する
C----------------------------------------------------------------------
      CALL FLUXWP(DW,UU,VV,WW,TMU,XC,YC,ZC,INDW,LLWALP)
C
C
C----------------------------------------------------------------------
C     (4) 仮流速のz方向成分を計算する(自由流入出境界上の点は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(WP,MXYZ,0.0D0)
      REXP = 1.0D0-RIMP
C
Cshu      DO 100 K=2,MZM
      DO 100 K=1,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
         IF( K.LE.KF(I,J)-1 ) THEN
         IF( INDW(I,J,K).GT.0 ) THEN
            HH1 = GV0(I,J,K)
            HH2 = FF(I,J,K+1)
            HH0 = HH1*ZC(8,K) + HH2*ZC(7,K)
            GV1 = GVD(I,J,K)*ZC(7,K) + GVD(I,J,K+1)*ZC(8,K)
            GVCM0=GVD(I,J,K  )+(1.0D0-GVD(I,J,K  ))*CMD(I,J,K  )
            GVCM1=GVD(I,J,K+1)+(1.0D0-GVD(I,J,K+1))*CMD(I,J,K+1)
            GV2 = GVCM0*ZC(7,K) + GVCM1*ZC(8,K)
C
            DP1 = GV1*HH0/RHO*(PP(I,J,K+1)-PP(I,J,K))*ZC(5,K)
C
            RHOZ = ZC(7,K)*RHOW(I,J,K)+ZC(8,K)*RHOW(I,J,K+1)
            if( rhoz/rho.gt.1.1d0 .or. rhoz/rho.lt.0.9d0 ) then
               write(*,*) 'istep,i,j,k,kf,rhow,rhow=',
     $            istep,i,j,k,kf(i,j),rhow(i,j,k),rhow(i,j,k+1)
            endif
            GRAV1 = GV1*HH0*GRAV*(RHOZ/RHO)
C
            UUWK= (UU(I  ,J,K)*ZC(7,K)+UU(I  ,J,K+1)*ZC(8,K))*0.5D0
     $          + (UU(I-1,J,K)*ZC(7,K)+UU(I-1,J,K+1)*ZC(8,K))*0.5D0
            VVWK= (VV(I,J  ,K)*ZC(7,K)+VV(I,J  ,K+1)*ZC(8,K))*0.5D0
     $          + (VV(I,J-1,K)*ZC(7,K)+VV(I,J-1,K+1)*ZC(8,K))*0.5D0
C
            VABS = DSQRT(UUWK**2+VVWK**2+WW(I,J,K)**2)
            IF(LDISS.EQ.0) THEN
               CDD1=MAX(CDD(I,J,K),CDD(I,J,K+1))
               FRICD=0.5D0*CDD1*ZC(5,K)*(1.0D0-GZD(I,J,K))
               FRICF=0.0D0
            ELSE
               CDD1=COE1D(I,J,K)*ZC(7,K)+COE1D(I,J,K+1)*ZC(8,K)
               CDD2=COE2D(I,J,K)*ZC(7,K)+COE2D(I,J,K+1)*ZC(8,K)
               FRICD=0.0D0
               FRICF=(CDD1+CDD2*GZD(I,J,K)*VABS)*GZD(I,J,K)
            ENDIF
            RESIZ = HH0*REXP*((FRIC(I,J,K)+FRICD)*VABS+FRICF)*WW(I,J,K)
C
            DHDTW = WW(I,J,K)
     $            *((( HU(I,J,K  )-HU(I-1,J,K  ) )*ZC(8,K)
     $            +  ( HU(I,J,K+1)-HU(I-1,J,K+1) )*ZC(7,K))*XC(6,I,J)
     $            + (( HV(I,J,K  )-HV(I,J-1,K  ) )*ZC(8,K)
     $            +  ( HV(I,J,K+1)-HV(I,J-1,K+1) )*ZC(7,K))
     $            *YC(6,J)/YCOS(J)
     $            +  ( HW(I,J,K+1)-HW(I,J,K-1)   )*0.5D0   *ZC(5,K) )
            IF(ISW(4).NE.0) DHDTW=0.0D0
C
            DW1 = DW(I,J,K) -DP1 + GRAV1 - RESIZ + DHDTW
     $          + ( FU(I,J,K) - FU(I-1,J,K) ) * XC(6,I,J)
     $          + ( FV(I,J,K) - FV(I,J-1,K) ) * YC(6,J)/YCOS(J)
     $          + ( FW(I,J,K+1) - FW(I,J,K) ) * ZC(5,K)
            DIMP1 = HH0*RIMP*((FRIC(I,J,K)+FRICD)*VABS+FRICF)
C
            IF(IFALLW.GT.0.AND.K.EQ.KF(I,J)-1) THEN
               DW1=DW1+FALLWZ(I,J)*XC(6,I,J)*ZC(5,K)
            ENDIF
C
            WP(I,J,K) = (WN(I,J,K) + DTV*DW1/HH0/GV2)/ (1.0D0+DTV*DIMP1)
C
C ......... GLHによる流速制限
            IF( ABS(WP(I,J,K)).GT.VVMAX ) THEN
               WP(I,J,K) = SIGN(VVMAX,WP(I,J,K))
            ENDIF
         ELSE
            WP(I,J,K) = WN(I,J,K)
         END IF
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (5) 自由流入出境界上の点にWPをコピー
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
C ...... 法線方向がZ方向の面
         IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 310 J=JS,JE
            DO 310 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
                  WP(I,J,K) = WP(I,J,K-1)
               ELSE
                  WP(I,J,K) = WP(I,J,K+1)
               END IF
  310       CONTINUE
         END IF
  300 CONTINUE
C
      DEALLOCATE(TMUZ)
C
      RETURN
      END
