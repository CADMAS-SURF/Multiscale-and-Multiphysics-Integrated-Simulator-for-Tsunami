      SUBROUTINE CLKMKH(RQ,RL,UU,VV,WW,RHOW,XC,YC,ZC,XCP,INDU,INDV,INDW,
     $                  INDP,KF,KP,KG,DKXX,DKYY,DKXY,DKM,DKH)
C----------------------------------------------------------------------
C     乱流粘性係数の計算
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MYCNST.h'
C
      REAL(8),INTENT(INOUT)::RQ(MX,MY,MZ),RL(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ),INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      REAL(8),INTENT(INOUT)::DKXX(MX,MY,MZ),DKYY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::DKXY(MX,MY,MZ),DKM(MX,MY,MZ),DKH(MX,MY,MZ)
C
      REAL(8)::DUDY1,DUDY2,DUDYJJ,DUDYJM,DUDY
      REAL(8)::DVDX1,DVDX2,DVDXII,DVDXIM,DVDX
      REAL(8)::DUDZIK,DUDZMK,DUDZKK,DVDZJK,DVDZMK,DVDZKK
      REAL(8)::DUDZIM,DUDZMM,DUDZKM,DVDZJM,DVDZMM,DVDZKM
      REAL(8)::DUVDZK,DUVDZM
      REAL(8)::SM1,SM2,SM,SH1,SH2,SH
      REAL(8)::DRHO,GAM1,GAM2,GAM3,GAM4,GAM5,GDCS,GHM,GHK
      REAL(8)::DXXMAX,DXYMAX,DYYMAX,DKMMAX,DKHMAX
CC      REAL(8)::RIC=0.195D0,RIFC=0.191D0
      REAL(8)::RIC=0.19D0,RIFC=0.19D0
      INTEGER::NFL=0
      INTEGER::I,J,K
C
      REAL(8),SAVE::RNUH,RNUV,RALV,RDFV
C
      IF(LTURB.EQ.3.AND.TIME.EQ.RSTART) THEN
        RNUH = ANUH
        RNUV = ANUV
        RALV = ALPV
        ANUH = 0.0D0
        ANUV = 0.0D0
        ALPH = 0.0D0
        ALPV = 0.0D0
        DIFH = 0.0D0
        DIFV = 0.0D0
      END IF
C
      CALL ZERCLR(DKM, MXYZ,0.0D0)
      CALL ZERCLR(DKH ,MXYZ,0.0D0)
      CALL ZERCLR(DKXX,MXYZ,0.0D0)
      CALL ZERCLR(DKYY,MXYZ,0.0D0)
      CALL ZERCLR(DKXY,MXYZ,0.0D0)
      IF(LTURB.NE.3) RETURN
C
C ... Mellor-Yamada2.5モデル
C
C ... 水平方向乱流粘性係数（セル中心）
C
      DXXMAX = -1.0D0
      DXYMAX = -1.0D0
      DYYMAX = -1.0D0
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
            DKXX(I,J,K) = 2.0D0*RCC*YC(4,J)*ABS(UU(I,J,K)-UU(I-1,J,K))
            DKYY(I,J,K) = 2.0D0*RCC*XC(4,I,J)*ABS(VV(I,J,K)-VV(I,J-1,K))
C
            DUDY1 = (UU(I-1,J+1,K)-UU(I-1,J,K))*YC(5,J)
            IF(INDU(I-1,J,K).LE.-1) DUDY1=0.0D0
            DUDY2 = (UU(I,J+1,K)-UU(I,J,K))*YC(5,J)
            IF(INDU(I,J,K).LE.-1) DUDY2=0.0D0
            DUDYJJ = 0.5D0*(DUDY1+DUDY2)
            DUDY1 = (UU(I-1,J,K)-UU(I-1,J-1,K))*YC(5,J-1)
            IF(INDU(I-1,J,K).LE.-1) DUDY1=0.0D0
            DUDY2 = (UU(I,J,K)-UU(I,J-1,K))*YC(5,J-1)
            IF(INDU(I,J,K).LE.-1) DUDY2=0.0D0
            DUDYJM = 0.5D0*(DUDY1+DUDY2)
            IF(INDV(I,J  ,K).LE.-1) DUDYJJ=0.0D0 
            IF(INDV(I,J-1,K).LE.-1) DUDYJM=0.0D0 
            DUDY = 0.5D0*(DUDYJJ+DUDYJM)
C
            DVDX1 = (VV(I+1,J-1,K)-VV(I,J-1,K))*XCP(5,I,J-1)
            IF(INDV(I,J-1,K).LE.-1) DVDX1=0.0D0
            DVDX2 = (VV(I+1,J,K)-VV(I,J,K))*XCP(5,I,J)
            IF(INDV(I,J,K).LE.-1) DVDX2=0.0D0
            DVDXII = 0.5D0*(DVDX1+DVDX2)
            DVDX1 = (VV(I,J-1,K)-VV(I-1,J-1,K))*XCP(5,I-1,J-1)
            IF(INDV(I,J-1,K).LE.-1) DVDX1=0.0D0
            DVDX2 = (VV(I,J,K)-VV(I-1,J,K))*XCP(5,I-1,J)
            IF(INDV(I,J,K).LE.-1) DVDX2=0.0D0
            DVDXIM = 0.5D0*(DVDX1+DVDX2)
            IF(INDU(I  ,J,K).LE.-1) DVDXII=0.0D0 
            IF(INDU(I-1,J,K).LE.-1) DVDXIM=0.0D0 
            DVDX = 0.5D0*(DVDXII+DVDXIM)
            DKXY(I,J,K) = RCC*XC(4,I,J)*YC(4,J)*(DUDY+DVDX)
C
            IF(DKXX(I,J,K).LT.TVSMIN) DKXX(I,J,K)=TVSMIN
            IF(DKXY(I,J,K).LT.TVSMIN) DKXY(I,J,K)=TVSMIN
            IF(DKYY(I,J,K).LT.TVSMIN) DKYY(I,J,K)=TVSMIN
            DXXMAX = MAX(DXXMAX,DKXX(I,J,K))
            DXYMAX = MAX(DXYMAX,DKXY(I,J,K))
            DYYMAX = MAX(DYYMAX,DKYY(I,J,K))
C
            DKXX(I,J,K) = MIN(DKXX(I,J,K),TVSMAX)
            DKYY(I,J,K) = MIN(DKYY(I,J,K),TVSMAX)
            DKXY(I,J,K) = MIN(DKXY(I,J,K),TVSMAX)
         END IF
  100 CONTINUE
C
C ... 鉛直方向乱流粘性係数（セル中心）
C     CS=1513m/s:音速（S=‰,T=20℃)
C
      GDCS  = (GRAV/1513.0D0)**2
      IF(NFL.EQ.0) GDCS=0.0D0
      DRHO = 1.0D0/RHO
      GAM1 = 3.0D0*RA2*RB2+18.0D0*RA1*RA2
      GAM2 = RA2*(1.0D0-6.0D0*RA1/RB1)
      GAM3 = 9.0D0*RA1*RA2
      GAM4 = 18.0D0*RA1*RA1+GAM3
      GAM5 = RA1*(1.0D0-3.0D0*RC1-6.0D0*RA1/RB1)
      DKMMAX = -1.0D0
      DKHMAX = -1.0D0
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
           GHK = (RL(I,J,K)/RQ(I,J,K))**2
     $        *(DRHO*(-GRAV)*(RHOW(I,J,K+1)-RHOW(I,J,K))*ZC(5,K)+GDCS)
           GHK = MIN(GHK,0.028D0)
           IF(K.EQ.KF(I,J)) GHK=(RL(I,J,K)/RQ(I,J,K))**2*GDCS
           SH1 = GAM2/(1.0D0-GAM1*GHK)
           SM1 = (SH1*GAM4*GHK+GAM5)/(1.0D0-GAM3*GHK)
           GHM = (RL(I,J,K)/RQ(I,J,K))**2
     $        *(DRHO*(-GRAV)*(RHOW(I,J,K)-RHOW(I,J,K-1))*ZC(5,K-1)+GDCS)
           IF(K.EQ.KG(I,J)) GHM=(RL(I,J,K)/RQ(I,J,K))**2*GDCS
           GHM = MIN(GHK,0.028D0)
           SH2 = GAM2/(1.0D0-GAM1*GHM)
           SM2 = (SH2*GAM4*GHM+GAM5)/(1.0D0-GAM3*GHM)
C
           SM = 0.5D0*(SM1+SM2)
           SH = 0.5D0*(SH1+SH2)
           DKM(I,J,K) = RL(I,J,K)*RQ(I,J,K)*SM
           DKH(I,J,K) = RL(I,J,K)*RQ(I,J,K)*SH
      if(istep.ge.10000.and.i.eq.61) then
      write(8,8) k,rq(i,j,k),rl(i,j,k),dkm(i,j,k),dkh(i,j,k)
 8    format('k,q,l,dkm,dkh=',i5,1p,4e13.5)
      write(8,*) 'sm1-3=',sm1,sm2,sm
      write(8,*) 'sh1-3=',sh1,sh2,sh
      write(8,*) '(l/q)**2,gdcs',(RL(I,J,K)/RQ(I,J,K))**2,gdcs
      write(8,*) '1ghk,m',(RL(I,J,K)/RQ(I,J,K))**2
     $         *(DRHO*(-GRAV)*(RHOW(I,J,K+1)-RHOW(I,J,K))*ZC(5,K)+GDCS),
     $                      (RL(I,J,K)/RQ(I,J,K))**2
     $         *(DRHO*(-GRAV)*(RHOW(I,J,K)-RHOW(I,J,K-1))*ZC(5,K)+GDCS)
      write(8,*) '2ghk,m',ghk,ghm
      write(8,*) 'dkm.dkh=',dkm(i,j,k),dkh(i,j,k)
      endif
           IF(DKM(I,J,K).LT.TVSMIN) DKM(I,J,K)=TVSMIN
           IF(DKH(I,J,K).LT.TVSMIN) DKH(I,J,K)=TVSMIN
C
           DKMMAX = MAX(DKMMAX,DKM(I,J,K))
           DKHMAX = MAX(DKHMAX,DKH(I,J,K))
C
           DKM(I,J,K) = MIN(DKM(I,J,K),TVSVMX)
           DKH(I,J,K) = MIN(DKH(I,J,K),TVSVMX)
      if(istep.ge.10000.and.i.eq.61) then
      write(8,*) 'dkm.dkh=',dkm(i,j,k),dkh(i,j,k)
      endif
         END IF
  200 CONTINUE
C
      write(6,1) DXXMAX,DXYMAX,DYYMAX,DKMMAX,DKHMAX
 1    format('MAX(DXX,DXY,DYY,DKM,DKH)=',1P,5d13.5)
C
      RETURN
      END
