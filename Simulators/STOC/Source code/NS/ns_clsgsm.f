      SUBROUTINE CLSGSM(AK,AKN,RL,UU,VV,WW,TMU,WX,WY,CD,RHOW,XC,YC,ZC,
     $                  XCP,YCOS,YCOSP,HU,HV,HW,HX,HDEP,KH,
     $                  GV,GX,GY,GZ,HH,INDU,INDV,INDW,INDP,INDK,
     $                  LLWALL,LLWALP,KF,KG,KP,AKBCN,
     $                  FU,FV,FW,WRK4,WRK5,WRK6,GS,GT,IST0)
C======================================================================
C     １方程式型LES乱流モデルを解く
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       AK: 新しい時刻のk,AKN:古い時刻のK
C       TMU:乱流動粘性係数 
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MYCNST.h'
C
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),AKN(MX,MY,MZ),RL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),YCOS(MY),YCOSP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDK(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY)
      REAL(8),INTENT(INOUT)::AKBCN(NXY,MZ,4)
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK4(MX,MY,MZ),WRK5(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK6(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GS(MX,MY,MZ),GT(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::IST0
C
      REAL(8)::AKMAX=1.0D+6
      REAL(8)::TMUMAX
      INTEGER::NFL=0
      INTEGER::I,J,K,ITM,JTM,KTM
C
      IF(ISTEP.EQ.IST0) THEN
        CALL WLCONT(INDK,INDP,LLWALL,LLWALP)
      END IF
C
      CALL CLKEGN(UU,VV,WW,TMU,RHOW,WRK4,WRK5,WRK6,INDU,INDV,INDW,INDP,
     $            XC,YC,ZC,KF,KP,KG,GS,GT)
C
      CALL CLSGE(AK,AKN,RL,HU,HV,HW,TMU,WX,WY,CD,GS,GT,
     $           XC,YC,ZC,XCP,YCOS,GV,GX,GY,GZ,HH,HX,HDEP,
     $           INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KH,KG,KP,
     $           AKBCN,FU,FV,FW,WRK4,WRK5,WRK6)
C
      IF(NFL.NE.0) THEN
C ..... 対数則条件処理
C       GS=1.0D0(WORK配列) 
        CALL ZERCLR(GS,MXYZ,1.0D0)
        CALL WALBND(AK,GS,UU,VV,WW,TMU,INDP,INDK,XC,YC,ZC,
     $              LLWALL,LLWALP,KF)
      END IF
C
C ... 乱流動粘性係数を計算
C
      CALL ZERCLR(TMU,MXYZ,0.0D0)
      TMUMAX = - 1.0D0
      DO 100 K=1,MZ
      DO 100 J=1,MY
      DO 100 I=1,MX
        IF(LSURF.EQ.1.AND.K.GT.KF(I,J)) AK(I,J,K)=0.0D0
        IF(LSURF.NE.1.OR.K.LE.KF(I,J)) THEN
          IF(INDP(I,J,K).NE.0) THEN
            IF(AK(I,J,K).GE.AKMAX) AK(I,J,K)=AKMAX
            IF(AK(I,J,K).LT.AKMIN) AK(I,J,K)=AKMIN
            TMU(I,J,K) = CSMG*RL(I,J,K)*SQRT(AK(I,J,K)) 
cccccccc
      if(tmu(i,j,k).gt.tmumax) then
        itm=i
        jtm=j
        ktm=k
        tmumax=tmu(i,j,k)
      endif
            TMU(I,J,K) = MIN(TMU(I,J,K),TVSMAX)
            TMU(I,J,K) = MAX(TMU(I,J,K),TVSMIN) 
          END IF
        END IF
  100 CONTINUE    
ccccccccccc
c      write(8,8) istep,itm,jtm,ktm,tmumax,tmu(itm,jtm,ktm)
 8    format('tmumax istep,i,j,k=',i10,3i4,'  tmumax,tmulmy=',1p,2d12.5)
cccccccccc
C
      RETURN
      END
