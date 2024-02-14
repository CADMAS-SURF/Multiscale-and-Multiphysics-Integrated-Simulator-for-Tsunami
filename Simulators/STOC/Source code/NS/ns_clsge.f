      SUBROUTINE CLSGE(AK,AKN,RL,HU,HV,HW,TMU,WX,WY,CD,GS,GT,
     $                 XC,YC,ZC,XCP,YCOS,GV,GX,GY,GZ,HH,HX,HDEP,
     $                 INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KH,KG,KP,
     $                 AKBCN,FU,FV,FW,XX,SRCA,SRCB)
C======================================================================
C     輸送方程式を解き新しい時刻のSGS乱流エネルギーを計算する
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       AK: 新しい時刻のk(m**2/s**2),AKN:古い時刻のk(m**/s**2)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MYCNST.h'
C
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),AKN(MX,MY,MZ),RL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),GS(MX,MY,MZ),GT(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),YCOS(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY)
      REAL(8),INTENT(INOUT)::AKBCN(NXY,MZ,4)
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
C
      INTEGER::NN=3,IZCAL=1
      INTEGER::I,J,K,LL
      REAL(8)::TAU,DCE
C ... 局所配列  XX=TCE*AK**(3/2)/RL.OR.DKH=TMU/PRT
      REAL(8),PARAMETER::ANUXX = 0.0D0
C
      CALL CELLSC(AK,AKN,GV,XC,YC,ZC,HX,HDEP,INDP,KH,KG)
C
      CALL FLUXSX(FU,AK,HU,TMU,GX,HDEP,HX,XC,ZC,INDP,INDU,LLWALL,
     $            KG,KP,KH,AKBCN,NN,ANUXX,PRT,PARAMK)
C
      CALL FLUXSY(FV,AK,HV,TMU,GY,HDEP,HX,YC,ZC,INDP,INDV,LLWALL,
     $            KG,KP,KH,AKBCN,NN,ANUXX,PRT,PARAMK)
C
C ... 鉛直方向拡散係数の制限(XX=MIN(TMU,TVSVMX)/PRT)
      DO 300 K=1,MZ
      DO 300 J=1,MY
      DO 300 I=1,MX
         TMU(I,J,K) = MIN(TMU(I,J,K),TVSVMX)
 300  CONTINUE
C
      CALL FLUXSZ(FW,AK,HW,TMU,GZ,ZC,INDP,INDW,LLWALL,
     $            KG,KP,KH,NN,ANUXX,PRT,PARAMK,IZCAL)
C
C ... 板境界処理は不要
C
CC      LL = 6+NN
CC      CALL FLUXPL(FU,FV,FW,LLWALP,LL)
C
C ... 消滅項の計算(XX=Ce/l*AK**(3/2))
      DO 100 K=1,MZ
      DO 100 J=1,MY
      DO 100 I=1,MX
        XX(I,J,K) = 0.0D0
        IF(INDP(I,J,K).GT.0) THEN
          XX(I,J,K)  = TCE*SQRT(AK(I,J,K)**3)/RL(I,J,K)
        END IF
 100  CONTINUE
C
C ... 新しい時刻のAKを計算する
C
      CALL ZERCLR(SRCA,MXYZ,0.0D0)
      CALL ZERCLR(SRCB,MXYZ,0.0D0)
C
      SRCB=GS+GT-XX
C
      CALL CLSNEW(AK,AKN,FU,FV,FW,SRCA,SRCB,TMU,HH,
     $            XC,YC,ZC,XCP,YCOS,GV,GZ,
     $            INDP,INDU,INDV,KG,KP,KF,ANUXX,PRT,0)
      CALL CHKVAL(AK,HH,HDEP,KF,0.0D0)
C
C ... 表層の境界条件
      DCE = (1.0D0/TCE)**(2.0D0/3.0D0)
      DO 200 J=1,MY
      DO 200 I=1,MX
        IF(KF(I,J).LT.MZ) THEN
          K = KF(I,J)
          TAU = CD(I,J)*ADRHO*(WX(I,J)**2+WY(I,J)**2)
          IF( K.GT.KG(I,J) ) AK(I,J,K-1) = TCE*TAU
          AK(I,J,K)   = TCE*TAU
          AK(I,J,K+1) = TCE*TAU
        END IF
  200 CONTINUE    
C
      RETURN
      END
