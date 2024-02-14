      SUBROUTINE CLQLEQ(QL,SN,RL,UU,VV,WW,TMUX,TMUY,TMUZ,GSGT,Q3DB,
     $                  HU,HV,HW,HX,HDEP,KH,
     $                  QLBCN,QLSRF,XC,YC,ZC,XCP,YCOS,GV,GX,GY,GZ,HH,
     $                  INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KG,KP,
     $                  FU,FV,FW,SRCA,SRCB)
C======================================================================
C     輸送方程式を解き新しい時刻の乱流エネルー散逸を計算する
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       QL: 新しい時刻のQ2L,AKN:古い時刻のQ2L
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MYCNST.h'
C
      REAL(8),INTENT(INOUT)::QL(MX,MY,MZ),SN(MX,MY,MZ),RL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUX(MX,MY,MZ),TMUY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUZ(MX,MY,MZ),GSGT(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::Q3DB(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::QLBCN(NXY,MZ,4),QLSRF(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),YCOS(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY)
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
C
      INTEGER::NN=4,IZCAL=1
      INTEGER::I,J,K,LL,KGIJ
      REAL(8)::DEP,DL
C
C
      CALL CELLSC(QL,SN,GV,XC,YC,ZC,HX,HDEP,INDP,KH,KG)
C
      CALL FLUXSX(FU,QL,HU,TMUX,GX,HDEP,HX,XC,ZC,INDP,INDU,LLWALL,
     $            KG,KP,KH,QLBCN,NN,0.0D0,1.0D0,PARAMK)
C
      CALL FLUXSY(FV,QL,HV,TMUY,GY,HDEP,HX,YC,ZC,INDP,INDV,LLWALL,
     $            KG,KP,KH,QLBCN,NN,0.0D0,1.0D0,PARAMK)
C
      CALL FLUXSZ(FW,QL,HW,TMUZ,GZ,ZC,INDP,INDW,LLWALL,
     $            KG,KP,KH,NN,0.0D0,1.0D0,PARAMK,IZCAL)
C
C ... 板境界処理は不要
C
CC      LL = 6+NN
CC      CALL FLUXPL(FU,FV,FW,LLWALP,LL)
C
C ... 新しい時刻のq2lを計算する
C
      CALL ZERCLR(SRCA,MXYZ,0.0D0)
      CALL ZERCLR(SRCB,MXYZ,0.0D0)
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(KF(I,J).LT.MZ) THEN
            IF( INDP(I,J,K).GT.0.AND.K.LT.KF(I,J) ) THEN
               KGIJ = KG(I,J)
               DEP = ZC(1,KGIJ)-GV(I,J,KGIJ)*ZC(4,KGIJ)
               DL = 1.0D0/(HH(I,J)-ZC(2,K))+1.0D0/(ZC(2,K)-DEP)
               SRCB(I,J,K) = RL(I,J,K)*RE1*GSGT(I,J,K)
     $                      -Q3DB(I,J,K)*RL(I,J,K)*
     $                       (1.0D0+RE2*(RL(I,J,K)/RKAR*DL)**2)
            ENDIF
         ENDIF
  100 CONTINUE
C
      CALL CLSNEW(QL,SN,FU,FV,FW,SRCA,SRCB,TMUZ,HH,
     $            XC,YC,ZC,XCP,YCOS,GV,GZ,
     $            INDP,INDU,INDV,KG,KP,KF,0.0D0,SCT,0)
      CALL CHKVAL(QL,HH,HDEP,KF,0.0D0)
C
C ... 表層の境界条件
      DO 200 J=1,MY
      DO 200 I=1,MX
        IF(KF(I,J).LT.MZ) THEN
          K = KF(I,J)
          IF( K.GT.KG(I,J) ) QL(I,J,K-1) = QLSRF(I,J)
          QL(I,J,K)   = QLSRF(I,J)
          QL(I,J,K+1) = QLSRF(I,J)
       END IF
  200 CONTINUE    
C
      RETURN
      END
