      SUBROUTINE CLENGY(TT,TN,HU,HV,HW,TMUX,TMUY,TMUZ,XC,YC,ZC,
     $                  XCP,YCOS,YCOSP,GV,GX,GY,GZ,HH,HX,HDEP,QQ,
     $                  INDP,INDU,INDV,INDW,LLWALL,LLWALP,KF,KH,KG,KP,
     $                  TTBCN,FU,FV,FW,SRCA,SRCB)
C======================================================================
C     エネルギー式のより新しい時刻の温度を計算する
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       TT: 新しい時刻の温度(°C)
C       TN: N時刻の温度*体積
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'CONSRV.h'
C
      REAL(8),INTENT(INOUT)::TN(MX,MY,MZ),TT(MX,MY,MZ),QQ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),HH(MX,MY),HX(MX,MY)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUX(MX,MY,MZ),TMUY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),YCOS(MY),YCOSP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KH(MX,MY),KG(MX,MY),KP(MX,MY)
      REAL(8),INTENT(INOUT)::TTBCN(NXY,MZ,4)
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
C
C     局所配列変数
      REAL(8)::TMP(MX,MY,MZ)
C
      INTEGER::I,J,K,KF1,LL
      INTEGER::NN=1,IZCAL=0
C
C
      tmp = 1.0d0
      call cellsc(tmp,tn,gv,xc,yc,zc,hx,hdep,indp,kh,kg)
      call chkcns(tn,sumvl)

      CALL CELLSC(TT,TN,GV,XC,YC,ZC,HX,HDEP,INDP,KH,KG)
      CALL CHKCNS(TN,SUMTT)
C
      CALL FLUXSX(FU,TT,HU,TMUX,GX,HDEP,HX,XC,ZC,INDP,INDU,LLWALL,
     $            KG,KP,KH,TTBCN,NN,ALPH,PRT,PARAMT)
C
      CALL FLUXSY(FV,TT,HV,TMUY,GY,HDEP,HX,YC,ZC,INDP,INDV,LLWALL,
     $            KG,KP,KH,TTBCN,NN,ALPH,PRT,PARAMT)
C
      CALL FLUXSZ(FW,TT,HW,TMUZ,GZ,ZC,INDP,INDW,LLWALL,
     $            KG,KP,KH,NN,ALPV,PRT,PARAMT,IZCAL)
C
C ... 板境界の流束を修正する
C
      LL=6+NN
      CALL FLUXPL(FU,FV,FW,LLWALP,LL)
C
      CALL ZERCLR(SRCA,MXYZ,0.0D0)
      CALL ZERCLR(SRCB,MXYZ,0.0D0)
C
C ... 海面からの熱流束条件を処理する
      IF( LSURF.EQ.1 )
     $   CALL BCTSRF(SRCA,SRCB,FW,TT,HH,HX,HDEP,QQ,XC,YC,ZC,GV,
     $               INDP,KF,KH,KG)
C
C ... 新しい時刻の温度を計算する
C
      CALL CLSNEW(TT,TN,FU,FV,FW,SRCA,SRCB,TMUZ,HH,
     $            XC,YC,ZC,XCP,YCOS,GV,GZ,
     $            INDP,INDU,INDV,KG,KP,KF,ALPV,PRT,0)
      CALL CHKVAL(TT,HH,HDEP,KF,20.0D0)
C
      RETURN
      END
