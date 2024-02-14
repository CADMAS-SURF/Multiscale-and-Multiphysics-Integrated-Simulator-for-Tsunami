      SUBROUTINE CLVEQ_AIR(VPA,UUA,VVA,WWA,HUA,HVA,HWA,PPA,FFA,
     $                     TMUA,VVBCAIR,GVA,GXA,GYA,GZA,XC,YC,ZCA,
     $                     XCP,YCOSP,YSINP,INDPA,INDUA,INDVA,INDWA,KFA,
     $                     FU,FV,FW,VVX,VVZ)
C======================================================================
C     Y方向の運動量の保存式を圧力項を陽に解いて仮流速Vを計算する
C     ※仮流速はUPUVWPルーチンで圧力補正量を用いて補正される
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'GRID.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(OUT)::VPA(MX,MY,MZA)
C
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::PPA(MX,MY,MZA),FFA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::VVBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      REAL(8),INTENT(IN)::YCOSP(MY),YSINP(MY)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::FU(MX,MY,MZA),FV(MX,MY,MZA),FW(MX,MY,MZA) ! WORK
      REAL(8)::VVX(MX,MY,MZA),VVZ(MX,MY,MZA)             ! WORK
C
      REAL(8)::GV1
      REAL(8)::DP1,UUVK,DV1,CORIU,DHDTV
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     (1) 境界面上のVを設定する
C----------------------------------------------------------------------
      CALL BCVVXZ_AIR(VVX,VVZ,UUA,VVA,WWA,FFA,TMUA,VVBCAIR,
     $                GVA,XCP,ZCA,INDPA,INDUA,INDWA)
C
C ... VVX,VVZ
      CALL CP_DSR_DC2(MX,MY,MZA,1,1,VVX)
      CALL CP_DSR_DC2(MX,MY,MZA,3,1,VVZ)
C
C
C----------------------------------------------------------------------
C     (2) コントロールボリューム界面の運動量流束を計算する(板境界は除く)
C----------------------------------------------------------------------
      CALL FLUXVX_AIR(FU,UUA,VVA,HUA,VVX,FFA,TMUA,XC,YC,XCP,
     $                GVA,GXA,INDUA,INDVA,KFA)
C
      CALL FLUXVY_AIR(FV,VVA,HVA,FFA,TMUA,YC,GVA,INDVA,KFA)
C
      CALL FLUXVZ_AIR(FW,VVA,WWA,HWA,VVZ,TMUA,YC,ZCA,
     $                GZA,INDVA,INDWA,KFA)
C
C ... FU,FW
      CALL CP_DSR_DC2(MX,MY,MZA,1,1,FU)
      CALL CP_DSR_DC2(MX,MY,MZA,3,1,FW)
C
C
C----------------------------------------------------------------------
C     (3) 仮流速のy方向成分を計算する(自由流入出境界上の点は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(VPA,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=1,MYM
      DO 100 I=2,MXM
         IF( INDVA(I,J,K).GT.0 ) THEN
            GV1 = (1.0D0-FFA(I,J  ,K)-GVA(I,J  ,K))*YC(8,J)
     $          + (1.0D0-FFA(I,J+1,K)-GVA(I,J+1,K))*YC(7,J)
C
            UUVK= (UUA(I  ,J,K)*YC(7,J)+UUA(I  ,J+1,K)*YC(8,J))*0.5D0
     $          + (UUA(I-1,J,K)*YC(7,J)+UUA(I-1,J+1,K)*YC(8,J))*0.5D0
            CORIU = GV1*2.0D0*CEARTH*YSINP(J)*UUVK
C
            DP1 = GV1/RHOAIR*(PPA(I,J+1,K)-PPA(I,J,K))*YC(5,J)
C
            DHDTV = VVA(I,J,K)
     $          *((( HUA(I,J  ,K)-HUA(I-1,J  ,K) )*YC(8,J)
     $          +  ( HUA(I,J+1,K)-HUA(I-1,J+1,K) )*YC(7,J))*XCP(6,I,J)
     $          +  ( HVA(I,J+1,K)-HVA(I,J-1,K)   )*0.5D0
     $          *YC(5,J)/YCOSP(J)
     $          + (( HWA(I,J  ,K)-HWA(I,J  ,K-1) )*YC(8,J)
     $          +  ( HWA(I,J+1,K)-HWA(I,J+1,K-1) )*YC(7,J))*ZCA(6,K) )
C
            DV1 = - DP1 - CORIU + DHDTV
     $          + ( FU(I,J,K) - FU(I-1,J,K) ) * XCP(6,I,J)
     $          + ( FV(I,J+1,K) - FV(I,J,K) ) * YC(5,J)/YCOSP(J)
     $          + ( FW(I,J,K) - FW(I,J,K-1) ) * ZCA(6,K)
C
            VPA(I,J,K) = VVA(I,J,K)+DTV*DV1/GV1
         ELSEIF( INDVA(I,J,K).EQ. 0 ) THEN
C           (NOTHING TO DO)
         ELSEIF( INDVA(I,J,K).EQ.-1 ) THEN
            VPA(I,J,K) = VVA(I,J,K)
         ELSEIF( INDVA(I,J,K).LE.-2 ) THEN
            VPA(I,J,K) = 0.0D0
         ENDIF
  100 CONTINUE
C
      RETURN
      END
