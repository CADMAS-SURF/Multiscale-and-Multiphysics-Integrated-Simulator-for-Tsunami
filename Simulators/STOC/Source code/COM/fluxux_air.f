      SUBROUTINE FLUXUX_AIR(FU,UUA,HUA,FFA,TMUA,XC,GVA,INDUA,KFA)
C======================================================================
C     X方向の運動量保存式のX方向界面の運動量流束を計算する
C     FU: セル中心定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FU(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),HUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),GVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::VIS1,ADV1,UU1,HU1,HH1,RNU
      INTEGER::I,J,K
C
C
      CALL ZERCLR(FU,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=2,MYM
C     DO 100 I=2,MXM
C                   ( for DOMAIN-DECOMP )
      DO 100 I=2,MX
C
         IF( K.GE.KFA(I,J) .AND.
     $       INDUA(I-1,J,K).GT.0 .OR. INDUA(I,J,K).GT.0 ) THEN
C
C ......... 粘性項
C           (壁面でGXA=0を回避)
            HH1 = MAX(1.D0-FFA(I,J,K)-GVA(I,J,K),0.0D0)
            RNU = AMUAIR/RHOAIR+TMUA(I,J,K)
C
            VIS1 = 2.0D0*HH1*RNU
     $           *(UUA(I,J,K)-UUA(I-1,J,K))*XC(6,I,J)
C
C ......... 慣性項
            UU1  = 0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
            HU1  = 0.5D0*(HUA(I-1,J,K)+HUA(I,J,K))
C
            ADV1 = PARAMAIR2* UU1*HU1
     $           + PARAMAIR *(UUA(I-1,J,K)*MAX(HU1,0.0D0)
     $                       +UUA(I  ,J,K)*MIN(HU1,0.0D0))
C
            FU(I,J,K) = VIS1 - ADV1
         END IF
  100 CONTINUE
C
      RETURN
      END
