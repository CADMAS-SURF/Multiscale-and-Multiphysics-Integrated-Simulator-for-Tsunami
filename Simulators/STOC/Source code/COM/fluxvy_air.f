      SUBROUTINE FLUXVY_AIR(FV,VVA,HVA,FFA,TMUA,YC,GVA,INDVA,KFA)
C======================================================================
C     Y方向の運動量保存式のY方向界面の運動量流束を計算する
C     FV: セル中心定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FV(MX,MY,MZA)
      REAL(8),INTENT(IN)::VVA(MX,MY,MZA),HVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::YC(8,MY),GVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::VIS1,ADV1,VV1,HV1,HH1,RNU
      INTEGER::I,J,K
C
C
      CALL ZERCLR(FV,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
C     DO 100 J=2,MYM
C                   ( for DOMAIN-DECOMP )
      DO 100 J=2,MY
      DO 100 I=2,MXM
C
         IF( K.GE.KFA(I,J) .AND.
     $       INDVA(I,J-1,K).GT.0 .OR. INDVA(I,J,K).GT.0 ) THEN
C
C ......... 粘性項
C           (壁面でGYA=0を回避)
            HH1  = MAX(1.D0-FFA(I,J,K)-GVA(I,J,K),0.0D0)
            RNU = AMUAIR/RHOAIR+TMUA(I,J,K)
C
            VIS1 = 2.0D0*HH1*RNU
     $           *(VVA(I,J,K)-VVA(I,J-1,K))*YC(6,J)
C
C ......... 慣性項
            VV1  = 0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
            HV1  = 0.5D0*(HVA(I,J-1,K)+HVA(I,J,K))
C
            ADV1 = PARAMAIR2* VV1*HV1
     $           + PARAMAIR *(VVA(I,J-1,K)*MAX(HV1,0.0D0)
     $                       +VVA(I,J  ,K)*MIN(HV1,0.0D0))
C
            FV(I,J,K) = VIS1 - ADV1
         END IF
  100 CONTINUE
C
      RETURN
      END
