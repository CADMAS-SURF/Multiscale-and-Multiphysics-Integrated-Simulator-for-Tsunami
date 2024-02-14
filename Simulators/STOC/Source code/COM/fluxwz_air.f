      SUBROUTINE FLUXWZ_AIR(FW,WWA,HWA,TMUA,ZCA,GVA,INDWA,KFA)
C======================================================================
C     Z方向の運動量保存式のZ方向界面の運動量流束を計算する
C     FW: セル中心定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::FW(MX,MY,MZA)
      REAL(8),INTENT(IN)::WWA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::ZCA(8,MZA),GVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::VIS1,ADV1,WW1,HW1,HH1,RNU
      INTEGER::I,J,K
C
C
      CALL ZERCLR(FW,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
         IF( K.GE.KFA(I,J)-1 .AND.
     $       INDWA(I,J,K-1).GT.0 .OR. INDWA(I,J,K).GT.0 ) THEN
C
C ......... 粘性項
            HH1 = 1.D0-GVA(I,J,K)
            RNU = AMUAIR/RHOAIR+TMUA(I,J,K)
C
            VIS1 = 2.0D0*HH1*RNU
     $           *(WWA(I,J,K)-WWA(I,J,K-1))*ZCA(6,K)
C
C ......... 慣性項
            WW1  = 0.5D0*(WWA(I,J,K-1)+WWA(I,J,K))
            HW1  = 0.5D0*(HWA(I,J,K-1)+HWA(I,J,K))
C
            ADV1 = PARAMAIR2* WW1*HW1
     $           + PARAMAIR *(WWA(I,J,K-1)*MAX(HW1,0.0D0)
     $                       +WWA(I,J,K  )*MIN(HW1,0.0D0))
C
            FW(I,J,K) = VIS1 - ADV1
         END IF
  100 CONTINUE
C
      RETURN
      END
