      SUBROUTINE FLUXWZ(FW,WW,HW,TMU,FF,ZC,GZ,INDW,KF)
C======================================================================
C     Z方向の運動量保存式のZ方向界面の運動量流束を計算する
C     FW: セル中心定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WW(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::ZC(8,MZ),GZ(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY)
C
      REAL(8)::GZ1,VIS1,WW1,HW1,ADV1
      INTEGER::I,J,K
C
C
      CALL ZERCLR(FW,MXYZ,0.0D0)
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
         IF( K.LE.KF(I,J) ) THEN
         IF( INDW(I,J,K-1).GT.0 .OR. INDW(I,J,K).GT.0 ) THEN
C
C ......... 粘性項
            GZ1  = 0.5D0*(GZ(I,J,K-1)+GZ(I,J,K))
            VIS1 = GZ1*(ANUV+TMU(I,J,K))*2.0D0
     $           *(WW(I,J,K)-WW(I,J,K-1))*ZC(6,K)
C
C ......... 慣性項
            WW1  = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            HW1  = 0.5D0*(HW(I,J,K-1)+HW(I,J,K))
C
            ADV1 = PARAMV2* WW1*HW1
     $           + PARAMV *(WW(I,J,K-1)*MAX(HW1,0.0D0)
     $                     +WW(I,J,K  )*MIN(HW1,0.0D0))
            IF(ISW(4).NE.0) ADV1=0.0D0
C
            FW(I,J,K) = VIS1 - ADV1
         END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
