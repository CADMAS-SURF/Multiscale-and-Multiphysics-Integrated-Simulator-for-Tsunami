      SUBROUTINE FLUXSZ_AIR(FW,AKE,HWA,TMUA,GZA,ZCA,INDWA,DTMU)
C======================================================================
C     Z方向の壁面、流速境界条件（固定、自由流入出）境界値を設定し流束を計算する
C     FW: Z方向格子点、X方向セル中心点、Y方向セル中心点で定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(OUT)::FW(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::AKE(MX,MY,MZA)
      REAL(8),INTENT(IN)::HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::ZCA(8,MZA)
      INTEGER,INTENT(IN)::INDWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::DTMU
C
      REAL(8)::ADV1,CON1,ENU,AKE1,GG1
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     自由流入出境界
C----------------------------------------------------------------------
C     上面
      IF( IBCAIRTOP.EQ.-2 ) THEN
         K=MZMA
         DO J=2,MYM
         DO I=2,MXM
            IF( INDWA(I,J,K).EQ.0 ) THEN
               AKE(I,J,K+1) = AKE(I,J,K)
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
C
C----------------------------------------------------------------------
C     流束計算
C----------------------------------------------------------------------
      CALL ZERCLR(FW,MXY*MZA,0.0D0)
C
      DO 200 K=1,MZMA
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF( INDWA(I,J,K).GE.-1 ) THEN
            AKE1 = AKE(I,J,K)*ZCA(7,K)+AKE(I,J,K+1)*ZCA(8,K)
            ADV1 = PARAMAIR2*HWA(I,J,K)*AKE1
     $           + PARAMAIR *(AKE(I,J,K  )*MAX(HWA(I,J,K),0.0D0)
     $           +            AKE(I,J,K+1)*MIN(HWA(I,J,K),0.0D0))
C
C
            GG1  = 1.0D0-GZA(I,J,K)
            ENU  = AMUAIR/RHOAIR
     $           +(TMUA(I,J,K)*ZCA(7,K)+TMUA(I,J,K+1)*ZCA(8,K))/DTMU
            CON1 = GG1*ENU*(AKE(I,J,K+1)-AKE(I,J,K))*ZCA(5,K)
C
C ...... 壁境界
         ELSE
            ADV1 = 0.0D0
            CON1 = 0.0D0
         ENDIF
C
         FW(I,J,K) = CON1 - ADV1
  200 CONTINUE
C
      RETURN
      END
