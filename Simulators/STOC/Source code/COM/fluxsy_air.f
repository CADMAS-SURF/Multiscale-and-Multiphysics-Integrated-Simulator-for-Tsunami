      SUBROUTINE FLUXSY_AIR(FV,AKE,HVA,TMUA,FFA,GYA,YC,AKEBCAIR,
     $                      INDVA,DTMU)
C======================================================================
C     Y方向の壁面、流速境界条件（固定、自由流入出）境界値を設定し流束を計算する
C     FV: Y方向格子点、X方向セル中心点、Z方向セル中心点で定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(OUT)::FV(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::AKE(MX,MY,MZA)
      REAL(8),INTENT(IN)::HVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA)
      REAL(8),INTENT(IN)::YC(8,MY)
      REAL(8),INTENT(IN)::AKEBCAIR(NXY,MZA,4)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::DTMU
C
      REAL(8)::ADV1,CON1,ENU,AKE1,FFA1,GG1
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     流速固定境界(固定流速が流入時固定、流出時勾配ゼロ）
C----------------------------------------------------------------------
C     南側境界
      IF( IBCAIRSOU.GE.0 ) THEN
         J=1
         DO K=2,MZMA
         DO I=2,MXM
            IF( INDVA(I,J,K).EQ.-1 ) THEN
               IF(HVA(I,J,K).GE.0.0D0) THEN
                  AKE(I,J,K) = AKEBCAIR(I,K,1)
               ELSE
                  AKE(I,J,K) = AKE(I,J+1,K)
               ENDIF
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
C     北側境界
      IF( IBCAIRNOR.GE.0 ) THEN
         J=MYM
         DO K=2,MZMA
         DO I=2,MXM
            IF( INDVA(I,J,K).EQ.-1 ) THEN
               IF(HVA(I,J,K).LE.0.0D0) THEN
                  AKE(I,J+1,K) = AKEBCAIR(I,K,4)
               ELSE
                  AKE(I,J+1,K) = AKE(I,J,K)
               ENDIF
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
      CALL CP_DSR_DC2(MX,MY,MZA,0,1,AKE)
C
C----------------------------------------------------------------------
C     流束計算
C----------------------------------------------------------------------
      CALL ZERCLR(FV,MXY*MZA,0.0D0)
C
      DO 200 K=2,MZMA
      DO 200 J=1,MYM
      DO 200 I=2,MXM
         IF( INDVA(I,J,K).GT.0 ) THEN
            AKE1 = AKE(I,J,K)*YC(7,J)+AKE(I,J+1,K)*YC(8,J)
            ADV1 = PARAMAIR2*HVA(I,J,K)*AKE1
     $           + PARAMAIR *(AKE(I,J  ,K)*MAX(HVA(I,J,K),0.0D0)
     $           +            AKE(I,J+1,K)*MIN(HVA(I,J,K),0.0D0))
C
            FFA1 = MAX(FFA(I,J,K),FFA(I,J+1,K))
            GG1  = 1.0D0-FFA1-GYA(I,J,K)
            ENU  = AMUAIR/RHOAIR
     $           +(TMUA(I,J,K)*YC(7,J)+TMUA(I,J+1,K)*YC(8,J))/DTMU
            CON1 = GG1*ENU*(AKE(I,J+1,K)-AKE(I,J,K))*YC(5,J)
C
C ...... 壁境界
         ELSE
            ADV1 = 0.0D0
            CON1 = 0.0D0
         ENDIF
C
         FV(I,J,K) = CON1 - ADV1
  200 CONTINUE
C
      RETURN
      END
