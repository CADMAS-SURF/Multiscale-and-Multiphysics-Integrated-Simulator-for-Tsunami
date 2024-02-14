      SUBROUTINE FLUXSX_AIR(FU,AKE,HUA,TMUA,FFA,GXA,XC,AKEBCAIR,
     $                      INDUA,DTMU)
C======================================================================
C     X方向の壁面、流速境界条件（固定、自由流入出）境界値を設定し流束を計算する
C     FU: X方向格子点、Y方向セル中心点、Z方向セル中心点で定義
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(OUT)::FU(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::AKE(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY)
      REAL(8),INTENT(IN)::AKEBCAIR(NXY,MZA,4)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::DTMU
C
      REAL(8)::ADV1,CON1,ENU,AKE1,FFA1,GG1
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     流速固定境界(固定流速が流入時固定、流出時勾配ゼロ）
C----------------------------------------------------------------------
C     西側境界
      IF( IBCAIRWES.GE.0 ) THEN
         I=1
         DO K=2,MZMA
         DO J=2,MYM
            IF( INDUA(I,J,K).EQ.-1 ) THEN
               IF(HUA(I,J,K).GE.0.0D0) THEN
                  AKE(I,J,K) = AKEBCAIR(J,K,2)
               ELSE
                  AKE(I,J,K) = AKE(I+1,J,K)
               ENDIF
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
C     東側境界
      I=MXM
      IF( IBCAIREAS.GE.0 ) THEN
         I=MXM
         DO K=2,MZMA
         DO J=2,MYM
            IF( INDUA(I,J,K).EQ.-1 ) THEN
               IF(HUA(I,J,K).LE.0.0D0) THEN
                  AKE(I+1,J,K) = AKEBCAIR(J,K,3)
               ELSE
                  AKE(I+1,J,K) = AKE(I,J,K)
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
      CALL ZERCLR(FU,MXY*MZA,0.0D0)
C
      DO 200 K=2,MZMA
      DO 200 J=2,MYM
      DO 200 I=1,MXM
         IF( INDUA(I,J,K).GE.-1 ) THEN
            AKE1 = AKE(I,J,K)*XC(7,I,J)+AKE(I+1,J,K)*XC(8,I,J)
            ADV1 = PARAMAIR2*HUA(I,J,K)*AKE1
     $           + PARAMAIR *(AKE(I  ,J,K)*MAX(HUA(I,J,K),0.0D0)
     $           +            AKE(I+1,J,K)*MIN(HUA(I,J,K),0.0D0))
C
            FFA1 = MAX(FFA(I,J,K),FFA(I+1,J,K))
            GG1  = 1.0D0-FFA1-GXA(I,J,K)
            ENU  = AMUAIR/RHOAIR
     $           +(TMUA(I,J,K)*XC(7,I,J)+TMUA(I+1,J,K)*XC(8,I,J))/DTMU
            CON1 = GG1*ENU*(AKE(I+1,J,K)-AKE(I,J,K))*XC(5,I,J)
C
C ...... 壁境界
         ELSE
            ADV1 = 0.0D0
            CON1 = 0.0D0
         ENDIF
C
         FU(I,J,K) = CON1 - ADV1
  200 CONTINUE
C
      RETURN
      END
