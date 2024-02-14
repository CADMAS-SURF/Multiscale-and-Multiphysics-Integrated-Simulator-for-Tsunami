      SUBROUTINE CLDP(DP,PP,PATM,FF,GV,GX,GY,ZC,INDU,INDV,LLWALB,
     $                KF,KG,IFLAG)
C======================================================================
C     セル間の圧力差を計算する
C     IFLAG = 1: X方向の圧力差を計算
C     IFLAG = 2: Y方向の圧力差を計算
C
C     遡上や防潮堤と関係のないセルでは次の単純な引き算のみを実施
C        DP(I,J,K)=PP(I+1,J,k)-PP(I,J,K) or PP(I,J+1,k)-PP(I,J,K)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(OUT)::DP(MX,MY,MZ)
      REAL(8),INTENT(IN)::PP(MX,MY,MZ),PATM(MX,MY)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(IN)::ZC(8,MZ)
C
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(IN)::LLWALB(3,MLWALB),KF(MX,MY),KG(MX,MY)
      INTEGER,INTENT(IN)::IFLAG
C
      INTEGER::I,J,K,N,KG1
      REAL(8)::P1,P2,GV1,GV2,GX1,GY1
C
C
C----------------------------------------------------------------------
C     (1) CLUEQで用いる圧力差を計算
C----------------------------------------------------------------------
      IF( IFLAG.EQ.1 ) THEN
         DO 100 K=2,MZM
         DO 100 J=2,MYM
         DO 100 I=1,MXM
            IF( K.LE.KF(I,J) .OR. K.LE.KF(I+1,J) ) THEN
            IF( INDU(I,J,K).GT.0 ) THEN
               KG1 = MAX(KG(I,J),KG(I+1,J))
               GV1 = 1.0D0-GV(I  ,J,K)
               GV2 = 1.0D0-GV(I+1,J,K)
C
C .............標高差,段波用処理
               IF( K.GT.KF(I,J) ) THEN
                  P1 = PATM(I,J) + 0.5D0*RHO*GRAV*ZC(4,K)
CC 高い側の標高で低い側の水位を制限する場合
CC               ELSE IF( FF(I,J,K).LT.GV2 ) THEN
CC                  P1 = PATM(I,J) - RHO*GRAV*(GV2-0.5D0)*ZC(4,K)
               ELSE
                  P1 = PP(I,J,K)
               END IF
C
               IF( K.GT.KF(I+1,J) ) THEN
                  P2 = PATM(I+1,J) + 0.5D0*RHO*GRAV*ZC(4,K)
CC 高い側の標高で低い側の水位を制限する場合
CC               ELSE IF( FF(I+1,J,K).LT.GV1 ) THEN
CC                  P2 = PATM(I+1,J) - RHO*GRAV*(GV1-0.5D0)*ZC(4,K)
               ELSE
                  P2 = PP(I+1,J,K)
               END IF
C
               DP(I,J,K) = P2-P1
            END IF
            END IF
  100    CONTINUE
C
C
C ...... 防潮堤用処理
!CDIR NODEP
         DO 110 N=1,MLWALBX
            I = LLWALB(1,N)
            J = LLWALB(2,N)
            K = LLWALB(3,N)
C
            GX1 = 1.0D0-GX(I,J,K)
            IF( FF(I,J,K).LT.GX1 ) THEN
               P1 = PATM(I,J) - RHO*GRAV*(GX1-0.5D0)*ZC(4,K)
            ELSE
               P1 = PP(I,J,K)
            END IF
            IF( FF(I+1,J,K).LT.GX1 ) THEN
               P2 = PATM(I+1,J) - RHO*GRAV*(GX1-0.5D0)*ZC(4,K)
            ELSE
               P2 = PP(I+1,J,K)
            END IF
            DP(I,J,K) = P2-P1
  110    CONTINUE
C
C
C----------------------------------------------------------------------
C     (2) CLVEQで用いる圧力差を計算
C----------------------------------------------------------------------
      ELSE IF( IFLAG.EQ.2 ) THEN
         DO 200 K=2,MZM
         DO 200 J=1,MYM
         DO 200 I=2,MXM
            IF( K.LE.KF(I,J) .OR. K.LE.KF(I,J+1) ) THEN
            IF( INDV(I,J,K).GT.0 ) THEN
               KG1 = MAX(KG(I,J),KG(I,J+1))
               GV1 = 1.0D0-GV(I,J  ,K)
               GV2 = 1.0D0-GV(I,J+1,K)
C
C .............標高差,段波用処理
               IF( K.GT.KF(I,J) ) THEN
                  P1 = PATM(I,J) + 0.5D0*RHO*GRAV*ZC(4,K)
CC 高い側の標高で低い側の水位を制限する場合
CC               ELSE IF( FF(I,J,K).LT.GV2 ) THEN
CC                  P1 = PATM(I,J) - RHO*GRAV*(GV2-0.5D0)*ZC(4,K)
               ELSE
                  P1 = PP(I,J,K)
               END IF
C
               IF( K.GT.KF(I,J+1) ) THEN
                  P2 = PATM(I,J+1) + 0.5D0*RHO*GRAV*ZC(4,K)
CC 高い側の標高で低い側の水位を制限する場合
CC               ELSE IF( FF(I,J+1,K).LT.GV1 ) THEN
CC                  P2 = PATM(I,J+1) - RHO*GRAV*(GV1-0.5D0)*ZC(4,K)
               ELSE
                  P2 = PP(I,J+1,K)
               END IF
C
               DP(I,J,K) = P2-P1
            END IF
            END IF
  200    CONTINUE
C
C
C ...... 防潮堤用処理
!CDIR NODEP
         DO 210 N=MLWALBX+1,MLWALB
            I = LLWALB(1,N)
            J = LLWALB(2,N)
            K = LLWALB(3,N)
C
            GY1 = 1.0D0-GY(I,J,K)
            IF( FF(I,J,K).LT.GY1 ) THEN
               P1 = PATM(I,J) - RHO*GRAV*(GY1-0.5D0)*ZC(4,K)
            ELSE
               P1 = PP(I,J,K)
            END IF
            IF( FF(I,J+1,K).LT.GY1 ) THEN
               P2 = PATM(I,J+1) - RHO*GRAV*(GY1-0.5D0)*ZC(4,K)
            ELSE
               P2 = PP(I,J+1,K)
            END IF
            DP(I,J,K) = P2-P1
  210    CONTINUE
C
      END IF
C
      RETURN
      END
