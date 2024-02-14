      SUBROUTINE CLDENS(RHOW,TT,CC,GV,ZC,INDP,KF)
C======================================================================
C     密度を計算する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),KF(MX,MY)
C
      REAL(8),EXTERNAL::AKNDSN,AKNDSN2,abrydon
C
      REAL(8)::CSUM,CVAL,RHO1,RHO2,RVAL,TSUM,TVAL,VOL,VSUM
      INTEGER::I,J,K,K1,K2,KK
C
C
      IF( LDENS.EQ.0 ) THEN
         DO 10 K=1,MZ
         DO 10 J=1,MY
         DO 10 I=1,MX
            IF(INDP(I,J,K).GT.0) THEN
               RHOW(I,J,K) = RHO
            ELSE
               RHOW(I,J,K) = 0.0D0
            END IF
   10    CONTINUE
C
      ELSE IF( LDENS.EQ.1 ) THEN
         DO 20 K=1,MZ
         DO 20 J=1,MY
         DO 20 I=1,MX
            IF(INDP(I,J,K).GT.0) THEN
               RHOW(I,J,K) = RHO*(1.0D0-AABB(1)*(TT(I,J,K)-AABB(2))
     $                                 +AABB(3)*(CC(I,J,K)-AABB(4)))
C//淡水と海水の密度式
C... ( CC : 塩素量濃度（‰） 、TT : 温度（℃）)
            ELSE
               RHOW(I,J,K) = 0.0D0
            END IF
   20    CONTINUE
C
      ELSE IF( LDENS.EQ.2 ) THEN
         DO 30 K=1,MZ
         DO 30 J=1,MY
         DO 30 I=1,MX
            IF(INDP(I,J,K).GT.0) THEN
               RHOW(I,J,K) = AKNDSN(CC(I,J,K),TT(I,J,K))
            ELSE
               RHOW(I,J,K) = 0.0D0
            END IF
   30    CONTINUE
C
      ELSE if ( LDENS .EQ. 3) then
         DO 40 K=1,MZ
         DO 40 J=1,MY
         DO 40 I=1,MX
            IF(INDP(I,J,K).GT.0) THEN
               RHOW(I,J,K) = AKNDSN2(CC(I,J,K),TT(I,J,K))
            ELSE
               RHOW(I,J,K) = 0.0D0
            END IF
   40    CONTINUE
C
      ELSE
         DO 50 K=1,MZ
         DO 50 J=1,MY
         DO 50 I=1,MX
            IF(INDP(I,J,K).GT.0) THEN
               RHOW(I,J,K) = ABRYDON(CC(I,J,K),TT(I,J,K))
            ELSE
               RHOW(I,J,K) = 0.0D0
            END IF
   50    CONTINUE
      END IF
C
C
C.... 対流調節処理
C
      IF(MLNS.EQ.0.AND.LDENS.NE.0) THEN
        DO 500 J=2,MYM
        DO 500 I=2,MXM
          K2 = 0
          DO 510 K=2,KF(I,J)-1
            IF(INDP(I,J,K).EQ.1) THEN
              IF(INDP(I,J,K-1).EQ.1) THEN
                IF(RHOW(I,J,K).GT.RHOW(I,J,K-1)) THEN
                  K2 = K
                  DO 520 KK=K1,K2
                    IF(RHOW(I,J,KK).LT.RHOW(I,J,K2)) GO TO 530
  520             CONTINUE
  530             CONTINUE
                  K1 = KK
                  RHO1 = RHOW(I,J,K1)
                  RHO2 = RHOW(I,J,K2)
                  TSUM = 0.0D0
                  CSUM = 0.0D0
                  VSUM = 0.0D0
                  DO 540 KK=K1,K2
                    VOL = ZC(4,K)*GV(I,J,KK)
                    VSUM = VSUM+VOL
                    TSUM = TSUM+VOL*TT(I,J,KK)
                    CSUM = CSUM+VOL*CC(I,J,KK)
  540             CONTINUE
                  TVAL = TSUM/VSUM
                  CVAL = CSUM/VSUM
                  IF(LDENS.EQ.1) THEN
                    RVAL = RHO*(1.0D0-AABB(1)*(TVAL-AABB(2))
     $                               +AABB(3)*(CVAL-AABB(4)))
                  ELSE IF(LDENS.EQ.2) THEN
                    RVAL = AKNDSN(CVAL,TVAL)
                  ELSE IF(LDENS.EQ.3) THEN
                    RVAL = AKNDSN2(CVAL,TVAL)
                  END IF
                  DO 550 KK=K1,K2
                    TT(I,J,KK)   = TVAL
                    CC(I,J,KK)   = CVAL
                    RHOW(I,J,KK) = RVAL
  550             CONTINUE
                END IF
              ELSE
                K1 = K
              END IF
            END IF
  510     CONTINUE
          KK = KF(I,J)
          IF(INDP(I,J,KK-1).EQ.1.AND.INDP(I,J,KK).EQ.1) THEN
            TT(I,J,KK)   = TT(I,J,KK-1)
            CC(I,J,KK)   = CC(I,J,KK-1)
            RHOW(I,J,KK) = RHOW(I,J,KK-1)
          END IF
  500   CONTINUE
      END IF
C
      RETURN
      END
