      SUBROUTINE MODRQRL(Q2,QL,RQ,RL,INDP,KF)
C----------------------------------------------------------------------
C     計算された値の最大値、最小値をチェックし、変数の値を更新する
C      1) l.LE.0.001.or.q2.LE.0.0005 : l=lmin=1.0,q2=0.001
C      2) l.LT.1.0                   : l=lmin=1.0
C      3) l.GT.500                   : l=lmax=500
C      4) q.GT.1000                  : q=qmax=1000
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::Q2(MX,MY,MZ),QL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::RQ(MX,MY,MZ),RL(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),KF(MX,MY)
C
      INTEGER::I,J,K
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(K.LE.KF(I,J)+1.AND.INDP(I,J,K).GT.0) THEN
            IF(K.NE.KF(I,J).AND.
     $         (Q2(I,J,K).LT.5.0D-4.OR.QL(I,J,K).LT.1.0D-3)) THEN
              Q2(I,J,K) = 5.0D-4
              RL(I,J,K) = 1.0D0
              QL(I,J,K) = 2.0D0*Q2(I,J,K)*RL(I,J,K)
            ELSE
              IF(K.NE.KF(I,J)) THEN
                RL(I,J,K) = 0.5D0*QL(I,J,K)/Q2(I,J,K)
              ELSE IF(Q2(I,J,K).NE.0.0D0.AND.QL(I,J,K).GE.1.0D-10) THEN
                RL(I,J,K) = 0.5D0*QL(I,J,K)/Q2(I,J,K)
              ELSE 
                Q2(I,J,K) = 5.0D-4
                RL(I,J,K) = 1.0D0
                QL(I,J,K) = 2.0D0*Q2(I,J,K)*RL(I,J,K)
              END IF          
            END IF
            IF(K.NE.KF(I,J).AND.RL(I,J,K).LT.1.0D0) THEN
              RL(I,J,K) = 1.0D0
              QL(I,J,K) = 2.0D0*Q2(I,J,K)*RL(I,J,K)
            END IF
            IF(RL(I,J,K).GT.500.0D0) THEN
              RL(I,J,K) = 500.0D0
              QL(I,J,K) = 2.0D0*Q2(I,J,K)*RL(I,J,K)
            END IF
            RQ(I,J,K) = SQRT(2.0D0*Q2(I,J,K))
            IF(RQ(I,J,K).GT.1.0D3) THEN
              RQ(I,J,K) = 1.0D3
              Q2(I,J,K) = 0.5D0*RQ(I,J,K)**2
              QL(I,J,K) = 2.0D0*Q2(I,J,K)*RL(I,J,K)
            END IF
         END IF
  100 CONTINUE
C
      RETURN
      END
