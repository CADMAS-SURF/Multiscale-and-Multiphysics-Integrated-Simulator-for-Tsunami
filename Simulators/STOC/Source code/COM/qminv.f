      SUBROUTINE QMINV(AL,AU,DD,CC,XX,INDP,MZ0)
C======================================================================
C     SOLVE M*X=C ( PRE CONDITION )
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(INOUT)::AL(3,MX,MY,MZ0),AU(3,MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::DD(MX,MY,MZ0),CC(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::XX(MX,MY,MZ0)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ0)
      INTEGER,INTENT(IN)::MZ0
C
      INTEGER::I,J,K
C
C
C ... (AL*DD+I)*XX=CC
C
      DO 100 K=2,MZ0-1
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J,K).GT.0 ) THEN
            XX(I,J,K) = CC(I,J,K) - AL(1,I,J,K)*DD(I-1,J,K)*XX(I-1,J,K)
     $                            - AL(2,I,J,K)*DD(I,J-1,K)*XX(I,J-1,K)
     $                            - AL(3,I,J,K)*DD(I,J,K-1)*XX(I,J,K-1)
         END IF
  100 CONTINUE
C
C
C ... (DD+AU)*XX=YY
C
      DO 200 K=MZ0-1,2,-1
      DO 200 J=MYM,2,-1
      DO 200 I=MXM,2,-1
         IF( INDP(I,J,K).GT.0 ) THEN
            XX(I,J,K) = DD(I,J,K)
     $                * ( XX(I,J,K) - AU(1,I,J,K)*XX(I+1,J,K)
     $                              - AU(2,I,J,K)*XX(I,J+1,K)
     $                              - AU(3,I,J,K)*XX(I,J,K+1) )
         END IF
  200 CONTINUE
C
      RETURN
      END
