      SUBROUTINE QMINV2D(AL,AU,DD,CC,XX,INDP)
C======================================================================
C     SOLVE M*X=C ( PRE CONDITION )
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(IN)::AL(2,MX,MY),AU(2,MX,MY)
      REAL(8),INTENT(IN)::DD(MX,MY)
      REAL(8),INTENT(INOUT)::CC(MX,MY),XX(MX,MY)
      INTEGER,INTENT(IN)::INDP(MX,MY)
C
      INTEGER::I,J
C
C
C ... (AL*DD+I)*XX=CC
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDP(I,J).GT.0 ) THEN
            XX(I,J) = CC(I,J) - AL(1,I,J)*DD(I-1,J)*XX(I-1,J)
     $                        - AL(2,I,J)*DD(I,J-1)*XX(I,J-1)
         END IF
  100 CONTINUE
C
C
C ... (DD+AU)*XX=YY
C
      DO 200 J=MYM,2,-1
      DO 200 I=MXM,2,-1
         IF( INDP(I,J).GT.0 ) THEN
            XX(I,J) = DD(I,J)
     $                * ( XX(I,J) - AU(1,I,J)*XX(I+1,J)
     $                            - AU(2,I,J)*XX(I,J+1))
         END IF
  200 CONTINUE
C
      RETURN
      END
