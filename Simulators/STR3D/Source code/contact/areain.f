      LOGICAL FUNCTION AREAIN(RL,TOL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RL(3)
C----&------------------------------------------------------------------
      IF( RL(1) .GT. -TOL .AND. RL(1) .LT. 1.+TOL .AND.
     &    RL(2) .GT. -TOL .AND. RL(2) .LT. 1.+TOL .AND.
     &    RL(3) .GT. -TOL .AND. RL(3) .LT. 1.+TOL ) THEN
        AREAIN=.TRUE.
      ELSE
        AREAIN=.FALSE.
      ENDIF
C
      RETURN
      END
