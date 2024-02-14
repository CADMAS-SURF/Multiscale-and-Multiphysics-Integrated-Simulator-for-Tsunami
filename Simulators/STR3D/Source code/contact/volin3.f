      LOGICAL FUNCTION VOLIN3(RN,TOL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RN(4)
C----&------------------------------------------------------------------
      DO 100 I=1,4
        IF( RN(I) .GT. -TOL ) THEN
          VOLIN3 = .TRUE.
        ELSE
          VOLIN3 = .FALSE.
          RETURN
        ENDIF
  100 CONTINUE
C
      END
