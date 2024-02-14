      SUBROUTINE BEAM2(IST,SY,S,D,E,ANU,ST)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(3),D(6)

      IF( ST > 0. ) THEN
        IF( S(1) >= ST ) THEN
          IST = 1
          S(:) = 0.
          D(:) = 0.
          RETURN
        ENDIF
      ENDIF

      IF( DABS(S(1)) >= SY ) THEN
        SY = DABS(S(1))
      ELSE
        IST = 0
        SY = DABS(S(1))
        CALL DBEAM0(D,E,ANU)
      ENDIF

      END