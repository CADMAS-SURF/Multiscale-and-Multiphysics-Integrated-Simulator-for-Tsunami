      SUBROUTINE BEAM0(IST,SY,S,D,E,ANU,ST,IYLD,HD)

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

      IF( IYLD > 0 ) THEN
        IF( DABS(S(1)) >= SY ) THEN
          IST = 2
          SY = DABS(S(1))
          CALL DBEAM2(D,E,ANU,HD)
        ENDIF
      ENDIF

      END