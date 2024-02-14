      SUBROUTINE CRH2VRTX(CRH,IVRQ)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION CRH(*)

      IF( IVRQ == 0 ) THEN
        CRH(1:3) = 1.D0
      ELSE
        CRH(1:12) = .25D0
      ENDIF

      END