      SUBROUTINE ENFDISP(DUG,SPCD,F,ITYP,UG1,UG2,DT1,DT2)
C
      IMPLICIT REAL*8(A-H,O-Z)
C-----------------------------------------------------------------------
      DT12 = .5D0 * ( DT1 + DT2 )

      SELECT CASE( ITYP )
      CASE( 1 )
        DUG = SPCD * F - UG2
      CASE( 2 )
        DUG = UG1 + 2.D0 * DT12 * SPCD * F - UG2
      CASE( 3 )
        DUG = DT2 * ( DT12 * SPCD * F + ( UG2 - UG1 ) / DT1 )
      END SELECT

      END