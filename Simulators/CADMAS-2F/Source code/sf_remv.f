      SUBROUTINE SF_REMV(H,GGW,GGV,GGF,DZ)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GGF(6)

      RZ = DMIN1(DZ,H)

      H = H - RZ

      IF( RZ == DZ ) THEN
        GGW = 1.D0
      ELSE
        GGW = RZ / DZ
      ENDIF

      GGV0 = GGV

      GGV = RZ / DZ + ( 1.D0 - RZ / DZ ) * GGV

      GGF(1:4) = GGV
      IF( RZ == DZ ) THEN
        GGF(5) = 1.D0
      ELSE
        GGF(5) = GGV0
      ENDIF
      GGF(6) = 1.D0

      END
