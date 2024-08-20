      SUBROUTINE SF_FILL(H,GGW,GGV,GGF,DZ,POR)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GGF(6)

      FZ = DMIN1(DZ,H)

      H = H - FZ

      IF( FZ == DZ ) THEN
        GGW = 0.D0
      ELSE
        GGW = 1.D0 - FZ / DZ
      ENDIF

      GGV = 1.D0 - FZ / DZ * ( 1.D0 - POR )

      GGF(1:4) = GGV
      GGF(5) = POR
      IF( FZ == DZ ) THEN
        GGF(6) = POR
      ELSE
        GGF(6) = 1.D0
      ENDIF

      END
