      INTEGER,PARAMETER :: NTIMSZ=50000, NTBLSZ=200
      REAL(8) :: TTABLE(NTIMSZ,NTBLSZ)
      REAL(8) :: VTABLE(NTIMSZ,NTBLSZ)
      REAL(8) :: TABLE(NTBLSZ)
      COMMON /TABLER/ TTABLE,VTABLE,TABLE
