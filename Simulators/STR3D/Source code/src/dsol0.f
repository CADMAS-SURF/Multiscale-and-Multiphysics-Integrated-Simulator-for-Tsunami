C***********************************************************************
      SUBROUTINE  DSOL0( D,E,ANU )
C***********************************************************************
C  MAKE INITIAL D-MATRIX OF SOLID ELEMENT
C
C  D      = D-MATRIX     1   2   3   4   5   6            --- ( O )
C                            7   8   9  10  11
C                               12  13  14  15
C                                   16  17  18
C                                       19  20
C                                           21
C  E      = YOUNG'S MODULUS                               --- ( I )
C  ANU    = POISSON RATIO                                 --- ( I )
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  D(21)
C
C
C--- MAKE D-MATRIX ---
      A = E*(1.D0-ANU) / ( (1.D0+ANU)*(1.D0-2.D0*ANU) )
      B = ANU / (1.D0-ANU)
      C = (1.D0-2.D0*ANU) / (2.D0*(1.D0-ANU))
C
      D( 1) = A
      D( 2) = A * B
      D( 3) = D( 2)
      D( 4) = 0.D0
      D( 5) = 0.D0
      D( 6) = 0.D0
      D( 7) = A
      D( 8) = D( 2)
      D( 9) = 0.D0
      D(10) = 0.D0
      D(11) = 0.D0
      D(12) = A
      D(13) = 0.D0
      D(14) = 0.D0
      D(15) = 0.D0
      D(16) = A * C
      D(17) = 0.D0
      D(18) = 0.D0
      D(19) = D(16)
      D(20) = 0.D0
      D(21) = D(16)
C
C
      RETURN
      END
