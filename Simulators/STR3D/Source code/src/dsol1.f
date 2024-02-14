C***********************************************************************
      SUBROUTINE  DSOL1( D,E,ANU )
C***********************************************************************
C  MAKE D-MATRIX OF CRACKED SOLID ELEMENT
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
      A = E / ( 1.D0 - ANU*ANU )
      G = .5D0 * E / ( 1.D0 + ANU )
C
      D(:) = 0.
C
      D( 1) = E * 1.D-4
      D( 7) = A
      D( 8) = A * ANU
      D(12) = A
      D(16) = G * .25D0
      D(19) = G
      D(21) = G * .25D0
C
      END
