      SUBROUTINE SF_IJ(I,J,IJ)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

      J  =(IJ-1)/NUMI+1
      I  =IJ-NUMI*(J-1)

      END