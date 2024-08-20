      SUBROUTINE SF_IJK(I,J,K,IJK)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
!-----------------------------------------------------------------------
      K  =(IJK-1)/(NUMI*NUMJ)+1
      IJK=IJK-NUMI*NUMJ*(K-1)
      J  =(IJK-1)/NUMI+1
      I  =IJK-NUMI*(J-1)

      END