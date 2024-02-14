      SUBROUTINE ST_MATS1( MAT, AMAT, IMATR, J_MATS1, R_MATS1, N_MATS1 )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION MAT(2,*), AMAT(33,*), IMATR(*), J_MATS1(2,N_MATS1),
     &          R_MATS1(3,N_MATS1)
C
      DO I = 1, N_MATS1
        MID = J_MATS1(1,I)
        IMAT = IMATR(MID)
        IF( IMAT > 0 ) THEN
          MAT(2,IMAT) = J_MATS1(2,I)
          AMAT(11,IMAT) = R_MATS1(2,I)
          AMAT(12,IMAT) = R_MATS1(1,I)
          AMAT(13,IMAT) = R_MATS1(3,I)
        ENDIF
      ENDDO
C
      END