      SUBROUTINE ST_MATT1( MATT, NMAT, J_MATT1, N_MATT1, IMATR, ITBM1R )
C
      DIMENSION MATT(5,NMAT), J_MATT1(6,N_MATT1), IMATR(*), ITBM1R(*)
C
      DO I = 1, N_MATT1
        IMAT = IMATR( J_MATT1(1,I) )
        MATT(:,IMAT) = J_MATT1(2:6,I)
      ENDDO
C
      DO I = 1, NMAT
        DO J = 1, 5
          IT = MATT(J,I)
          IF( IT > 0 ) MATT(J,I) = ITBM1R(IT)
        ENDDO
      ENDDO
C
      END