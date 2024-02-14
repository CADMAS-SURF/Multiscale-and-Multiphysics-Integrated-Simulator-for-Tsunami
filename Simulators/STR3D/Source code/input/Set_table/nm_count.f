      SUBROUTINE NM_COUNT( NM, J_SHEL, N_SHEL, J_SOL, N_SOL )
C
      DIMENSION J_SHEL(10,N_SHEL), J_SOL(22,N_SOL)
C
      ND_MAX = 2
C
      DO I = 1, N_SHEL
        DO J = 1, 8
          IF( J_SHEL(2+J,I) == 0 ) EXIT
        ENDDO
        ND = J - 1
        IF( ND == 8 ) ND = 9
        IF( ND > ND_MAX ) ND_MAX = ND
      ENDDO
C
      DO I = 1, N_SOL
        DO J = 1, 20
          IF( J_SOL(2+J,I) == 0 ) EXIT
        ENDDO
        ND = J - 1
        IF( ND > ND_MAX ) ND_MAX = ND
      ENDDO
C
      NM = ND_MAX + 7
C
      END