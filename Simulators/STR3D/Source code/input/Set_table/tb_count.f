      SUBROUTINE TB_COUNT( N, J_IDX, M, N_IDX, INDR )
C
      DIMENSION J_IDX(M,N_IDX), INDR(*)
C
      N = 0
C
      DO I = 1, N_IDX
        DO J = J_IDX(2,I), J_IDX(3,I)
          IF( INDR(J) > 0 ) N = N + 1
        ENDDO
      ENDDO
C
      END