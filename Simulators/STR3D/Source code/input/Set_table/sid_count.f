      SUBROUTINE SID_COUNT( NSID, J_IDX, M, N_IDX )
C
      DIMENSION J_IDX(M,N_IDX)
C
      NSID = 0
C
      DO 10 I = 1, N_IDX
C
        DO J = I - 1, 1, -1
          IF( J_IDX(1,I) == J_IDX(1,J) ) GOTO 10
        ENDDO
C
        NSID = NSID + 1
C
   10 CONTINUE
C
      END