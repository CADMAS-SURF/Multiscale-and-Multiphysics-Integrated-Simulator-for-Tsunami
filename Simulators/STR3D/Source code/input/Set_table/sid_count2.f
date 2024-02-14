      SUBROUTINE SID_COUNT2( NITMP, J_TMPD, N_TMPD, J_TMP, N_TMP )
C
      DIMENSION J_TMPD(N_TMPD), J_TMP(2,N_TMP)
C
      NITMP = N_TMPD
C
      DO 10 I = 1, N_TMP
C
        DO J = 1, N_TMPD
          IF( J_TMP(1,I) == J_TMPD(J) ) GOTO 10
        ENDDO
C
        DO J = I - 1, 1, -1
          IF( J_TMP(1,I) == J_TMP(1,J) ) GOTO 10
        ENDDO
C
        NITMP = NITMP + 1
C
   10 CONTINUE
C
      END