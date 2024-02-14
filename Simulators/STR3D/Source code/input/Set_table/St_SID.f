      SUBROUTINE ST_SID( ISID, J_IDX, M, N_IDX )
C
      DIMENSION ISID(2,*), J_IDX(M,N_IDX)
C
      NSID = 0
C
      DO 10 I = 1, N_IDX
C
        DO J = 1, NSID
          IF( J_IDX(1,I) == ISID(1,J) ) GOTO 10
        ENDDO
C
        NSID = NSID + 1
        ISID(1,NSID) = J_IDX(1,I)
C
   10 CONTINUE
C
      END