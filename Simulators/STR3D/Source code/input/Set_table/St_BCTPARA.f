      SUBROUTINE ST_BCTPARA( CPR, NICPR, ICPR, J_BCTP, R_BCTP, N_BCTP)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION CPR(2,NICPR), ICPR(2,NICPR), J_BCTP(N_BCTP),
     &          R_BCTP(2,N_BCTP)
C
      DO I = 1, NICPR
C
        DO J = 1, N_BCTP
C
          IF( J_BCTP(J) == ICPR(1,I) ) THEN
C
            CPR(:,I) = R_BCTP(:,J)
            EXIT
C
          ENDIF
C
        ENDDO
C
      ENDDO
C
      END