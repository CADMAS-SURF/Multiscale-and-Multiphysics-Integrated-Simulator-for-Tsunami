      SUBROUTINE ST_SPC( ISPC, NSPC, SPC, NISPC, NNSPC, J_SPC,
     &                   R_SPC, N_SPC, INDGR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ISPC(2,NISPC), NSPC(7,NNSPC), SPC(6,NNSPC), 
     &          J_SPC(8,N_SPC), R_SPC(N_SPC), INDGR(*)
C
      IP = 0
C
      DO I = 1, NISPC
C
        DO J = 1, N_SPC
C
          IF( J_SPC(1,J) == ISPC(1,I) ) THEN
C
            IP = IP + 1
            NSPC(:,IP) = J_SPC(2:8,J)
            SPC(:,IP) = R_SPC(J) * J_SPC(3:8,J)
C
          ENDIF
C
        ENDDO
C
        ISPC(2,I) = IP
C
      ENDDO
C
      NSPC(1,:) = INDGR( NSPC(1,:) )
C
      END