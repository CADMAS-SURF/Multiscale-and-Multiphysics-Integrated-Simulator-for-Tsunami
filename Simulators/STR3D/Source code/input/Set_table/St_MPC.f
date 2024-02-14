      SUBROUTINE ST_MPC( IMPC, JMPC, NMPC, AMPC, NIMPC, NNMPC,
     &                   J_MPC, J2_MPC, R2_MPC, N_MPC, INDGR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IMPC(2,NIMPC), JMPC(*), NMPC(2,NNMPC), AMPC(NNMPC), 
     &          J_MPC(3,N_MPC), J2_MPC(2,*), R2_MPC(*), INDGR(*)
C
      IP = 0
      IP2 = 0
C
      DO I = 1, NIMPC
        DO J = 1, N_MPC
          IF( J_MPC(1,J) == IMPC(1,I) ) THEN
            IS = J_MPC(2,J)
            IE = J_MPC(3,J)
            DO K = IS, IE
              IP2 = IP2 + 1
              NMPC(1,IP2) = J2_MPC(1,K)
              NMPC(2,IP2) = J2_MPC(2,K)
              AMPC(IP2)   = R2_MPC(K)
            ENDDO
            IP = IP + 1
            JMPC(IP) = IP2
          ENDIF
        ENDDO
        IMPC(2,I) = IP
      ENDDO
C
      NMPC(1,:) = INDGR( NMPC(1,:) )
C
      END