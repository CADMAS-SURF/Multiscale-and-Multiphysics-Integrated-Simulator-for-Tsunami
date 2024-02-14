      SUBROUTINE ST_PLOAD1( IPL1, NPL1, PLD1, NIPL1, NNPL1, J_PL1,
     &                      R_PL1, N_PL1, IELR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IPL1(2,NIPL1), NPL1(NNPL1), PLD1(3,NNPL1), 
     &          J_PL1(3,N_PL1), R_PL1(N_PL1), IELR(*)
C
      IP = 0
C
      DO I = 1, NIPL1
C
        DO J = 1, N_PL1
C
          IF( J_PL1(1,J) == IPL1(1,I) ) THEN
C
            IP = IP + 1
C
            NPL1(IP) = J_PL1(2,J)
            IC = J_PL1(3,J)
            PLD1(IC,IP) = R_PL1(J)
C
          ENDIF
C
        ENDDO
C
        IPL1(2,I) = IP
C
      ENDDO
C
      NPL1(:) = IELR( NPL1(:) )
C
      END