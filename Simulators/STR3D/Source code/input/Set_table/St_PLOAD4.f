      SUBROUTINE ST_PLOAD4( IPL4, NPL4, PLD4, NIPL4, NNPL4, J_PL4,
     &                      R_PL4, N_PL4, ICRDR, INDGR, IELR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IPL4(2,NIPL4), NPL4(4,NNPL4), PLD4(4,NNPL4), 
     &          J_PL4(6,N_PL4), R_PL4(4,N_PL4), IELR(*), INDGR(*),
     &          ICRDR(*)
C
      IP = 0
C
      DO I = 1, NIPL4
C
        DO J = 1, N_PL4
C
          IF( J_PL4(1,J) == IPL4(1,I) ) THEN
C
            IS = J_PL4(2,J)
            IE = J_PL4(3,J)
C
            DO K = IS, IE
              IF( IELR(K) > 0 ) THEN
                IP = IP + 1
                NPL4(1,IP) = K
                NPL4(2,IP) = J_PL4(4,J)
                NPL4(3,IP) = J_PL4(5,J)
                NPL4(4,IP) = J_PL4(6,J)
                PLD4(:,IP) = R_PL4(:,J)
              ENDIF
            ENDDO 
C
          ENDIF
C
        ENDDO
C
        IPL4(2,I) = IP
C
      ENDDO
C
      DO I = 1, NNPL4
        NPL4(1,I) = IELR( NPL4(1,I) )
        IF( NPL4(2,I) > 0 ) NPL4(2,I) = INDGR( NPL4(2,I) )
        IF( NPL4(3,I) > 0 ) NPL4(3,I) = INDGR( NPL4(3,I) )
        IF( NPL4(4,I) > 0 ) NPL4(4,I) = ICRDR( NPL4(4,I) )
      ENDDO
C
      END