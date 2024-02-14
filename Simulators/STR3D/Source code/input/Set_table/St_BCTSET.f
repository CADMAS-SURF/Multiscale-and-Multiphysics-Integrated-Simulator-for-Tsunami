      SUBROUTINE ST_BCTSET( ICPR, NCPR, NICPR, NNCPR, J_BCTS, N_BCTS,
     &                      ICRGR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ICPR(2,NICPR), NCPR(2,NNCPR), J_BCTS(3,N_BCTS), ICRGR(*)
C
      IP = 0
C
      DO I = 1, NICPR
C
        DO J = 1, N_BCTS
C
          IF( J_BCTS(1,J) == ICPR(1,I) ) THEN
C
            IP = IP + 1
            NCPR(:,IP) = J_BCTS(2:3,J)
C
          ENDIF
C
        ENDDO
C
        ICPR(2,I) = IP
C
      ENDDO
C
      DO I = 1, NNCPR
        NCPR(:,I) = ICRGR( NCPR(:,I) )
      ENDDO
C
      END