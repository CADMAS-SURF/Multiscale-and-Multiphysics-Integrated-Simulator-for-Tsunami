      SUBROUTINE RD_MPC( J_MPC, J2_MPC, R2_MPC, IP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION J_MPC(3), J2_MPC(2,*), R2_MPC(*), IG(2), IC(2), A(2)
C
      READ(CHAR,'(BN,8X,I8,2(2I8,F8.0))') 
     &  IDS, ( IG(I), IC(I), A(I), I = 1, 2 )
C
      J_MPC(1) = IDS
      J_MPC(2) = IP + 1
C
      DO I = 1, 2
        IP = IP + 1
        J2_MPC(1,IP) = IG(I)
        J2_MPC(2,IP) = IC(I)
        R2_MPC(IP) = A(I)
      ENDDO
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,16X,2(2I8,F8.0))') 
     &      ( IG(I), IC(I), A(I), I = 1, 2 )
          DO I = 1, 2
            IF( IG(I) == 0 ) EXIT
            IP = IP + 1
            J2_MPC(1,IP) = IG(I)
            J2_MPC(2,IP) = IC(I)
            R2_MPC(IP) = A(I)
          ENDDO
        ELSE
          J_MPC(3) = IP
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
