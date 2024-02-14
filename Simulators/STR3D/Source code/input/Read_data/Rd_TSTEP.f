      SUBROUTINE RD_TSTEP( ISTP, NSTP, DELT, IP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION ISTP(*), NSTP(2,*), DELT(*)
C
      IP = IP + 1
      READ(CHAR,'(BN,8X,2I8,F8.0,I8)') 
     &  ISTP(1), NSTP(1,IP), DELT(IP), NSTP(2,IP)
      IF( NSTP(2,IP) == 0 ) NSTP(2,IP) = 1
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          IP = IP + 1
          READ(CHAR,'(BN,16X,I8,F8.0,I8)') 
     &      NSTP(1,IP), DELT(IP), NSTP(2,IP)
          IF( NSTP(2,IP) == 0 ) NSTP(2,IP) = 1
        ELSE
          ISTP(2) = IP
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
