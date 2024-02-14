      SUBROUTINE RD_BCTSET( J_BCTS, IP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION ID(2), J_BCTS(3,*)
C
      READ(CHAR,'(BN,8X,3I8)') IDS, ID(:)
C
      IP = IP + 1
      J_BCTS(1,IP) = IDS
      J_BCTS(2:3,IP) = ID(:)
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,16X,2I8)') ID(:)
          IP = IP + 1
          J_BCTS(1,IP) = IDS
          J_BCTS(2:3,IP) = ID(:)
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
