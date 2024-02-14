      SUBROUTINE RD_RLOAD1( IRL1, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION IRL1(7)
C
      READ(CHAR,'(BN,8X,2I8,16X,3I8)') ID, IDEX, ITC, ITD
C
      ITYP= 0
      DO I = 57, 64
        IF( CHAR(I:I) == ' ' ) CYCLE
        IF( CHAR(I:I) == '0' .OR. CHAR(I:I) == 'L' ) THEN
          ITYP = 0
        ELSEIF( CHAR(I:I) == '1' .OR. CHAR(I:I) == 'D' ) THEN
          ITYP = 1
        ELSEIF( CHAR(I:I) == '2' .OR. CHAR(I:I) == 'V' ) THEN
          ITYP = 2
        ELSEIF( CHAR(I:I) == '3' .OR. CHAR(I:I) == 'A' ) THEN
          ITYP = 3
        ENDIF
        EXIT
      ENDDO
C
      IRL1(1) = ID
      IRL1(2) = IDEX
      IRL1(5) = ITC
      IRL1(6) = ITD
      IRL1(7) = ITYP
C
      END