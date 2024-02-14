      SUBROUTINE RD_TLOAD1( ITL1, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION ITL1(4)
C
      READ(CHAR,'(BN,8X,2I8,16X,I8)') ID, IDEX, IDT
C
      ITYP= 0
      DO I = 33, 40
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
      ITL1(1) = ID
      ITL1(2) = IDEX
      ITL1(3) = ITYP
      ITL1(4) = IDT
C
      END