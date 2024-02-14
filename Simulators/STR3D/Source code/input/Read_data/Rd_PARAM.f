      SUBROUTINE RD_PARAM( KK, RR, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION KK(*),RR(*)
C
      DO I = 7, 10
        IF( CHAR(I:I+1) == 'W4' ) THEN
          READ(CHAR(I+3:),*) RR(5)
          RETURN
        ELSEIF( CHAR(I:I+5) == 'LGDISP' ) THEN
          READ(CHAR(I+7:),*) LGDISP
          IF( LGDISP == 1 ) THEN
            KK(2) = 2
          ELSEIF( LGDISP == 2 ) THEN
            KK(2) = 1
          ENDIF
          RETURN
        ELSEIF( CHAR(I:I+3) == 'EPSA' ) THEN
          READ(CHAR(I+5:),*) RR(3)
          RETURN
        ELSEIF( CHAR(I:I+3) == 'EPSR' ) THEN
          READ(CHAR(I+5:),*) RR(6)
          RETURN
        ELSEIF( CHAR(I:I+3) == 'EPSC' ) THEN
          READ(CHAR(I+5:),*) RR(14)
          RETURN
        ENDIF
      ENDDO
C
      END