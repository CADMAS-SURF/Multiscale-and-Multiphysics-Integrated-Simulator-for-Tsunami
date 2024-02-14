      SUBROUTINE RD_NLPARM( NLP, RNLP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION NLP(4), RNLP(2)
C
      READ(CHAR,'(BN,8X,2I8,24X,I8)') ID, NINC, MAXITER
C
      IF( NINC == 0 ) NINC = 10
      IF( MAXITER == 0 ) MAXITER = 25
C
      INTOUT = NINC
      DO I = 65, 72
        IF( CHAR(I:I+2) == 'YES' ) THEN
          INTOUT = 1
          EXIT
        ENDIF          
      ENDDO
C
      READ(ITI,'(A80)') CHAR
C
      IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
        READ(CHAR,'(BN,16X,F8.0)') EPSP
        IF( EPSP == 0. ) EPSP = 1.D-3
      ELSE
        BACKSPACE(ITI)
        EPSP = 1.D-3
      ENDIF
C
      NLP(1) = ID
      NLP(2) = NINC
      NLP(3) = MAXITER
      NLP(4) = INTOUT
      RNLP(1) = EPSP
C
      END