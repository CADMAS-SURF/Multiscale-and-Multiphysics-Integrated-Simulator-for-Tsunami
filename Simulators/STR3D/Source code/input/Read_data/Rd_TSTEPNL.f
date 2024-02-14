      SUBROUTINE RD_TSTEPNL( NLP, RNLP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION NLP(4), RNLP(2)
C
      READ(CHAR,'(BN,8X,2I8,F8.0,I8,16X,I8)') ID, NDT, DT, NO, MAXITER
C
      IF( NO == 0 ) NO = 1
      IF( MAXITER == 0 ) MAXITER = 10
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
      NLP(2) = NDT
      NLP(3) = MAXITER
      NLP(4) = NO
      RNLP(1) = EPSP
      RNLP(2) = DT
C
      END