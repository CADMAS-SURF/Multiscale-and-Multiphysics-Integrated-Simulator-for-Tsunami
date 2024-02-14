      SUBROUTINE RD_MAT9( MAT, AMAT, IMATR, I_MAT, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION MAT(2), AMAT(33), IMATR(*), G(21), A(6)
C
      READ(CHAR,'(BN,8X,I8,7F8.0)') MID, G(1:7)
C
      READ(ITI,'(BN,8X,8F8.0)') G(8:15)
C
      READ(ITI,'(BN,8X,8F8.0)') G(16:21), RHO, A(1)
C
      READ(ITI,'(A80)') CHAR
C
      IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
        READ(CHAR,'(BN,8X,7F8.0)') A(2:6), TREF, GE
      ELSE
        A(2:6) = 0.
        TREF = 0.
        GE = 0.
        BACKSPACE(ITI)
      ENDIF
C
      MAT(1) = 1
C
      AMAT(3) = RHO
      AMAT(4) = GE
      AMAT(6) = TREF
      AMAT(7:27) = G(:)
      AMAT(28:33) = A(:)
C
      IMATR(MID) = I_MAT
C
      END
