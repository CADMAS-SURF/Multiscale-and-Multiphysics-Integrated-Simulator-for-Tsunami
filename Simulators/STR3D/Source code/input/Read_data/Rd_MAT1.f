      SUBROUTINE RD_MAT1( AMAT, IMATR, I_MAT, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION AMAT(*), IMATR(*)
C
      READ(CHAR,'(BN,8X,I8,F8.0,8X,5F8.0)') MID, E, RNU, RHO, A, POR,GE
C
      AMAT(1) = E
      AMAT(2) = RNU
      AMAT(4) = GE
      AMAT(5) = A
      AMAT(6) = POR
C
      AMAT(3) = ( 1.D0 - POR ) * RHO + POR * 1.D3
C
      READ(ITI,'(A80)') CHAR
C
      IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
C
        IF( MID < 100 ) THEN
C
          READ(CHAR,'(BN,8X,F8.0)') ST
C
          AMAT(14) = ST
C
        ELSE
C
          READ(CHAR,'(BN,8X,3F8.0)') POR, RKF, RK
C
          AMAT(7) = POR
          AMAT(8) = RKF
          AMAT(9) = RK / 9.8D3
          AMAT(10) = 1.D3  ! RHOF
C
          AMAT(3) = ( 1.D0 - POR ) * RHO + POR * 1.D3
C
        ENDIF
C
      ELSE
C
        BACKSPACE(ITI)
C
      ENDIF
C
      IMATR(MID) = I_MAT
C
      END
