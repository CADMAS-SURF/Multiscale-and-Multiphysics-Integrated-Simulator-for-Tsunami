      SUBROUTINE RD_FRIC( KK, RR, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION KK(*),RR(*)
C
      IF( CHAR(6:11) == 'MITER0' ) THEN
        READ(CHAR(13:),*) KK(110)
      ELSEIF( CHAR(6:11) == 'MITER1' ) THEN
        READ(CHAR(13:),*) KK(111)
      ELSEIF( CHAR(6:12) == 'MITERD0' ) THEN
        READ(CHAR(14:),*) KK(112)
      ELSEIF( CHAR(6:12) == 'MITERD1' ) THEN
        READ(CHAR(14:),*) KK(113)
      ELSEIF( CHAR(6:9) == 'EPS0' ) THEN
        READ(CHAR(11:),*) RR(10)
      ELSEIF( CHAR(6:9) == 'EPSD' ) THEN
        READ(CHAR(11:),*) RR(11)
      ELSEIF( CHAR(6:9) == 'EPSS' ) THEN
        READ(CHAR(11:),*) RR(12)
      ENDIF
C
      END