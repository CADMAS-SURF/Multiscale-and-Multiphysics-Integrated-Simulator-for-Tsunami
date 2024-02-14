      SUBROUTINE RD_REST( RTIM, IP, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION RTIM(*)
C
      IF( CHAR(6:8) == 'RUN' ) THEN
        READ(CHAR(10:),*) RTIM(1)
      ELSEIF( CHAR(6:7) == 'DT' ) THEN
        READ(CHAR(9:),*) RTIM(2)
      ELSEIF( CHAR(6:9) == 'TIME' ) THEN
        IP = IP + 1
        READ(CHAR(11:),*) RTIM(2+IP)
      ENDIF
C
      END