      SUBROUTINE RD_LOAD( LOAD, SLOD, SILD, LILD, IP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION LOAD(2), SILD(*), LILD(*), S(4), L(4)
C
      READ(CHAR,'(BN,8X,I8,F8.0,3(F8.0,I8))') 
     &  LOAD(1), SLOD, ( S(I), L(I), I = 1, 3 )
C
      DO I = 1, 3
        IF( L(I) == 0 ) EXIT
        IP = IP + 1
        SILD(IP) = S(I)
        LILD(IP) = L(I)
      ENDDO
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,4(F8.0,I8))') ( S(I), L(I), I = 1, 4 )
          DO I = 1, 4
            IF( L(I) == 0 ) EXIT
            IP = IP + 1
            SILD(IP) = S(I)
            LILD(IP) = L(I)
          ENDDO
        ELSE
          LOAD(2) = IP
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
