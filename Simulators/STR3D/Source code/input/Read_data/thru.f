      LOGICAL FUNCTION THRU(CHAR)
C
      CHARACTER*8 CHAR
C
      THRU = .FALSE.
C
      DO I = 1, 5
        IF( CHAR(I:I+3) == 'THRU' ) THEN
          THRU = .TRUE.
          RETURN
        ENDIF
      ENDDO
C
      END