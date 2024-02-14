      LOGICAL FUNCTION ENDT(CHAR)
C
      CHARACTER*8 CHAR
C
      ENDT = .FALSE.
C
      DO I = 1, 5
        IF( CHAR(I:I+3) == 'ENDT' ) THEN
          ENDT = .TRUE.
          RETURN
        ENDIF
      ENDDO
C
      END