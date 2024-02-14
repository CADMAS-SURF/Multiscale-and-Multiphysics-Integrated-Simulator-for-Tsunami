      SUBROUTINE GET_COMM( IP, IS, IE, CHAR )
C
      CHARACTER*80 CHAR
C
      DO I = 1, 80 
        IF( CHAR(I:I) == '=' ) THEN
          IP = I
          GOTO 10
        ENDIF
      ENDDO
C
      IP = 0
      RETURN
C
   10 DO I = 1, IP - 1
        IF( CHAR(I:I) /= ' ' ) THEN
          IS = I
          DO J = IS + 1, IP - 1
            IF( CHAR(J:J) == ' ' ) EXIT
          ENDDO
          IE = J - 1
          EXIT
        ENDIF
      ENDDO
C
      END