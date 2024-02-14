      LOGICAL FUNCTION INQTXT( CHAR, TEXT, N )
C
      CHARACTER*8 CHAR
      CHARACTER TEXT
C
      INQTXT = .FALSE.
C
      DO I = 1, 9 - N
        IF( CHAR(I:I+N-1) == TEXT(1:N) ) THEN
          INQTXT = .TRUE.
          RETURN
        ENDIF
      ENDDO
C
      END