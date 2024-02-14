      SUBROUTINE TB_END( N, CHAR )
C
      LOGICAL ENDT
      CHARACTER*80 CHAR
C
      DO I = 0, 3
        DO J = 0, 1
          IS = 8 + 16 * I + 8 * J + 1
          IE = IS + 7
          IF( ENDT( CHAR(IS:IE) ) ) THEN
            N = I
            RETURN
          ENDIF
        ENDDO
      ENDDO
C
      N = 4
C
      END