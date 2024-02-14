      SUBROUTINE CT_BSURFS( N_BSRF, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION IE(2)
C
      N_BSRF = N_BSRF + 1
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,I8,24X,I8)') IE(:)
          DO I = 1, 2
            IF( IE(I) == 0 ) RETURN
            N_BSRF = N_BSRF + 1
          ENDDO
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
