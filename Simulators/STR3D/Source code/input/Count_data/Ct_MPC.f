      SUBROUTINE CT_MPC( N2, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION IG(2)
C
      N2 = N2 + 2
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,16X,I8,16X,I8)') IG(:)
          DO I = 1, 2
            IF( IG(I) == 0 ) RETURN
            N2 = N2 + 1
          ENDDO
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
