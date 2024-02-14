      SUBROUTINE CT_SPCADD( N2, CHAR, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION ID(8)
C
      READ(CHAR,'(BN,16X,7I8)') ID(1:7)
C
      DO I = 1, 7
        IF( ID(I) == 0 ) RETURN
        N2 = N2 + 1
      ENDDO
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,8I8)') ID(1:8)
          DO I = 1, 8
            IF( ID(I) == 0 ) RETURN
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
