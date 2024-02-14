      SUBROUTINE CT_SPC1( N_SPC1, CHAR, ITI )
C
      LOGICAL THRU
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION IG(8)
C
      IF( THRU( CHAR(33:40) ) ) THEN
        N_SPC1 = N_SPC1 + 1
        RETURN
      ELSE
        READ(CHAR,'(BN,24X,6I8)') IG(1:6)
        DO I = 1, 6
          IF( IG(I) == 0 ) RETURN
          N_SPC1 = N_SPC1 + 1
        ENDDO
      ENDIF
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,8I8)') IG(1:8)
          DO I = 1, 8
            IF( IG(I) == 0 ) RETURN
            N_SPC1 = N_SPC1 + 1
          ENDDO
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
