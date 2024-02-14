      SUBROUTINE CT_TEMPP1( N_TMP1, ITI )
C
      LOGICAL THRU
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION IE(8)
C
      N_TMP1 = N_TMP1 + 1
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
C
          IF( THRU( CHAR(17:24) ) ) THEN
            N_TMP1 = N_TMP1 + 1
            IF( THRU( CHAR(41:48) ) ) N_TMP1 = N_TMP1 + 1
          ELSE
            READ(CHAR,'(BN,8X,8I8)') IE(:)
            DO I = 1, 8
              IF( IE(I) == 0 ) EXIT
              N_TMP1 = N_TMP1 + 1
            ENDDO
          ENDIF
C
        ELSE
C
          BACKSPACE(ITI)
          RETURN
C
        ENDIF
C
      ENDDO
C
      END
