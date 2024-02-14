      SUBROUTINE CT_LOAD( N2_LD, CHAR, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION LID(4)
C
      READ(CHAR,'(BN,24X,3(8X,I8))') LID(1:3)
C
      DO I = 1, 3
        IF( LID(I) == 0 ) RETURN
        N2_LD = N2_LD + 1
      ENDDO
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,4(8X,I8))') LID(1:4)
          DO I = 1, 4
            IF( LID(I) == 0 ) RETURN
            N2_LD = N2_LD + 1
          ENDDO
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
