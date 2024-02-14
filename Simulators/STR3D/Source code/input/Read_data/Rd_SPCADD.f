      SUBROUTINE RD_SPCADD( ISPA, NSPA, IP, CHAR, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION ISPA(2), NSPA(*), ID(8)
C
      READ(CHAR,'(BN,8X,8I8)') ISPA(1), ID(1:7)
C
      DO I = 1, 7
        IF( ID(I) == 0 ) EXIT
        IP = IP + 1
        NSPA(IP) = ID(I)
      ENDDO
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,8I8)') ID(1:8)
          DO I = 1, 8
            IF( ID(I) == 0 ) EXIT
            IP = IP + 1
            NSPA(IP) = ID(I)
          ENDDO
        ELSE
          ISPA(2) = IP
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
